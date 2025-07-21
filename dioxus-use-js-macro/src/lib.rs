#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use std::{fs, path::Path};
use swc_common::comments::{CommentKind, Comments};
use swc_common::{SourceMap, Span, comments::SingleThreadedComments};
use swc_common::{SourceMapper, Spanned};
use swc_ecma_ast::{
    Decl, ExportDecl, ExportSpecifier, FnDecl, ModuleExportName, NamedExport, Param, Pat, TsType,
    VarDeclarator,
};
use swc_ecma_parser::EsSyntax;
use swc_ecma_parser::{Parser, StringInput, Syntax, lexer::Lexer};
use swc_ecma_visit::{Visit, VisitWith};
use syn::{
    Ident, LitStr, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

const JSVALUE_JS: &str = "JsValue";
const JSVALUE_OUTPUT: &str = "dioxus_use_js::JsValue";
const JSVALUE_INPUT: &str = "&dioxus_use_js::JsValue";
const SERDE_OUTPUT: &str = "dioxus_use_js::SerdeJsonValue";
const SERDE_INPUT: &str = "impl dioxus_use_js::SerdeSerialize";

#[derive(Debug, Clone)]
enum ImportSpec {
    /// *
    All,
    /// {greeting, other_func}
    Named(Vec<Ident>),
    /// greeting
    Single(Ident),
}

struct UseJsInput {
    js_bundle_path: LitStr,
    ts_source_path: Option<LitStr>,
    import_spec: ImportSpec,
}

impl Parse for UseJsInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let first_str: LitStr = input.parse()?;

        // Check if => follows (i.e., we have "src.ts" => "bundle.js")
        let (ts_source_path, js_bundle_path) = if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            let second_str: LitStr = input.parse()?;
            (Some(first_str), second_str)
        } else {
            (None, first_str)
        };

        // Check for optional :: following bundle path
        let import_spec = if input.peek(Token![::]) {
            input.parse::<Token![::]>()?;

            if input.peek(Token![*]) {
                input.parse::<Token![*]>()?;
                ImportSpec::All
            } else if input.peek(Ident) {
                let ident: Ident = input.parse()?;
                ImportSpec::Single(ident)
            } else if input.peek(syn::token::Brace) {
                let content;
                syn::braced!(content in input);
                let idents: syn::punctuated::Punctuated<Ident, Token![,]> =
                    content.parse_terminated(Ident::parse, Token![,])?;
                ImportSpec::Named(idents.into_iter().collect())
            } else {
                return Err(input.error("Expected `*`, an identifier, or a brace group after `::`"));
            }
        } else {
            return Err(input
                .error("Expected `::` followed by an import spec (even for wildcard with `*`)"));
        };

        Ok(UseJsInput {
            js_bundle_path,
            ts_source_path,
            import_spec,
        })
    }
}

#[derive(Debug, Clone)]
struct ParamInfo {
    name: String,
    #[allow(unused)]
    js_type: Option<String>,
    rust_type: String,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    /// If specified in the use declaration
    name_ident: Option<Ident>,
    /// js param types
    params: Vec<ParamInfo>,
    // js return type
    #[allow(unused)]
    js_return_type: Option<String>,
    rust_return_type: String,
    is_exported: bool,
    is_async: bool,
    /// The stripped lines
    doc_comment: Vec<String>,
}

struct FunctionVisitor {
    functions: Vec<FunctionInfo>,
    comments: SingleThreadedComments,
    source_map: SourceMap,
}

impl FunctionVisitor {
    fn new(comments: SingleThreadedComments, source_map: SourceMap) -> Self {
        Self {
            functions: Vec::new(),
            comments,
            source_map,
        }
    }

    fn extract_doc_comment(&self, span: Span) -> Vec<String> {
        // Get leading comments for the span
        let leading_comment = self.comments.get_leading(span.lo());

        if let Some(comments) = leading_comment {
            let mut doc_lines = Vec::new();

            for comment in comments.iter() {
                let comment_text = &comment.text;
                match comment.kind {
                    // Handle `///`. `//` is already stripped
                    CommentKind::Line => {
                        if let Some(content) = comment_text.strip_prefix("/") {
                            let cleaned = content.trim_start();
                            doc_lines.push(cleaned.to_string());
                        }
                    }
                    // Handle `/*` `*/`. `/*` `*/` is already stripped
                    CommentKind::Block => {
                        for line in comment_text.lines() {
                            if let Some(cleaned) = line.trim_start().strip_prefix("*") {
                                doc_lines.push(cleaned.to_string());
                            }
                        }
                    }
                };
            }

            doc_lines
        } else {
            Vec::new()
        }
    }
}

fn ts_type_to_rust_type(ts_type: Option<&str>, is_input: bool) -> String {
    let Some(ts_type) = ts_type else {
        return (if is_input { SERDE_INPUT } else { SERDE_OUTPUT }).to_owned();
    };
    if ts_type.starts_with(JSVALUE_JS) {
        if is_input {
            return JSVALUE_INPUT.to_owned();
        } else {
            return JSVALUE_OUTPUT.to_owned();
        }
    }
    match ts_type_to_rust_type_helper(ts_type, is_input, true) {
        Some(value) => {
            if is_input && value != SERDE_INPUT {
                assert!(
                    !value.starts_with("&"),
                    "helper function should not return a reference"
                );
                format!("&{}", value)
            } else {
                value
            }
        },
        None => (if is_input { SERDE_INPUT } else { SERDE_OUTPUT }).to_owned(),
    }
}

/// Simple converter, needs null in the second position to enable Option, handles regular ts types.
/// Does not handle all edge cases
fn ts_type_to_rust_type_helper(mut ts_type: &str, is_input: bool, is_root: bool) -> Option<String> {
    ts_type = ts_type.trim();
    while ts_type.starts_with("(") && ts_type.ends_with(")") {
        ts_type = &ts_type[1..ts_type.len() - 1].trim();
    }

    let parts = split_top_level_union(ts_type);
    if parts.len() > 1 {
        // Handle single null union: T | null or null | T
        if parts.len() == 2 && parts.contains(&"null") {
            let inner = parts.iter().find(|p| **p != "null")?;
            let inner_rust = ts_type_to_rust_type_helper(inner, is_input, false)?;
            return Some(format!("Option<{}>", inner_rust));
        }
        // Unsupported union type
        return None;
    }

    ts_type = parts[0];

    if ts_type.ends_with("[]") {
        let inner = ts_type.strip_suffix("[]").unwrap();
        let inner_rust = ts_type_to_rust_type_helper(inner, is_input, false)?;
        return Some(if is_input && is_root {
            format!("[{}]", inner_rust)
        } else {
            format!("Vec<{}>", inner_rust)
        });
    }

    if ts_type.starts_with("Array<") && ts_type.ends_with(">") {
        let inner = &ts_type[6..ts_type.len() - 1];
        let inner_rust = ts_type_to_rust_type_helper(inner, is_input, false)?;
        return Some(if is_input && is_root {
            format!("[{}]", inner_rust)
        } else {
            format!("Vec<{}>", inner_rust)
        });
    }

    if ts_type.starts_with("Set<") && ts_type.ends_with(">") {
        let inner = &ts_type[4..ts_type.len() - 1];
        let inner_rust = ts_type_to_rust_type_helper(inner, is_input, false)?;
        return Some(format!("std::collections::HashSet<{}>", inner_rust));
    }

    if ts_type.starts_with("Map<") && ts_type.ends_with(">") {
        let inner = &ts_type[4..ts_type.len() - 1];
        let mut depth = 0;
        let mut split_index = None;
        for (i, c) in inner.char_indices() {
            match c {
                '<' => depth += 1,
                '>' => depth -= 1,
                ',' if depth == 0 => {
                    split_index = Some(i);
                    break;
                }
                _ => {}
            }
        }

        if let Some(i) = split_index {
            let (key, value) = inner.split_at(i);
            let value = &value[1..]; // skip comma
            let key_rust = ts_type_to_rust_type_helper(key.trim(), is_input, false)?;
            let value_rust = ts_type_to_rust_type_helper(value.trim(), is_input, false)?;
            return Some(format!(
                "std::collections::HashMap<{}, {}>",
                key_rust, value_rust
            ));
        } else {
            return None;
        }
    }

    // Base types
    let rust_type = match ts_type {
        "string" => {
            if is_input && is_root {
                "str"
            } else {
                "String"
            }
        }
        "number" => "f64",
        "boolean" => "bool",
        "any" | "unknown" | "object" | _ => {
            if is_input {
                SERDE_INPUT
            } else {
                SERDE_OUTPUT
            }
        }
    };

    Some(rust_type.to_owned())
}

/// Splits e.g. `number | null | string` ignoring nesting like `(number | null)[]`
fn split_top_level_union(s: &str) -> Vec<&str> {
    let mut parts = vec![];
    let mut last = 0;
    let mut depth_angle = 0;
    let mut depth_paren = 0;

    for (i, c) in s.char_indices() {
        match c {
            '<' => depth_angle += 1,
            '>' => {
                if depth_angle > 0 {
                    depth_angle -= 1
                }
            }
            '(' => depth_paren += 1,
            ')' => {
                if depth_paren > 0 {
                    depth_paren -= 1
                }
            }
            '|' if depth_angle == 0 && depth_paren == 0 => {
                parts.push(s[last..i].trim());
                last = i + 1;
            }
            _ => {}
        }
    }

    if last < s.len() {
        parts.push(s[last..].trim());
    }

    parts
}

fn type_to_string(ty: &Box<TsType>, source_map: &SourceMap) -> String {
    let span = ty.span();
    source_map
        .span_to_snippet(span)
        .expect("Could not get snippet from span for type")
}

fn function_params_to_param_info(params: &[Param], source_map: &SourceMap) -> Vec<ParamInfo> {
    params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let name = if let Some(ident) = param.pat.as_ident() {
                ident.id.sym.to_string()
            } else {
                format!("arg{}", i)
            };

            let js_type = param
                .pat
                .as_ident()
                .and_then(|ident| ident.type_ann.as_ref())
                .map(|type_ann| {
                    let ty = &type_ann.type_ann;
                    type_to_string(ty, source_map)
                });
            let rust_type = ts_type_to_rust_type(js_type.as_deref(), true);

            ParamInfo {
                name,
                js_type,
                rust_type,
            }
        })
        .collect()
}

fn function_pat_to_param_info(pats: &[Pat], source_map: &SourceMap) -> Vec<ParamInfo> {
    pats.iter()
        .enumerate()
        .map(|(i, pat)| {
            let name = if let Some(ident) = pat.as_ident() {
                ident.id.sym.to_string()
            } else {
                format!("arg{}", i)
            };

            let js_type = pat
                .as_ident()
                .and_then(|ident| ident.type_ann.as_ref())
                .map(|type_ann| {
                    let ty = &type_ann.type_ann;
                    type_to_string(ty, source_map)
                });
            let rust_type = ts_type_to_rust_type(js_type.as_deref(), true);

            ParamInfo {
                name,
                js_type,
                rust_type,
            }
        })
        .collect()
}

impl Visit for FunctionVisitor {
    /// Visit function declarations: function foo() {}
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        let doc_comment = self.extract_doc_comment(node.span());

        let params = function_params_to_param_info(&node.function.params, &self.source_map);

        let js_return_type = node.function.return_type.as_ref().map(|type_ann| {
            let ty = &type_ann.type_ann;
            type_to_string(ty, &self.source_map)
        });
        let rust_return_type = ts_type_to_rust_type(js_return_type.as_deref(), false);

        self.functions.push(FunctionInfo {
            name: node.ident.sym.to_string(),
            name_ident: None,
            params,
            js_return_type,
            rust_return_type,
            is_exported: false,
            is_async: node.function.is_async,
            doc_comment,
        });
        node.visit_children_with(self);
    }

    /// Visit function expressions: const foo = function() {}
    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        if let swc_ecma_ast::Pat::Ident(ident) = &node.name {
            if let Some(init) = &node.init {
                let doc_comment = self.extract_doc_comment(node.span());

                match &**init {
                    swc_ecma_ast::Expr::Fn(fn_expr) => {
                        let params = function_params_to_param_info(
                            &fn_expr.function.params,
                            &self.source_map,
                        );

                        let js_return_type =
                            fn_expr.function.return_type.as_ref().map(|type_ann| {
                                let ty = &type_ann.type_ann;
                                type_to_string(ty, &self.source_map)
                            });
                        let rust_return_type =
                            ts_type_to_rust_type(js_return_type.as_deref(), false);

                        self.functions.push(FunctionInfo {
                            name: ident.id.sym.to_string(),
                            name_ident: None,
                            params,
                            js_return_type,
                            rust_return_type,
                            is_exported: false,
                            is_async: fn_expr.function.is_async,
                            doc_comment,
                        });
                    }
                    swc_ecma_ast::Expr::Arrow(arrow_fn) => {
                        let params = function_pat_to_param_info(&arrow_fn.params, &self.source_map);

                        let js_return_type = arrow_fn.return_type.as_ref().map(|type_ann| {
                            let ty = &type_ann.type_ann;
                            type_to_string(ty, &self.source_map)
                        });
                        let rust_return_type =
                            ts_type_to_rust_type(js_return_type.as_deref(), false);

                        self.functions.push(FunctionInfo {
                            name: ident.id.sym.to_string(),
                            name_ident: None,
                            params,
                            js_return_type,
                            rust_return_type,
                            is_exported: false,
                            is_async: arrow_fn.is_async,
                            doc_comment,
                        });
                    }
                    _ => {}
                }
            }
        }
        node.visit_children_with(self);
    }

    /// Visit export declarations: export function foo() {}
    fn visit_export_decl(&mut self, node: &ExportDecl) {
        if let Decl::Fn(fn_decl) = &node.decl {
            let doc_comment = self.extract_doc_comment(node.span());

            let params = function_params_to_param_info(&fn_decl.function.params, &self.source_map);

            let js_return_type = fn_decl.function.return_type.as_ref().map(|type_ann| {
                let ty = &type_ann.type_ann;
                type_to_string(ty, &self.source_map)
            });
            let rust_return_type = ts_type_to_rust_type(js_return_type.as_deref(), false);

            self.functions.push(FunctionInfo {
                name: fn_decl.ident.sym.to_string(),
                name_ident: None,
                params,
                js_return_type,
                rust_return_type,
                is_exported: true,
                is_async: fn_decl.function.is_async,
                doc_comment,
            });
        }
        node.visit_children_with(self);
    }

    /// Visit named exports: export { foo }
    fn visit_named_export(&mut self, node: &NamedExport) {
        for spec in &node.specifiers {
            if let ExportSpecifier::Named(named) = spec {
                let name = match &named.orig {
                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                    ModuleExportName::Str(str_lit) => str_lit.value.to_string(),
                };

                if let Some(func) = self.functions.iter_mut().find(|f| f.name == name) {
                    func.is_exported = true;
                }
            }
        }
        node.visit_children_with(self);
    }
}

fn parse_script_file(file_path: &Path, is_js: bool) -> Result<Vec<FunctionInfo>> {
    let js_content = fs::read_to_string(file_path).map_err(|e| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            format!("Could not read file '{}': {}", file_path.display(), e),
        )
    })?;

    let source_map = SourceMap::default();
    let fm = source_map.new_source_file(
        swc_common::FileName::Custom(file_path.display().to_string()).into(),
        js_content.clone(),
    );
    let comments = SingleThreadedComments::default();

    // Enable TypeScript parsing to handle type annotations
    let syntax = if is_js {
        Syntax::Es(EsSyntax {
            jsx: false,
            fn_bind: false,
            decorators: false,
            decorators_before_export: false,
            export_default_from: false,
            import_attributes: false,
            allow_super_outside_method: false,
            allow_return_outside_function: false,
            auto_accessors: false,
            explicit_resource_management: false,
        })
    } else {
        Syntax::Typescript(swc_ecma_parser::TsSyntax {
            tsx: false,
            decorators: false,
            dts: false,
            no_early_errors: false,
            disallow_ambiguous_jsx_like: true,
        })
    };

    let lexer = Lexer::new(
        syntax,
        Default::default(),
        StringInput::from(&*fm),
        Some(&comments),
    );

    let mut parser = Parser::new_from(lexer);

    let module = parser.parse_module().map_err(|e| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "Failed to parse script file '{}': {:?}",
                file_path.display(),
                e
            ),
        )
    })?;

    let mut visitor = FunctionVisitor::new(comments, source_map);
    module.visit_with(&mut visitor);

    // Functions are added twice for some reason
    visitor
        .functions
        .dedup_by(|e1, e2| e1.name.as_str() == e2.name.as_str());
    Ok(visitor.functions)
}

fn remove_valid_function_info(
    name: &str,
    functions: &mut Vec<FunctionInfo>,
    file: &Path,
) -> Result<FunctionInfo> {
    let function_info = if let Some(pos) = functions.iter().position(|f| f.name == name) {
        functions.remove(pos)
    } else {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!("Function '{}' not found in file '{}'", name, file.display()),
        ));
    };
    if !function_info.is_exported {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "Function '{}' not exported in file '{}'",
                name,
                file.display()
            ),
        ));
    }
    Ok(function_info)
}

fn get_functions_to_generate(
    mut functions: Vec<FunctionInfo>,
    import_spec: &ImportSpec,
    file: &Path,
) -> Result<Vec<FunctionInfo>> {
    match import_spec {
        ImportSpec::All => Ok(functions),
        ImportSpec::Single(name) => {
            let mut func =
                remove_valid_function_info(name.to_string().as_str(), &mut functions, file)?;
            func.name_ident.replace(name.clone());
            Ok(vec![func])
        }
        ImportSpec::Named(names) => {
            let mut result = Vec::new();
            for name in names {
                let mut func =
                    remove_valid_function_info(name.to_string().as_str(), &mut functions, file)?;
                func.name_ident.replace(name.clone());
                result.push(func);
            }
            Ok(result)
        }
    }
}

fn generate_function_wrapper(func: &FunctionInfo, asset_path: &LitStr) -> TokenStream2 {
    let send_calls: Vec<TokenStream2> = func
        .params
        .iter()
        .map(|param| {
            let param_name = format_ident!("{}", param.name);
            if param.rust_type == JSVALUE_INPUT {
                quote! {
                    #[allow(deprecated)]
                    eval.send(#param_name.internal_get()).map_err(dioxus_use_js::JsError::Eval)?;
                }
            } else {
                quote! {
                    eval.send(#param_name).map_err(dioxus_use_js::JsError::Eval)?;
                }
            }
        })
        .collect();

    let js_func_name = &func.name;
    let params_list = func
        .params
        .iter()
        .map(|p| p.name.as_str())
        .collect::<Vec<&str>>()
        .join(", ");
    let recv_lines = func
        .params
        .iter()
        .map(|param| {
            if param.rust_type == JSVALUE_INPUT {
                format!(
                    "let {}Temp_ = await dioxus.recv();\nlet {} = window[{}Temp_];",
                    param.name, param.name, param.name
                )
            } else {
                format!("let {} = await dioxus.recv();", param.name)
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    let mut await_fn = String::new();
    if func.is_async {
        await_fn.push_str("await");
    }
    let end_statement = if func.rust_return_type == JSVALUE_OUTPUT {
        format!(
            r#"
const ___result___ = {await_fn} {js_func_name}({params_list});
const ___id___ = crypto.randomUUID();
window[___id___] = ___result___;
return ___id___;
        "#
        )
    } else {
        // eval will fail if returning undefined. undefined happens if there is no return type
        format!(
            r#"
const ___result___ = {await_fn} {js_func_name}({params_list});
if (___result___ === undefined) {{{{ return null; }}}}
return ___result___;
"#
        )
    };

    let js_format = format!(
        r#"
const {{{{ {js_func_name} }}}} = await import("{{}}");
{recv_lines}
{end_statement}
"#
    );

    // Generate parameter types with extracted type information
    let param_types: Vec<_> = func
        .params
        .iter()
        .map(|param| {
            let param_name = format_ident!("{}", param.name);
            let type_tokens = param
                .rust_type
                .parse::<TokenStream2>()
                .expect("Calculated Rust type should always be valid");
            quote! { #param_name: #type_tokens }
        })
        .collect();

    let parsed_type = func
        .rust_return_type
        .parse::<TokenStream2>()
        .expect("Calculated Rust type should always be valid");
    let return_type_tokens = quote! { Result<#parsed_type, dioxus_use_js::JsError> };

    // Generate documentation comment if available - preserve original JSDoc format
    let doc_comment = if func.doc_comment.is_empty() {
        quote! {}
    } else {
        let doc_lines: Vec<_> = func
            .doc_comment
            .iter()
            .map(|line| quote! { #[doc = #line] })
            .collect();
        quote! { #(#doc_lines)* }
    };

    let func_name = func
        .name_ident
        .clone()
        // Can not exist if `::*`
        .unwrap_or_else(|| Ident::new(func.name.as_str(), proc_macro2::Span::call_site()));

    let return_value_mapping = if func.rust_return_type == SERDE_OUTPUT {
        quote! {
            .map_err(dioxus_use_js::JsError::Eval)
        }
    } else {
        quote! {
            .map_err(dioxus_use_js::JsError::Eval)
            .and_then(|v| dioxus_use_js::serde_json_from_value(v).map_err(dioxus_use_js::JsError::Deserialize))
        }
    };

    let return_value = if func.rust_return_type == JSVALUE_OUTPUT {
        quote! {
        let id: String = eval
            .await
            #return_value_mapping?;
            #[allow(deprecated)]
            Ok(dioxus_use_js::JsValue::internal_create(id))
        }
    } else {
        quote! {
            eval
                .await
                #return_value_mapping
        }
    };

    quote! {
        #doc_comment
        #[allow(non_snake_case)]
        pub async fn #func_name(#(#param_types),*) -> #return_type_tokens {
            const MODULE: Asset = asset!(#asset_path);
            let js = format!(#js_format, MODULE);
            let eval = dioxus::document::eval(js.as_str());
            #(#send_calls)*
            #return_value
        }
    }
}

/// A macro to create rust binding to javascript and typescript functions.
///```rust,no_run
/// use dioxus::prelude::*;
/// use dioxus_use_js::use_js;
///
/// // Generate the greeting function at compile time
/// use_js!("example/assets/example.js"::greeting);
///
///  // Or generate multiple functions:
///  // use_js!("example/assets/example.js"::{greeting, add});
///
///  // Or generate all exported functions:
///  // use_js!("example/assets/example.js"::*);
///
/// fn main() {
///     launch(App);
/// }
///
/// #[component]
/// fn App() -> Element {
///     let future = use_resource(|| async move {
///         let from = "dave";
///         let to = "john";
///
///         // Now we can call the generated function directly!
///         let greeting_result = greeting(from, to)
///             .await
///             .map_err(Box::<dyn std::error::Error>::from)?;
///         let greeting: String =
///             serde_json::from_value(greeting_result).map_err(Box::<dyn std::error::Error>::from)?;
///         Ok::<String, Box<dyn std::error::Error>>(greeting)
///     });
///
///     rsx!(
///         div {
///             h1 { "Dioxus `use_js!` macro example!" }
///             {
///                 match &*future.read() {
///                     Some(Ok(greeting)) => rsx! {
///                         p { "Greeting from JavaScript: {greeting}" }
///                     },
///                     Some(Err(e)) => rsx! {
///                         p { "Error: {e}" }
///                     },
///                     None => rsx! {
///                         p { "Running js..." }
///                     },
///                 }
///             }
///         }
///     )
/// }
/// ```
#[proc_macro]
pub fn use_js(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as UseJsInput);

    let manifest_dir = match std::env::var("CARGO_MANIFEST_DIR") {
        Ok(dir) => dir,
        Err(_) => {
            return TokenStream::from(
                syn::Error::new(
                    proc_macro2::Span::call_site(),
                    "CARGO_MANIFEST_DIR environment variable not found",
                )
                .to_compile_error(),
            );
        }
    };

    let UseJsInput {
        js_bundle_path,
        ts_source_path,
        import_spec,
    } = input;

    let js_file_path = std::path::Path::new(&manifest_dir).join(js_bundle_path.value());

    let js_all_functions = match parse_script_file(&js_file_path, true) {
        Ok(funcs) => funcs,
        Err(e) => return TokenStream::from(e.to_compile_error()),
    };

    let js_functions_to_generate =
        match get_functions_to_generate(js_all_functions, &import_spec, &js_file_path) {
            Ok(funcs) => funcs,
            Err(e) => return TokenStream::from(e.to_compile_error()),
        };

    let functions_to_generate = if let Some(ts_file_path) = ts_source_path {
        let ts_file_path = std::path::Path::new(&manifest_dir).join(ts_file_path.value());
        let ts_all_functions = match parse_script_file(&ts_file_path, false) {
            Ok(funcs) => funcs,
            Err(e) => return TokenStream::from(e.to_compile_error()),
        };

        let ts_functions_to_generate =
            match get_functions_to_generate(ts_all_functions, &import_spec, &ts_file_path) {
                Ok(funcs) => funcs,
                Err(e) => {
                    return TokenStream::from(e.to_compile_error());
                }
            };

        for ts_func in ts_functions_to_generate.iter() {
            if let Some(js_func) = js_functions_to_generate
                .iter()
                .find(|f| f.name == ts_func.name)
            {
                if ts_func.params.len() != js_func.params.len() {
                    return TokenStream::from(syn::Error::new(
                        proc_macro2::Span::call_site(),
                        format!(
                            "Function '{}' has different parameter count in JS and TS files. Bundle may be out of date",
                            ts_func.name
                        ),
                    )
                    .to_compile_error());
                }
            } else {
                return TokenStream::from(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    format!(
                        "Function '{}' is defined in TS file but not in JS file. Bundle may be out of date",
                        ts_func.name
                    ),
                )
                .to_compile_error());
            }
        }
        ts_functions_to_generate
    } else {
        js_functions_to_generate
    };

    let function_wrappers: Vec<TokenStream2> = functions_to_generate
        .iter()
        .map(|func| generate_function_wrapper(func, &js_bundle_path))
        .collect();

    let expanded = quote! {
        #(#function_wrappers)*
    };

    TokenStream::from(expanded)
}

//************************************************************************//

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives() {
        assert_eq!(ts_type_to_rust_type(Some("string"), false), "String");
        assert_eq!(ts_type_to_rust_type(Some("string"), true), "&str");
        assert_eq!(ts_type_to_rust_type(Some("number"), false), "f64");
        assert_eq!(ts_type_to_rust_type(Some("number"), true), "&f64");
        assert_eq!(ts_type_to_rust_type(Some("boolean"), false), "bool");
        assert_eq!(ts_type_to_rust_type(Some("boolean"), true), "&bool");
        assert_eq!(
            ts_type_to_rust_type(Some("object"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("object"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
    }

    #[test]
    fn test_nullable_primitives() {
        assert_eq!(
            ts_type_to_rust_type(Some("string | null"), true),
            "&Option<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | null"), false),
            "Option<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("number | null"), true),
            "&Option<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("number | null"), false),
            "Option<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("boolean | null"), true),
            "&Option<bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("boolean | null"), false),
            "Option<bool>"
        );
    }

    #[test]
    fn test_arrays() {
        assert_eq!(ts_type_to_rust_type(Some("string[]"), true), "&[String]");
        assert_eq!(ts_type_to_rust_type(Some("string[]"), false), "Vec<String>");
        assert_eq!(ts_type_to_rust_type(Some("Array<number>"), true), "&[f64]");
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number>"), false),
            "Vec<f64>"
        );
    }

    #[test]
    fn test_nullable_array_elements() {
        assert_eq!(
            ts_type_to_rust_type(Some("(string | null)[]"), true),
            "&[Option<String>]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("(string | null)[]"), false),
            "Vec<Option<String>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number | null>"), true),
            "&[Option<f64>]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number | null>"), false),
            "Vec<Option<f64>>"
        );
    }

    #[test]
    fn test_nullable_array_itself() {
        assert_eq!(
            ts_type_to_rust_type(Some("string[] | null"), true),
            "&Option<Vec<String>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string[] | null"), false),
            "Option<Vec<String>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number> | null"), true),
            "&Option<Vec<f64>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number> | null"), false),
            "Option<Vec<f64>>"
        );
    }

    #[test]
    fn test_nullable_array_and_elements() {
        assert_eq!(
            ts_type_to_rust_type(Some("Array<string | null> | null"), true),
            "&Option<Vec<Option<String>>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<string | null> | null"), false),
            "Option<Vec<Option<String>>>"
        );
    }

    #[test]
    fn test_fallback_for_union() {
        assert_eq!(
            ts_type_to_rust_type(Some("string | number"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | number"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | number | null"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | number | null"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
    }

    #[test]
    fn test_unknown_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("foo"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("foo"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("any"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("any"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("object"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("object"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("unknown"), true),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("unknown"), false),
            "dioxus_use_js::SerdeJsonValue"
        );
    }

    #[test]
    fn test_extra_whitespace() {
        assert_eq!(
            ts_type_to_rust_type(Some("  string | null  "), true),
            "&Option<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("  string | null  "), false),
            "Option<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some(" Array< string > "), true),
            "&[String]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some(" Array< string > "), false),
            "Vec<String>"
        );
    }

    #[test]
    fn test_map_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, number>"), true),
            "&std::collections::HashMap<String, f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, number>"), false),
            "std::collections::HashMap<String, f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, boolean>"), true),
            "&std::collections::HashMap<String, bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, boolean>"), false),
            "std::collections::HashMap<String, bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<number, string>"), true),
            "&std::collections::HashMap<f64, String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<number, string>"), false),
            "std::collections::HashMap<f64, String>"
        );
    }

    #[test]
    fn test_set_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("Set<string>"), true),
            "&std::collections::HashSet<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<string>"), false),
            "std::collections::HashSet<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<number>"), true),
            "&std::collections::HashSet<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<number>"), false),
            "std::collections::HashSet<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<boolean>"), true),
            "&std::collections::HashSet<bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<boolean>"), false),
            "std::collections::HashSet<bool>"
        );
    }
}
