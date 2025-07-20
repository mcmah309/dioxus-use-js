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
const JSVALUE_RUST: &str = "dioxus_use_js::JsValue";

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

/// Reusable parsing for `ImportSpec`
fn parse_import_spec(input: ParseStream) -> Result<ImportSpec> {
    if input.peek(Token![*]) {
        input.parse::<Token![*]>()?;
        Ok(ImportSpec::All)
    } else if input.peek(syn::token::Brace) {
        let content;
        syn::braced!(content in input);
        let mut functions = Vec::new();
        while !content.is_empty() {
            let ident: Ident = content.parse()?;
            functions.push(ident);
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }
        Ok(ImportSpec::Named(functions))
    } else {
        let ident: Ident = input.parse()?;
        Ok(ImportSpec::Single(ident))
    }
}

impl Parse for UseJsInput {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitStr) {
            // shorthand: "bundle_path"::import_spec
            let js_bundle_path: LitStr = input.parse()?;
            input.parse::<Token![::]>()?;
            let import_spec = parse_import_spec(input)?;

            Ok(UseJsInput {
                js_bundle_path,
                ts_source_path: None,
                import_spec,
            })
        } else if input.peek(Ident) {
            // full syntax: ts: "...", bundle: "...", functions: ...
            let mut ts_source_path = None;
            let mut js_bundle_path = None;
            let mut import_spec = None;

            while !input.is_empty() {
                let key: Ident = input.parse()?;
                input.parse::<Token![:]>()?;

                match key.to_string().as_str() {
                    "ts" => {
                        ts_source_path = Some(input.parse::<LitStr>()?);
                    }
                    "bundle" => {
                        js_bundle_path = Some(input.parse::<LitStr>()?);
                    }
                    "functions" => {
                        import_spec = Some(parse_import_spec(input)?);
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            key,
                            "Expected one of: ts, bundle, functions",
                        ));
                    }
                }

                if input.peek(Token![,]) {
                    input.parse::<Token![,]>()?;
                }
            }

            Ok(UseJsInput {
                js_bundle_path: js_bundle_path.ok_or_else(|| {
                    syn::Error::new(proc_macro2::Span::call_site(), "`bundle` is required")
                })?,
                ts_source_path: Some(ts_source_path.ok_or_else(|| {
                    syn::Error::new(proc_macro2::Span::call_site(), "`ts` is required")
                })?),
                import_spec: import_spec.ok_or_else(|| {
                    syn::Error::new(proc_macro2::Span::call_site(), "`functions` is required")
                })?,
            })
        } else {
            Err(syn::Error::new(
                input.span(),
                "Expected string literal or braced block",
            ))
        }
    }
}

#[derive(Debug, Clone)]
struct ParamInfo {
    name: String,
    rust_type: Option<String>,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    /// If specified in the use declaration
    name_ident: Option<Ident>,
    params: Vec<ParamInfo>,
    return_type: Option<String>,
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

fn ts_type_to_rust_type(ts_type: &str) -> Option<String> {
    if ts_type == JSVALUE_JS {
        return Some(JSVALUE_RUST.to_string());
    }
    ts_type_to_rust_type_helper(ts_type)
}

/// Simple converter, needs null in the second position to enable Option, handles regular ts types.
/// Does not handle all edge cases
fn ts_type_to_rust_type_helper(mut ts_type: &str) -> Option<String> {
    ts_type = ts_type.trim();
    while ts_type.starts_with("(") && ts_type.ends_with(")") {
        ts_type = &ts_type[1..ts_type.len() - 1].trim();
    }

    if ts_type.ends_with("null") {
        ts_type = ts_type.strip_suffix("null").unwrap();
        ts_type = ts_type.trim_end();
        if ts_type.ends_with("|") {
            ts_type = ts_type.strip_suffix('|').unwrap();
            ts_type = ts_type.trim_end();
        }
        let inner = ts_type_to_rust_type_helper(ts_type)?;
        return Some(format!("Option<{}>", inner));
    }

    if ts_type.ends_with("[]") {
        ts_type = ts_type.strip_suffix("[]").unwrap();
        let inner = ts_type_to_rust_type_helper(ts_type)?;
        return Some(format!("Vec<{}>", inner));
    }

    if ts_type.starts_with("Array<") && ts_type.ends_with(">") {
        let inner = &ts_type[6..ts_type.len() - 1];
        let inner_rust = ts_type_to_rust_type_helper(inner)?;
        return Some(format!("Vec<{}>", inner_rust));
    }

    if ts_type.starts_with("Set<") && ts_type.ends_with(">") {
        let inner = &ts_type[4..ts_type.len() - 1];
        let inner_rust = ts_type_to_rust_type_helper(inner)?;
        return Some(format!("HashSet<{}>", inner_rust));
    }

    if ts_type.starts_with("Map<") && ts_type.ends_with(">") {
        let inner = &ts_type[4..ts_type.len() - 1];
        let params: Vec<&str> = inner.split(',').collect();
        if params.len() != 2 {
            return None;
        }
        let key_type = ts_type_to_rust_type_helper(params[0])?;
        let value_type = ts_type_to_rust_type_helper(params[1])?;
        return Some(format!("HashMap<{}, {}>", key_type, value_type));
    }

    let rust_type = match ts_type {
        "string" => "String",
        "number" => "f64",
        "boolean" => "bool",
        "any" => "serde_json::Value",
        "unknown" => "serde_json::Value",
        "object" => "serde_json::Value",
        _ => {
            return None;
        }
    };

    Some(rust_type.to_owned())
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

            let rust_type = param
                .pat
                .as_ident()
                .and_then(|ident| ident.type_ann.as_ref())
                .and_then(|type_ann| {
                    let ty = &type_ann.type_ann;
                    ts_type_to_rust_type(&type_to_string(ty, source_map))
                });

            ParamInfo { name, rust_type }
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

            let rust_type = pat
                .as_ident()
                .and_then(|ident| ident.type_ann.as_ref())
                .and_then(|type_ann| {
                    let ty = &type_ann.type_ann;
                    ts_type_to_rust_type(&type_to_string(ty, source_map))
                });

            ParamInfo { name, rust_type }
        })
        .collect()
}

impl Visit for FunctionVisitor {
    /// Visit function declarations: function foo() {}
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        let doc_comment = self.extract_doc_comment(node.span());

        let params = function_params_to_param_info(&node.function.params, &self.source_map);

        let return_type = node.function.return_type.as_ref().and_then(|type_ann| {
            let ty = &type_ann.type_ann;
            ts_type_to_rust_type(&type_to_string(ty, &self.source_map))
        });

        self.functions.push(FunctionInfo {
            name: node.ident.sym.to_string(),
            name_ident: None,
            params,
            return_type,
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

                        let return_type =
                            fn_expr.function.return_type.as_ref().and_then(|type_ann| {
                                let ty = &type_ann.type_ann;
                                ts_type_to_rust_type(&type_to_string(ty, &self.source_map))
                            });

                        self.functions.push(FunctionInfo {
                            name: ident.id.sym.to_string(),
                            name_ident: None,
                            params,
                            return_type,
                            is_exported: false,
                            is_async: fn_expr.function.is_async,
                            doc_comment,
                        });
                    }
                    swc_ecma_ast::Expr::Arrow(arrow_fn) => {
                        let params = function_pat_to_param_info(&arrow_fn.params, &self.source_map);

                        let return_type = arrow_fn.return_type.as_ref().and_then(|type_ann| {
                            let ty = &type_ann.type_ann;
                            ts_type_to_rust_type(&type_to_string(ty, &self.source_map))
                        });

                        self.functions.push(FunctionInfo {
                            name: ident.id.sym.to_string(),
                            name_ident: None,
                            params,
                            return_type,
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

            let return_type = fn_decl.function.return_type.as_ref().and_then(|type_ann| {
                let ty = &type_ann.type_ann;
                ts_type_to_rust_type(&type_to_string(ty, &self.source_map))
            });

            self.functions.push(FunctionInfo {
                name: fn_decl.ident.sym.to_string(),
                name_ident: None,
                params,
                return_type,
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
            if param
                .rust_type
                .as_ref()
                .is_some_and(|v| v.as_str() == JSVALUE_RUST)
            {
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
            if param
                .rust_type
                .as_ref()
                .is_some_and(|v| v.as_str() == JSVALUE_RUST)
            {
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

    let end_statement = if func
        .return_type
        .as_ref()
        .is_some_and(|v| v.as_str() == JSVALUE_RUST)
    {
        let mut await_fn = String::new();
        if func.is_async {
            await_fn.push_str("await");
        }
        format!(
            r#"
const ___result___ = {await_fn} {js_func_name}({params_list});
const ___id___ = crypto.randomUUID();
window[___id___] = ___result___;
return ___id___;
        "#
        )
    } else {
        format!("return {js_func_name}({params_list});")
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
            if let Some(rust_type) = &param.rust_type {
                // Try to parse the type, but fall back to impl serde::Serialize if parsing fails
                if let Ok(type_tokens) = rust_type.parse::<TokenStream2>() {
                    quote! { #param_name: &#type_tokens }
                } else if rust_type.as_str() == JSVALUE_RUST {
                    let js_value_type = format_ident!("{}", JSVALUE_RUST);
                    quote! { #param_name: &#js_value_type }
                } else {
                    quote! { #param_name: impl serde::Serialize }
                }
            } else {
                // Default case: no type information available
                quote! { #param_name: impl serde::Serialize }
            }
        })
        .collect();

    // Generate return type
    let return_type_tokens = if let Some(return_type) = &func.return_type {
        // Try to parse the return type, but fall back to serde_json::Value if parsing fails
        if let Ok(parsed_type) = return_type.parse::<TokenStream2>() {
            quote! { Result<#parsed_type, dioxus_use_js::JsError> }
        } else {
            quote! { Result<serde_json::Value, dioxus_use_js::JsError> }
        }
    } else {
        // Default case: no return type information available
        quote! { Result<serde_json::Value, dioxus_use_js::JsError> }
    };

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

    let return_value = if func
        .return_type
        .as_ref()
        .is_some_and(|v| v.as_str() == JSVALUE_RUST)
    {
        quote! {
        let id: String = eval
            .await
            .map_err(dioxus_use_js::JsError::Eval)
            .and_then(|v| serde_json::from_value(v).map_err(dioxus_use_js::JsError::Deserialize))?;
            #[allow(deprecated)]
            Ok(dioxus_use_js::JsValue::internal_create(id))
        }
    } else {
        quote! {
            eval
                .await
                .map_err(dioxus_use_js::JsError::Eval)
                .and_then(|v| serde_json::from_value(v).map_err(dioxus_use_js::JsError::Deserialize))
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
        assert_eq!(ts_type_to_rust_type("string"), Some("String".to_owned()));
        assert_eq!(ts_type_to_rust_type("number"), Some("f64".to_owned()));
        assert_eq!(ts_type_to_rust_type("boolean"), Some("bool".to_owned()));
        assert_eq!(
            ts_type_to_rust_type("object"),
            Some("serde_json::Value".to_owned())
        );
    }

    #[test]
    fn test_nullable_primitives() {
        assert_eq!(
            ts_type_to_rust_type("string | null"),
            Some("Option<String>".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("number | null"),
            Some("Option<f64>".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("boolean | null"),
            Some("Option<bool>".to_owned())
        );
    }

    #[test]
    fn test_arrays() {
        assert_eq!(
            ts_type_to_rust_type("string[]"),
            Some("Vec<String>".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("Array<number>"),
            Some("Vec<f64>".to_owned())
        );
    }

    #[test]
    fn test_nullable_array_elements() {
        assert_eq!(
            ts_type_to_rust_type("(string | null)[]"),
            Some("Vec<Option<String>>".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("Array<number | null>"),
            Some("Vec<Option<f64>>".to_owned())
        );
    }

    #[test]
    fn test_nullable_array_itself() {
        assert_eq!(
            ts_type_to_rust_type("string[] | null"),
            Some("Option<Vec<String>>".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("Array<number> | null"),
            Some("Option<Vec<f64>>".to_owned())
        );
    }

    #[test]
    fn test_nullable_array_and_elements() {
        assert_eq!(
            ts_type_to_rust_type("Array<string | null> | null"),
            Some("Option<Vec<Option<String>>>".to_owned())
        );
    }

    #[test]
    fn test_fallback_for_union() {
        assert_eq!(ts_type_to_rust_type("string | number"), None);
        assert_eq!(ts_type_to_rust_type("string | number | null"), None);
    }

    #[test]
    fn test_unknown_types() {
        assert_eq!(ts_type_to_rust_type("foo"), None);
        assert_eq!(
            ts_type_to_rust_type("any"),
            Some("serde_json::Value".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("object"),
            Some("serde_json::Value".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type("unknown"),
            Some("serde_json::Value".to_owned())
        );
    }

    #[test]
    fn test_extra_whitespace() {
        assert_eq!(
            ts_type_to_rust_type("  string | null  "),
            Some("Option<String>".to_owned())
        );
        assert_eq!(
            ts_type_to_rust_type(" Array< string > "),
            Some("Vec<String>".to_owned())
        );
    }

    #[test]
    fn test_map_types() {
        assert_eq!(
            ts_type_to_rust_type("Map<string, number>"),
            Some("HashMap<String, f64>".to_string())
        );
        assert_eq!(
            ts_type_to_rust_type("Map<string, boolean>"),
            Some("HashMap<String, bool>".to_string())
        );
        assert_eq!(
            ts_type_to_rust_type("Map<number, string>"),
            Some("HashMap<f64, String>".to_string())
        );
    }

    #[test]
    fn test_set_types() {
        assert_eq!(
            ts_type_to_rust_type("Set<string>"),
            Some("HashSet<String>".to_string())
        );
        assert_eq!(
            ts_type_to_rust_type("Set<number>"),
            Some("HashSet<f64>".to_string())
        );
        assert_eq!(
            ts_type_to_rust_type("Set<boolean>"),
            Some("HashSet<bool>".to_string())
        );
    }
}
