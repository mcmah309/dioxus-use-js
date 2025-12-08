#![doc = include_str!("../README.md")]

use core::panic;
use indexmap::IndexMap;
use proc_macro::TokenStream;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use std::collections::HashMap;
use std::str::FromStr;
use std::{fs, path::Path};
use swc_common::comments::{CommentKind, Comments};
use swc_common::{SourceMap, Span, comments::SingleThreadedComments};
use swc_common::{SourceMapper, Spanned};
use swc_ecma_ast::{
    Decl, ExportDecl, ExportSpecifier, FnDecl, NamedExport, Pat, TsType, TsTypeAnn, VarDeclarator,
};
use swc_ecma_parser::EsSyntax;
use swc_ecma_parser::{Parser, StringInput, Syntax, lexer::Lexer};
use swc_ecma_visit::{Visit, VisitWith};
use syn::TypeParam;
use syn::{
    Ident, LitStr, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// `JsValue<T>`
const JSVALUE_START: &str = "JsValue";
const JSVALUE: &str = "dioxus_use_js::JsValue";
const DEFAULT_GENRIC_INPUT: &str = "impl dioxus_use_js::SerdeSerialize";
const DEFAULT_GENERIC_OUTPUT: &str = "DeserializeOwned";
const DEFAULT_OUTPUT_GENERIC_DECLARTION: &str =
    "DeserializeOwned: dioxus_use_js::SerdeDeDeserializeOwned";
const SERDE_VALUE: &str = "dioxus_use_js::SerdeJsonValue";
const JSON: &str = "Json";
/// `RustCallback<T,TT>`
const RUST_CALLBACK_JS_START: &str = "RustCallback";
const UNIT: &str = "()";
const DROP_TYPE: &str = "Drop";
const DROP_NAME: &str = "drop";

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
    rust_type: RustType,
}

impl ParamInfo {
    fn is_drop(&self) -> bool {
        match self.js_type.as_ref() {
            Some(js_type) => js_type == DROP_TYPE,
            None => self.name == DROP_NAME,
        }
    }
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    /// If specified in the `use_js!` declaration. Used to link the generated code to this span
    name_ident: Option<Ident>,
    /// js param types
    params: Vec<ParamInfo>,
    // js return type
    #[allow(unused)]
    js_return_type: Option<String>,
    rust_return_type: RustType,
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

    fn extract_doc_comment(&self, span: &Span) -> Vec<String> {
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

#[derive(Debug, Clone)]
enum RustType {
    Regular(String),
    Callback(RustCallback),
    JsValue(JsValue),
}

impl ToString for RustType {
    fn to_string(&self) -> String {
        match self {
            RustType::Regular(ty) => ty.clone(),
            RustType::Callback(callback) => callback.to_string(),
            RustType::JsValue(js_value) => js_value.to_string(),
        }
    }
}

impl RustType {
    fn to_tokens(&self) -> TokenStream2 {
        self.to_string()
            .parse::<TokenStream2>()
            .expect("Calculated Rust type should always be valid")
    }
}

#[derive(Debug, Clone)]
struct RustCallback {
    input: Option<String>,
    output: Option<String>,
}

impl ToString for RustCallback {
    fn to_string(&self) -> String {
        let input = self.input.as_deref();
        let output = self.output.as_deref().unwrap_or(UNIT);
        format!(
            "dioxus::core::Callback<{}, impl Future<Output = Result<{}, dioxus_use_js::SerdeJsonValue>> + 'static>",
            input.unwrap_or("()"),
            output
        )
    }
}

#[derive(Debug, Clone)]
struct JsValue {
    is_option: bool,
    is_input: bool,
}

impl ToString for JsValue {
    fn to_string(&self) -> String {
        if self.is_option {
            format!(
                "Option<{}>",
                if self.is_input {
                    format!("&{}", JSVALUE)
                } else {
                    JSVALUE.to_owned()
                }
            )
        } else {
            if self.is_input {
                format!("&{}", JSVALUE)
            } else {
                JSVALUE.to_owned()
            }
        }
    }
}

fn strip_parenthesis(mut ts_type: &str) -> &str {
    while ts_type.starts_with("(") && ts_type.ends_with(")") {
        ts_type = &ts_type[1..ts_type.len() - 1].trim();
    }
    return ts_type;
}

/// Splits into correct comma delimited arguments
fn split_into_args(ts_type: &str) -> Vec<&str> {
    let mut depth_angle: u16 = 0;
    let mut depth_square: u16 = 0;
    let mut depth_paren: u16 = 0;
    let mut splits = Vec::new();
    let mut last: usize = 0;
    for (i, c) in ts_type.char_indices() {
        match c {
            '<' => depth_angle += 1,
            '>' => depth_angle = depth_angle.saturating_sub(1),
            '[' => depth_square += 1,
            ']' => depth_square = depth_square.saturating_sub(1),
            '(' => depth_paren += 1,
            ')' => depth_paren = depth_paren.saturating_sub(1),
            ',' if depth_angle == 0 && depth_square == 0 && depth_paren == 0 => {
                splits.push(ts_type[last..i].trim());
                last = i + 1;
            }
            _ => {}
        }
    }
    let len = ts_type.len();
    if last != len {
        let maybe_arg = ts_type[last..len].trim();
        if !maybe_arg.is_empty() {
            splits.push(maybe_arg);
        }
    }
    splits
}

fn ts_type_to_rust_type(ts_type: Option<&str>, is_input: bool) -> RustType {
    let Some(mut ts_type) = ts_type else {
        return RustType::Regular(
            (if is_input {
                DEFAULT_GENRIC_INPUT
            } else {
                DEFAULT_GENERIC_OUTPUT
            })
            .to_owned(),
        );
    };
    ts_type = strip_parenthesis(&mut ts_type);
    if ts_type.starts_with("Promise<") && ts_type.ends_with(">") {
        assert!(!is_input, "Promise cannot be used as input type");
        ts_type = &ts_type[8..ts_type.len() - 1];
    }
    ts_type = strip_parenthesis(&mut ts_type);
    if ts_type.contains(JSVALUE_START) {
        let parts = split_top_level_union(ts_type);
        let len = parts.len();
        if len == 1 && parts[0].starts_with(JSVALUE_START) {
            return RustType::JsValue(JsValue {
                is_option: false,
                is_input,
            });
        }

        if len == 2 && parts.contains(&"null") {
            return RustType::JsValue(JsValue {
                is_option: true,
                is_input,
            });
        } else {
            panic!("Invalid use of `{}` for `{}`", JSVALUE_START, ts_type);
        }
    }
    if ts_type.contains(RUST_CALLBACK_JS_START) {
        if !ts_type.starts_with(RUST_CALLBACK_JS_START) {
            panic!("Nested RustCallback is not valid: {}", ts_type);
        }
        assert!(is_input, "Cannot return a RustCallback: {}", ts_type);
        let ts_type = &ts_type[RUST_CALLBACK_JS_START.len()..];
        if !(ts_type.starts_with("<") && ts_type.ends_with(">")) {
            panic!("Invalid RustCallback type: {}", ts_type);
        }
        let inner = &ts_type[1..ts_type.len() - 1];
        let parts = split_into_args(inner);
        let len = parts.len();
        if len != 2 {
            panic!(
                "A RustCallback type expects two parameters, got: {:?}",
                parts
            );
        }
        let ts_input = parts[0];
        let rs_input = if ts_input == "void" {
            None
        } else {
            let rs_input = ts_type_to_rust_type_helper(ts_input, false);
            if rs_input.is_none() || rs_input.as_ref().is_some_and(|e| e == UNIT) {
                panic!("Type `{ts_input}` is not a valid input for `{RUST_CALLBACK_JS_START}`");
            }
            rs_input
        };
        let ts_output = parts[1];
        let rs_output = if ts_output == "void" {
            None
        } else {
            let rs_output = ts_type_to_rust_type_helper(ts_output, false);
            if rs_output.is_none() || rs_output.as_ref().is_some_and(|e| e == UNIT) {
                panic!("Type `{ts_output}` is not a valid output for `{RUST_CALLBACK_JS_START}`");
            }
            rs_output
        };
        return RustType::Callback(RustCallback {
            input: rs_input,
            output: rs_output,
        });
    }
    RustType::Regular(match ts_type_to_rust_type_helper(ts_type, is_input) {
        Some(value) => {
            if value.contains(UNIT) && (is_input || &value != UNIT) {
                // Would cause serialization errors since `serde_json::Value::Null` or any, cannot be deserialized into `()`.
                // We handle `()` special case to account for this if this is the root type in the output. But not input or output nested.
                panic!("`{}` is not valid in this position", ts_type);
            }
            value
        }
        None => (if is_input {
            DEFAULT_GENRIC_INPUT
        } else {
            DEFAULT_GENERIC_OUTPUT
        })
        .to_owned(),
    })
}

/// Returns None if could not determine type
fn ts_type_to_rust_type_helper(mut ts_type: &str, can_be_ref: bool) -> Option<String> {
    ts_type = ts_type.trim();
    ts_type = strip_parenthesis(&mut ts_type);

    let parts = split_top_level_union(ts_type);
    if parts.len() > 1 {
        // Handle single null union: T | null or null | T
        if parts.len() == 2 && parts.contains(&"null") {
            let inner = parts.iter().find(|p| **p != "null")?;
            let inner_rust = ts_type_to_rust_type_helper(inner, can_be_ref)?;
            return Some(format!("Option<{}>", inner_rust));
        }
        // Unsupported union type
        return None;
    }

    ts_type = parts[0];

    if ts_type.ends_with("[]") {
        let inner = ts_type.strip_suffix("[]").unwrap();
        let inner_rust = ts_type_to_rust_type_helper(inner, false)?;
        return Some(if can_be_ref {
            format!("&[{}]", inner_rust)
        } else {
            format!("Vec<{}>", inner_rust)
        });
    }

    if ts_type.starts_with("Array<") && ts_type.ends_with(">") {
        let inner = &ts_type[6..ts_type.len() - 1];
        let inner_rust = ts_type_to_rust_type_helper(inner, false)?;
        return Some(if can_be_ref {
            format!("&[{}]", inner_rust)
        } else {
            format!("Vec<{}>", inner_rust)
        });
    }

    if ts_type.starts_with("Set<") && ts_type.ends_with(">") {
        let inner = &ts_type[4..ts_type.len() - 1];
        let inner_rust = ts_type_to_rust_type_helper(inner, false)?;
        if can_be_ref {
            return Some(format!("&std::collections::HashSet<{}>", inner_rust));
        } else {
            return Some(format!("std::collections::HashSet<{}>", inner_rust));
        }
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
            let key_rust = ts_type_to_rust_type_helper(key.trim(), false)?;
            let value_rust = ts_type_to_rust_type_helper(value.trim(), false)?;
            if can_be_ref {
                return Some(format!(
                    "&std::collections::HashMap<{}, {}>",
                    key_rust, value_rust
                ));
            } else {
                return Some(format!(
                    "std::collections::HashMap<{}, {}>",
                    key_rust, value_rust
                ));
            }
        } else {
            return None;
        }
    }

    // Base types
    let rust_type = match ts_type {
        "string" => {
            if can_be_ref {
                Some("&str".to_owned())
            } else {
                Some("String".to_owned())
            }
        }
        "number" => Some("f64".to_owned()),
        "boolean" => Some("bool".to_owned()),
        "void" | "undefined" | "never" | "null" => Some(UNIT.to_owned()),
        JSON => {
            if can_be_ref {
                Some(format!("&{SERDE_VALUE}"))
            } else {
                Some(SERDE_VALUE.to_owned())
            }
        }
        "Promise" => {
            panic!("`{}` - nested promises are not valid", ts_type)
        }
        // "any" | "unknown" | "object" | .. etc.
        _ => None,
    };

    rust_type
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

fn function_pat_to_param_info<'a, I>(pats: I, source_map: &SourceMap) -> Vec<ParamInfo>
where
    I: Iterator<Item = &'a Pat>,
{
    pats.enumerate()
        .map(|(i, pat)| to_param_info_helper(i, pat, source_map))
        .collect()
}

fn to_param_info_helper(i: usize, pat: &Pat, source_map: &SourceMap) -> ParamInfo {
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
}

fn function_info_helper<'a, I>(
    visitor: &FunctionVisitor,
    name: String,
    span: &Span,
    params: I,
    return_type: Option<&Box<TsTypeAnn>>,
    is_async: bool,
    is_exported: bool,
) -> FunctionInfo
where
    I: Iterator<Item = &'a Pat>,
{
    let doc_comment = visitor.extract_doc_comment(span);

    let params = function_pat_to_param_info(params, &visitor.source_map);

    let js_return_type = return_type.as_ref().map(|type_ann| {
        let ty = &type_ann.type_ann;
        type_to_string(ty, &visitor.source_map)
    });
    if !is_async
        && let Some(ref js_return_type) = js_return_type
        && js_return_type.starts_with("Promise")
    {
        panic!(
            "Promise return type is only supported for async functions, use `async fn` instead. For `{js_return_type}`"
        );
    }

    let rust_return_type = ts_type_to_rust_type(js_return_type.as_deref(), false);

    FunctionInfo {
        name,
        name_ident: None,
        params,
        js_return_type,
        rust_return_type,
        is_exported,
        is_async,
        doc_comment,
    }
}

impl Visit for FunctionVisitor {
    /// Visit function declarations: function foo() {}
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        let name = node.ident.sym.to_string();
        self.functions.push(function_info_helper(
            self,
            name,
            &node.span(),
            node.function.params.iter().map(|e| &e.pat),
            node.function.return_type.as_ref(),
            node.function.is_async,
            false,
        ));
        node.visit_children_with(self);
    }

    /// Visit function expressions: const foo = function() {}
    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        if let swc_ecma_ast::Pat::Ident(ident) = &node.name {
            if let Some(init) = &node.init {
                let span = node.span();
                let name = ident.id.sym.to_string();
                match &**init {
                    swc_ecma_ast::Expr::Fn(fn_expr) => {
                        self.functions.push(function_info_helper(
                            &self,
                            name,
                            &span,
                            fn_expr.function.params.iter().map(|e| &e.pat),
                            fn_expr.function.return_type.as_ref(),
                            fn_expr.function.is_async,
                            false,
                        ));
                    }
                    swc_ecma_ast::Expr::Arrow(arrow_fn) => {
                        self.functions.push(function_info_helper(
                            &self,
                            name,
                            &span,
                            arrow_fn.params.iter(),
                            arrow_fn.return_type.as_ref(),
                            arrow_fn.is_async,
                            false,
                        ));
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
            let span = node.span();
            let name = fn_decl.ident.sym.to_string();
            self.functions.push(function_info_helper(
                &self,
                name,
                &span,
                fn_decl.function.params.iter().map(|e| &e.pat),
                fn_decl.function.return_type.as_ref(),
                fn_decl.function.is_async,
                true,
            ));
        }
        node.visit_children_with(self);
    }

    /// Visit named exports: export { foo }
    fn visit_named_export(&mut self, node: &NamedExport) {
        for spec in &node.specifiers {
            if let ExportSpecifier::Named(named) = spec {
                let original_name = named.orig.atom().to_string();
                let out_name = named
                    .exported
                    .as_ref()
                    .map(|e| e.atom().to_string())
                    .unwrap_or_else(|| original_name.clone());

                if let Some(func) = self.functions.iter_mut().find(|f| f.name == original_name) {
                    let mut func = func.clone();
                    func.name = out_name;
                    func.is_exported = true;
                    self.functions.push(func);
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

    // Functions are added twice for some reason.
    visitor
        .functions
        .dedup_by(|e1, e2| e1.name.as_str() == e2.name.as_str());
    Ok(visitor.functions)
}

fn take_function_by_name(
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
        ImportSpec::All => Ok(functions.into_iter().filter(|e| e.is_exported).collect()),
        ImportSpec::Single(name) => {
            let mut func = take_function_by_name(name.to_string().as_str(), &mut functions, file)?;
            func.name_ident.replace(name.clone());
            Ok(vec![func])
        }
        ImportSpec::Named(names) => {
            let mut result = Vec::new();
            for name in names {
                let mut func =
                    take_function_by_name(name.to_string().as_str(), &mut functions, file)?;
                func.name_ident.replace(name.clone());
                result.push(func);
            }
            Ok(result)
        }
    }
}

fn generate_function_wrapper(func: &FunctionInfo, asset_path: &LitStr) -> TokenStream2 {
    // If we have callbacks, we cant do a simpl return, we have to do message passing
    let mut callback_name_to_index: HashMap<String, u64> = HashMap::new();
    let mut callback_name_to_info: IndexMap<String, &RustCallback> = IndexMap::new();
    let mut index: u64 = 0;
    let mut needs_drop = false;
    for param in &func.params {
        if let RustType::Callback(callback) = &param.rust_type {
            callback_name_to_index.insert(param.name.to_owned(), index);
            index += 1;
            callback_name_to_info.insert(param.name.to_owned(), callback);
        }
        if param.is_drop() {
            needs_drop = true;
        }
    }
    let js_func_name = &func.name;
    let js_func_name_ident = quote! { FUNC_NAME };

    let mut has_callbacks = false;
    let send_calls: Vec<TokenStream2> = func
        .params
        .iter()
        .flat_map(|param| {
            if param.is_drop() {
                return None;
            }
            let param_name = format_ident!("{}", param.name);
            match &param.rust_type {
                RustType::Regular(_) => Some(quote! {
                    eval.send(#param_name).map_err(|e| dioxus_use_js::JsError::Eval { func: #js_func_name_ident, error: e })?;
                }),
                RustType::JsValue(js_value) => {
                    if js_value.is_option {
                        Some(quote! {
                            #[allow(deprecated)]
                            eval.send(#param_name.map(|e| e.internal_get())).map_err(|e| dioxus_use_js::JsError::Eval { func: #js_func_name_ident, error: e })?;
                        })
                    } else {
                        Some(quote! {
                            #[allow(deprecated)]
                            eval.send(#param_name.internal_get()).map_err(|e| dioxus_use_js::JsError::Eval { func: #js_func_name_ident, error: e })?;
                        })
                    }
                },
                RustType::Callback(_) => {
                    has_callbacks = true;
                    None
                },
            }
        })
        .collect();

    let params_list = func
        .params
        .iter()
        .map(|p| p.name.as_str())
        .collect::<Vec<&str>>()
        .join(", ");
    let prepare_callbacks = if has_callbacks {
        "let _i_=\"**INVOCATION_ID**\";let _l_={};window[_i_]=_l_;let _g_ = 0;let _a_=true;const _c_=(c, v)=>{if(!_a_){return Promise.reject(new Error(\"Channel already destroyed\"));}_g_+=1;if(_g_>Number.MAX_SAFE_INTEGER){_g_= 0;}let o, e;let p=new Promise((rs, rj)=>{o=rs;e=rj});_l_[_g_]=[o, e];dioxus.send([c,_g_,v]);return p;};"
    } else {
        ""
    };
    let param_declarations = func
        .params
        .iter()
        .map(|param| {
            if needs_drop && param.is_drop() {
                return format!("let {}=_dp_;", param.name);
            }
            match &param.rust_type {
            RustType::Regular(_) => {
                format!("let {}=await dioxus.recv();", param.name)
            }
            RustType::JsValue(js_value) => {
                let param_name = &param.name;
                if js_value.is_option {
                format!(
                    "let {param_name}Temp_=await dioxus.recv();let {param_name}=null;if({param_name}Temp_!==null){{{param_name}=window[{param_name}Temp_]}};",
                )
            }
            else {
                format!(
                    "let {param_name}Temp_=await dioxus.recv();let {param_name}=window[{param_name}Temp_];",
                )
            }
            },
            RustType::Callback(rust_callback) => {
                let name = &param.name;
                let index = callback_name_to_index.get(name).unwrap();
                let RustCallback { input, output } = rust_callback;
                match (input, output) {
                    (None, None) => {
                        // no return, but still need to await ack
                        format!(
                            "const {}=async()=>{{await _c_({},null);}};",
                            name, index
                        )
                    },
                    (None, Some(_)) => {
                        format!(
                            "const {}=async()=>{{return await _c_({},null);}};",
                            name, index

                        )
                    },
                    (Some(_), None) => {
                        // no return, but still need to await ack
                        format!(
                            "const {}=async(v)=>{{await _c_({},v);}};",
                            name, index
                        )
                    },
                    (Some(_), Some(_)) => {
                        format!(
                            "const {}=async(v)=>{{return await _c_({},v);}};",
                            name, index
                        )
                    },
                }
            },
        }})
        .collect::<Vec<_>>()
        .join("");
    let mut maybe_await = String::new();
    if func.is_async {
        maybe_await.push_str("await");
    }
    let call_function = match &func.rust_return_type {
        RustType::Regular(_) => {
            format!("return [true, {maybe_await} {js_func_name}({params_list})];")
        }
        RustType::Callback(_) => {
            unreachable!("This cannot be an output type, the macro should have panicked earlier.")
        }
        RustType::JsValue(js_value) => {
            let check = if js_value.is_option {
                // null or undefined is valid, since this is e.g. `Option<JsValue>`
                "if (_v_===null||_v_===undefined){return [true,null];}".to_owned()
            } else {
                format!(
                    "if (_v_===null||_v_===undefined){{console.error(\"The result of `{js_func_name}` was null or undefined, but a value is needed for JsValue\");return [true,null];}}"
                )
            };
            format!(
                "const _v_={maybe_await} {js_func_name}({params_list});{check}let _j_=\"__js-value-\"+crypto.randomUUID();window[_j_]=_v_;return [true,_j_];"
            )
        }
    };
    let drop_declare = if needs_drop {
        "let _d_;let _dp_=new Promise((r)=>_d_=r);"
    } else {
        ""
    };
    let drop_handle = if needs_drop {
        if has_callbacks {
            "(async()=>{await dioxus.recv();dioxus.close();_d_();_a_=false;let w=window[_i_];delete window[_i_];for(const[o, e] of Object.values(w)){e(new Error(\"Channel destroyed\"));}})();"
        } else {
            "(async()=>{await dioxus.recv();dioxus.close();_d_();})();"
        }
    } else {
        ""
    };
    let finally = if needs_drop {
        ""
    } else {
        "finally{dioxus.close();}"
    };
    let asset_path_string = asset_path.value();
    // Note: eval will fail if returning undefined. undefined happens if there is no return type
    let js = format!(
        "const{{{js_func_name}}}=await import(\"{asset_path_string}\");{prepare_callbacks}{drop_declare}{param_declarations}{drop_handle}try{{{call_function}}}catch(e){{console.warn(\"Executing `{js_func_name}` threw:\", e);return [false,null];}}{finally}"
    );
    fn to_raw_string_literal(s: &str) -> Literal {
        let mut hashes = String::from("#");
        while s.contains(&format!("\"{}", hashes)) {
            hashes.push('#');
        }

        let raw = format!("r{h}\"{s}\"{h}", h = hashes);
        Literal::from_str(&raw).unwrap()
    }
    let comment = to_raw_string_literal(&js);
    // Easier debugging to see what the generated js is. Will be compiled away.
    let js_in_comment = quote! {
        #[doc = #comment]
        fn ___above_is_the_generated_js___() {}
    };
    let js_format = js
        .replace("{", "{{")
        .replace("}", "}}")
        .replace(&asset_path_string, "{}");
    let js_format = if has_callbacks {
        js_format.replace("**INVOCATION_ID**", "{}")
    } else {
        js_format
    };

    // Generate parameter types with extracted type information
    let param_types: Vec<_> = func
        .params
        .iter()
        .filter_map(|param| {
            if param.is_drop() {
                return None;
            }
            let param_name = format_ident!("{}", param.name);
            let type_tokens = param.rust_type.to_tokens();
            if let RustType::Callback(_) = param.rust_type {
                Some(quote! { mut #param_name: #type_tokens })
            } else {
                Some(quote! { #param_name: #type_tokens })
            }
        })
        .collect();

    let parsed_type = func.rust_return_type.to_tokens();
    let (return_type_tokens, generic_tokens) = if func.rust_return_type.to_string()
        == DEFAULT_GENERIC_OUTPUT
    {
        let span = func
            .name_ident
            .as_ref()
            .map(|e| e.span())
            .unwrap_or_else(|| proc_macro2::Span::call_site());
        let generic = Ident::new(DEFAULT_GENERIC_OUTPUT, span);
        let generic_decl: TypeParam = syn::parse_str(DEFAULT_OUTPUT_GENERIC_DECLARTION).unwrap();
        (
            quote! { Result<#generic, dioxus_use_js::JsError> },
            Some(quote! { <#generic_decl> }),
        )
    } else {
        (
            quote! { Result<#parsed_type, dioxus_use_js::JsError> },
            None,
        )
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

    // void like returns always send back "Null" as an ack
    let void_output_mapping = if func.rust_return_type.to_string() == UNIT {
        quote! {
            .and_then(|e| {
                if matches!(e, dioxus_use_js::SerdeJsonValue::Null) {
                    Ok(())
                } else {
                    Err(dioxus_use_js::JsError::Eval {
                        func: #js_func_name_ident,
                        error: dioxus::document::EvalError::Serialization(
                            <dioxus_use_js::SerdeJsonError as dioxus_use_js::SerdeDeError>::custom(dioxus_use_js::__BAD_VOID_RETURN.to_owned())
                        )
                    })
                }
            })
        }
    } else {
        quote! {}
    };

    let callback_arms: Vec<TokenStream2> = callback_name_to_index
        .iter()
        .map(|(name, index)| {
            let callback_name = format_ident!("{}", name);
            let callback_info = callback_name_to_info.get(name).unwrap();
            let callback_call = match (&callback_info.input, &callback_info.output) {
                (None, None) => {
                    quote! {
                    dioxus::prelude::spawn({let responder = responder.clone(); async move {
                        let result = #callback_name(()).await;

                        match result {
                            // send ack
                            Ok(_) => responder.respond(request_id, true, dioxus_use_js::SerdeJsonValue::Null),
                            Err(error) => responder.respond(request_id, false, error),
                        }
                    }});
                    }
                },
                (None, Some(_)) => {
                    quote! {
                    dioxus::prelude::spawn({let responder = responder.clone(); async move {
                        let result = #callback_name(()).await;

                        match result {
                            Ok(value) => responder.respond(request_id, true, value),
                            Err(error) => responder.respond(request_id, false, error),
                        }
                    }});
                }
                },
                (Some(_), None) => {
                    quote! {
                    let value = values.next().unwrap();
                    let value = match dioxus_use_js::serde_json_from_value(value) {
                        Ok(value) => value,
                        Err(value) => {
                            responder.respond(request_id, false, dioxus_use_js::SerdeJsonValue::String(dioxus_use_js::__UNEXPECTED_CALLBACK_TYPE.to_owned()));
                            continue;
                        }
                    };

                    dioxus::prelude::spawn({let responder = responder.clone(); async move {
                        let result = #callback_name(value).await;

                        match result {
                            // send ack
                            Ok(_) => responder.respond(request_id, true, dioxus_use_js::SerdeJsonValue::Null),
                            Err(error) => responder.respond(request_id, false, error),
                        }
                    }});
                }
                },
                (Some(_), Some(_)) => {
                    quote! {
                    let value = values.next().unwrap();
                    let value = match dioxus_use_js::serde_json_from_value(value) {
                        Ok(value) => value,
                        Err(value) => {
                            responder.respond(request_id, false, dioxus_use_js::SerdeJsonValue::String(dioxus_use_js::__UNEXPECTED_CALLBACK_TYPE.to_owned()));
                            continue;
                        }
                    };

                    dioxus::prelude::spawn({let responder = responder.clone(); async move {
                        let result = #callback_name(value).await;

                        match result {
                            Ok(value) => responder.respond(request_id, true, value),
                            Err(error) => responder.respond(request_id, false, error),
                        }
                    }});
                }
                }
            };
            quote! {
                #index => {
                    #callback_call
                }
            }
        })
        .collect();

    let callback_spawn = if !callback_arms.is_empty() {
        quote! {
            dioxus::prelude::spawn({
                let mut eval = dioxus_use_js::EvalDrop::new(eval);
                    async move {
                        let responder = dioxus_use_js::CallbackResponder::new(&invocation_id);
                        loop {
                            let result = eval.recv::<dioxus_use_js::SerdeJsonValue>().await;
                            let value = match result {
                                Ok(v) => v,
                                Err(e) => {
                                    // Though we still may be able to accept more callback requests,
                                    // We shutdown otherwise the invocation of this callback will be awaiting forever
                                    // since we can't cancel it since we do not know the id. (Dropping the eval triggers shutdown)
                                    dioxus::prelude::error!(
                                        "Callback receiver errored. Shutting down all callbacks for invocation id `{}`: {:?}",
                                        &invocation_id,
                                        e
                                    );
                                    return;
                                }
                            };
                            let dioxus_use_js::SerdeJsonValue::Array(values) = value else {
                                unreachable!("{}", dioxus_use_js::__CALLBACK_SEND_VALIDATION_MSG);
                            };
                            let len = values.len();
                            if len != 3 {
                                unreachable!("{}", dioxus_use_js::__CALLBACK_SEND_VALIDATION_MSG);
                            }
                            let mut values = values.into_iter();
                            let action = values.next().unwrap().as_u64().expect(dioxus_use_js::__INDEX_VALIDATION_MSG);
                            let request_id = values.next().unwrap().as_u64().expect(dioxus_use_js::__INDEX_VALIDATION_MSG);
                            match action {
                                #(#callback_arms,)*
                                _ => unreachable!("{}", dioxus_use_js::__BAD_CALL_MSG),
                            }
                        }
                    }
            });
        }
    } else {
        quote! {}
    };

    let end_statement = quote! {
        let value = eval.await.map_err(|e| {
            dioxus_use_js::JsError::Eval {
                func: #js_func_name_ident,
                error: e,
            }
        })?;
        let dioxus_use_js::SerdeJsonValue::Array(values) = value else {
            unreachable!("{}", dioxus_use_js::__RESULT_SEND_VALIDATION_MSG);
        };
        if values.len() != 2 {
            unreachable!("{}", dioxus_use_js::__RESULT_SEND_VALIDATION_MSG);
        }
        let mut values = values.into_iter();
        let success = values.next().unwrap().as_bool().expect(dioxus_use_js::__INDEX_VALIDATION_MSG);
        if success {
            let value = values.next().unwrap();
            return dioxus_use_js::serde_json_from_value(value).map_err(|e| {
                dioxus_use_js::JsError::Eval {
                    func: #js_func_name_ident,
                    error: dioxus::document::EvalError::Serialization(e),
                }
            })
            #void_output_mapping;
        } else {
             return Err(dioxus_use_js::JsError::Threw { func: #js_func_name_ident });
        }
    };
    let macro_invocation_id = uuid::Uuid::now_v7().to_string();
    let js_string = if has_callbacks {
        quote! {
            static INVOCATION_NUM: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
            // Each invocation id garuntees a unique namespace for the callback invocation where new requested/responded and everything clean/promises rejected on drop.
            let invocation_id = format!("{}{}", #macro_invocation_id, INVOCATION_NUM.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
            let js = format!(#js_format, MODULE, &invocation_id);
        }
    } else {
        quote! {let js = format!(#js_format, MODULE);}
    };

    quote! {
        #doc_comment
        #[allow(non_snake_case)]
        pub async fn #func_name #generic_tokens(#(#param_types),*) -> #return_type_tokens {
            const MODULE: Asset = asset!(#asset_path);
            const #js_func_name_ident: &str = #js_func_name;
            #js_in_comment
            #js_string
            let mut eval = dioxus::document::eval(js.as_str());
            #(#send_calls)*
            #callback_spawn
            #end_statement
        }
    }
}

/// A macro to create rust bindings to javascript and typescript functions. See [README](https://github.com/mcmah309/dioxus-use-js) and [example](https://github.com/mcmah309/dioxus-use-js/blob/master/example/src/main.rs) for more.
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
    for function in functions_to_generate.iter() {
        for param in function.params.iter() {
            if param.name.starts_with("_") && param.name.ends_with("_") {
                panic!(
                    "Parameter name '{}' in function '{}' is invalid. Parameters starting and ending with underscores are reserved.",
                    param.name, function.name
                );
            }
            if param.name == "dioxus" {
                panic!(
                    "Parameter name 'dioxus' in function '{}' is invalid. This parameter name is reserved.",
                    function.name
                );
            }
            if param.name == function.name {
                panic!(
                    "Parameter name '{}' in function '{}' is invalid. Parameters cannot have the same name as the function.",
                    param.name, function.name
                );
            }
        }
    }

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
        assert_eq!(
            ts_type_to_rust_type(Some("string"), false).to_string(),
            "String"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string"), true).to_string(),
            "&str"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("number"), false).to_string(),
            "f64"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("number"), true).to_string(),
            "f64"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("boolean"), false).to_string(),
            "bool"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("boolean"), true).to_string(),
            "bool"
        );
    }

    #[test]
    fn test_nullable_primitives() {
        assert_eq!(
            ts_type_to_rust_type(Some("string | null"), true).to_string(),
            "Option<&str>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | null"), false).to_string(),
            "Option<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("number | null"), true).to_string(),
            "Option<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("number | null"), false).to_string(),
            "Option<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("boolean | null"), true).to_string(),
            "Option<bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("boolean | null"), false).to_string(),
            "Option<bool>"
        );
    }

    #[test]
    fn test_arrays() {
        assert_eq!(
            ts_type_to_rust_type(Some("string[]"), true).to_string(),
            "&[String]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string[]"), false).to_string(),
            "Vec<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number>"), true).to_string(),
            "&[f64]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number>"), false).to_string(),
            "Vec<f64>"
        );
    }

    #[test]
    fn test_nullable_array_elements() {
        assert_eq!(
            ts_type_to_rust_type(Some("(string | null)[]"), true).to_string(),
            "&[Option<String>]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("(string | null)[]"), false).to_string(),
            "Vec<Option<String>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number | null>"), true).to_string(),
            "&[Option<f64>]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number | null>"), false).to_string(),
            "Vec<Option<f64>>"
        );
    }

    #[test]
    fn test_nullable_array_itself() {
        assert_eq!(
            ts_type_to_rust_type(Some("string[] | null"), true).to_string(),
            "Option<&[String]>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string[] | null"), false).to_string(),
            "Option<Vec<String>>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number> | null"), true).to_string(),
            "Option<&[f64]>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<number> | null"), false).to_string(),
            "Option<Vec<f64>>"
        );
    }

    #[test]
    fn test_nullable_array_and_elements() {
        assert_eq!(
            ts_type_to_rust_type(Some("Array<string | null> | null"), true).to_string(),
            "Option<&[Option<String>]>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Array<string | null> | null"), false).to_string(),
            "Option<Vec<Option<String>>>"
        );
    }

    #[test]
    fn test_fallback_for_union() {
        assert_eq!(
            ts_type_to_rust_type(Some("string | number"), true).to_string(),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | number"), false).to_string(),
            "DeserializeOwned"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | number | null"), true).to_string(),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("string | number | null"), false).to_string(),
            "DeserializeOwned"
        );
    }

    #[test]
    fn test_unknown_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("foo"), true).to_string(),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("foo"), false).to_string(),
            "DeserializeOwned"
        );

        assert_eq!(
            ts_type_to_rust_type(Some("any"), true).to_string(),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("any"), false).to_string(),
            "DeserializeOwned"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("object"), true).to_string(),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("object"), false).to_string(),
            "DeserializeOwned"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("unknown"), true).to_string(),
            "impl dioxus_use_js::SerdeSerialize"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("unknown"), false).to_string(),
            "DeserializeOwned"
        );

        assert_eq!(ts_type_to_rust_type(Some("void"), false).to_string(), "()");
        assert_eq!(
            ts_type_to_rust_type(Some("undefined"), false).to_string(),
            "()"
        );
        assert_eq!(ts_type_to_rust_type(Some("null"), false).to_string(), "()");
    }

    #[test]
    fn test_extra_whitespace() {
        assert_eq!(
            ts_type_to_rust_type(Some("  string | null  "), true).to_string(),
            "Option<&str>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("  string | null  "), false).to_string(),
            "Option<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some(" Array< string > "), true).to_string(),
            "&[String]"
        );
        assert_eq!(
            ts_type_to_rust_type(Some(" Array< string > "), false).to_string(),
            "Vec<String>"
        );
    }

    #[test]
    fn test_map_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, number>"), true).to_string(),
            "&std::collections::HashMap<String, f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, number>"), false).to_string(),
            "std::collections::HashMap<String, f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, boolean>"), true).to_string(),
            "&std::collections::HashMap<String, bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<string, boolean>"), false).to_string(),
            "std::collections::HashMap<String, bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<number, string>"), true).to_string(),
            "&std::collections::HashMap<f64, String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Map<number, string>"), false).to_string(),
            "std::collections::HashMap<f64, String>"
        );
    }

    #[test]
    fn test_set_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("Set<string>"), true).to_string(),
            "&std::collections::HashSet<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<string>"), false).to_string(),
            "std::collections::HashSet<String>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<number>"), true).to_string(),
            "&std::collections::HashSet<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<number>"), false).to_string(),
            "std::collections::HashSet<f64>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<boolean>"), true).to_string(),
            "&std::collections::HashSet<bool>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Set<boolean>"), false).to_string(),
            "std::collections::HashSet<bool>"
        );
    }

    #[test]
    fn test_rust_callback() {
        assert_eq!(
            ts_type_to_rust_type(Some("RustCallback<number,string>"), true).to_string(),
            "dioxus::core::Callback<f64, impl Future<Output = Result<String, dioxus_use_js::SerdeJsonValue>> + 'static>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("RustCallback<void,string>"), true).to_string(),
            "dioxus::core::Callback<(), impl Future<Output = Result<String, dioxus_use_js::SerdeJsonValue>> + 'static>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("RustCallback<void,void>"), true).to_string(),
            "dioxus::core::Callback<(), impl Future<Output = Result<(), dioxus_use_js::SerdeJsonValue>> + 'static>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("RustCallback<number,void>"), true).to_string(),
            "dioxus::core::Callback<f64, impl Future<Output = Result<(), dioxus_use_js::SerdeJsonValue>> + 'static>"
        );
    }

    #[test]
    fn test_promise_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("Promise<string>"), false).to_string(),
            "String"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Promise<number>"), false).to_string(),
            "f64"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Promise<boolean>"), false).to_string(),
            "bool"
        );
    }

    #[test]
    fn test_json_types() {
        assert_eq!(
            ts_type_to_rust_type(Some("Json"), true).to_string(),
            "&dioxus_use_js::SerdeJsonValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("Json"), false).to_string(),
            "dioxus_use_js::SerdeJsonValue"
        );
    }

    #[test]
    fn test_js_value() {
        assert_eq!(
            ts_type_to_rust_type(Some("JsValue"), true).to_string(),
            "&dioxus_use_js::JsValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("JsValue"), false).to_string(),
            "dioxus_use_js::JsValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("JsValue<CustomType>"), true).to_string(),
            "&dioxus_use_js::JsValue"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("JsValue<CustomType>"), false).to_string(),
            "dioxus_use_js::JsValue"
        );

        assert_eq!(
            ts_type_to_rust_type(Some("Promise<JsValue>"), false).to_string(),
            "dioxus_use_js::JsValue"
        );

        assert_eq!(
            ts_type_to_rust_type(Some("Promise<JsValue | null>"), false).to_string(),
            "Option<dioxus_use_js::JsValue>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("JsValue | null"), true).to_string(),
            "Option<&dioxus_use_js::JsValue>"
        );
        assert_eq!(
            ts_type_to_rust_type(Some("JsValue | null"), false).to_string(),
            "Option<dioxus_use_js::JsValue>"
        );
    }
}
