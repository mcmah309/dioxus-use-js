#![doc = include_str!("../README.md")]

use base64::Engine;
use core::panic;
use indexmap::IndexMap;
use proc_macro::TokenStream;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use std::collections::HashMap;
use std::str::FromStr;
use std::{fs, path::Path};
use swc_common::comments::{CommentKind, Comments};
use swc_common::{SourceMap, comments::SingleThreadedComments};
use swc_common::{SourceMapper, Spanned};
use swc_ecma_ast::{
    ClassDecl, ClassMember, Decl, ExportDecl, ExportSpecifier, FnDecl, NamedExport, Pat, PropName,
    TsType, TsTypeAnn, VarDeclarator,
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
    ident: Option<Ident>,
    /// js param types
    params: Vec<ParamInfo>,
    // js return type
    js_return_type: Option<String>,
    rust_return_type: RustType,
    is_exported: bool,
    is_async: bool,
    /// The stripped lines
    doc_comment: Vec<String>,
}

#[derive(Debug, Clone)]
struct MethodInfo {
    name: String,
    /// js param types
    params: Vec<ParamInfo>,
    // js return type
    js_return_type: Option<String>,
    rust_return_type: RustType,
    is_async: bool,
    is_static: bool,
    /// The stripped lines
    doc_comment: Vec<String>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct ClassInfo {
    name: String,
    /// If specified in the `use_js!` declaration. Used to link the generated code to this span
    ident: Option<Ident>,
    /// Class methods
    methods: Vec<MethodInfo>,
    is_exported: bool,
    /// The stripped lines
    doc_comment: Vec<String>,
}

struct JsVisitor {
    functions: Vec<FunctionInfo>,
    classes: Vec<ClassInfo>,
    comments: SingleThreadedComments,
    source_map: SourceMap,
}

impl JsVisitor {
    fn new(comments: SingleThreadedComments, source_map: SourceMap) -> Self {
        Self {
            functions: Vec::new(),
            classes: Vec::new(),
            comments,
            source_map,
        }
    }

    fn extract_doc_comment(&self, span: &swc_common::Span) -> Vec<String> {
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
    visitor: &JsVisitor,
    name: String,
    span: &swc_common::Span,
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
        ident: None,
        params,
        js_return_type,
        rust_return_type,
        is_exported,
        is_async,
        doc_comment,
    }
}

impl Visit for JsVisitor {
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

    /// Visit export declarations: export function foo() {} or export class Bar {}
    fn visit_export_decl(&mut self, node: &ExportDecl) {
        match &node.decl {
            Decl::Fn(fn_decl) => {
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
            Decl::Class(class_decl) => {
                let name = class_decl.ident.sym.to_string();
                let span = class_decl.class.span();
                let doc_comment = self.extract_doc_comment(&span);
                let mut methods = Vec::new();

                for member in &class_decl.class.body {
                    match member {
                        ClassMember::Method(method) => {
                            let method_name = match &method.key {
                                PropName::Ident(ident) => ident.sym.to_string(),
                                PropName::Str(str_lit) => str_lit.value.to_string(),
                                _ => continue,
                            };

                            let method_span = method.span();
                            let method_doc = self.extract_doc_comment(&method_span);

                            let params = function_pat_to_param_info(
                                method.function.params.iter().map(|p| &p.pat),
                                &self.source_map,
                            );

                            let js_return_type =
                                method.function.return_type.as_ref().map(|type_ann| {
                                    let ty = &type_ann.type_ann;
                                    type_to_string(ty, &self.source_map)
                                });

                            let is_async = method.function.is_async;
                            if !is_async
                                && js_return_type
                                    .as_ref()
                                    .is_some_and(|js_return_type: &String| {
                                        js_return_type.starts_with("Promise")
                                    })
                            {
                                panic!(
                                    "Method `{}` in exported class `{}` returns a Promise but is not marked as async",
                                    method_name, name
                                );
                            }

                            let rust_return_type =
                                ts_type_to_rust_type(js_return_type.as_deref(), false);

                            methods.push(MethodInfo {
                                name: method_name,
                                params,
                                js_return_type,
                                rust_return_type,
                                is_async,
                                is_static: method.is_static,
                                doc_comment: method_doc,
                            });
                        }
                        _ => {}
                    }
                }

                self.classes.push(ClassInfo {
                    name,
                    ident: None,
                    methods,
                    is_exported: true,
                    doc_comment,
                });
            }
            _ => {}
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
                    func.name = out_name.clone();
                    func.is_exported = true;
                    self.functions.push(func);
                }

                if let Some(class) = self.classes.iter_mut().find(|c| c.name == original_name) {
                    let mut class = class.clone();
                    class.name = out_name.clone();
                    class.is_exported = true;
                    self.classes.push(class);
                }
            }
        }
        node.visit_children_with(self);
    }

    /// Visit class declarations: class Foo {}
    fn visit_class_decl(&mut self, node: &ClassDecl) {
        let name = node.ident.sym.to_string();
        let span = node.span();
        let doc_comment = self.extract_doc_comment(&span);
        let mut methods = Vec::new();

        for member in &node.class.body {
            match member {
                ClassMember::Method(method) => {
                    let method_name = match &method.key {
                        PropName::Ident(ident) => ident.sym.to_string(),
                        PropName::Str(str_lit) => str_lit.value.to_string(),
                        _ => continue,
                    };

                    let method_span = method.span();
                    let method_doc = self.extract_doc_comment(&method_span);

                    let params = function_pat_to_param_info(
                        method.function.params.iter().map(|p| &p.pat),
                        &self.source_map,
                    );

                    let js_return_type = method.function.return_type.as_ref().map(|type_ann| {
                        let ty = &type_ann.type_ann;
                        type_to_string(ty, &self.source_map)
                    });

                    let is_async = method.function.is_async;
                    if !is_async
                        && js_return_type
                            .as_ref()
                            .is_some_and(|js_return_type: &String| {
                                js_return_type.starts_with("Promise")
                            })
                    {
                        panic!(
                            "Function `{}` in class `{}` returns a Promise but is not marked as async",
                            method_name, name
                        );
                    }

                    let rust_return_type = ts_type_to_rust_type(js_return_type.as_deref(), false);

                    methods.push(MethodInfo {
                        name: method_name,
                        params,
                        js_return_type,
                        rust_return_type,
                        is_async,
                        is_static: method.is_static,
                        doc_comment: method_doc,
                    });
                }
                _ => {}
            }
        }

        self.classes.push(ClassInfo {
            name,
            ident: None,
            methods,
            is_exported: false,
            doc_comment,
        });

        node.visit_children_with(self);
    }
}

fn parse_script_file(file_path: &Path, is_js: bool) -> Result<(Vec<FunctionInfo>, Vec<ClassInfo>)> {
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

    let mut visitor = JsVisitor::new(comments, source_map);
    module.visit_with(&mut visitor);

    // Functions and classes are added twice for some reason.
    visitor
        .functions
        .dedup_by(|e1, e2| e1.name.as_str() == e2.name.as_str());
    visitor
        .classes
        .dedup_by(|e1, e2| e1.name.as_str() == e2.name.as_str());
    Ok((visitor.functions, visitor.classes))
}

fn get_types_to_generate(
    classes: Vec<ClassInfo>,
    functions: Vec<FunctionInfo>,
    import_spec: &ImportSpec,
    file: &Path,
) -> Result<(Vec<ClassInfo>, Vec<FunctionInfo>)> {
    fn named_helper(
        names: &Vec<Ident>,
        mut classes: Vec<ClassInfo>,
        mut functions: Vec<FunctionInfo>,
        file: &Path,
    ) -> Result<(Vec<ClassInfo>, Vec<FunctionInfo>)> {
        let mut funcs = Vec::new();
        let mut classes = Vec::new();
        for name in names {
            let name_str = name.to_string();
            if let Some(pos) = functions
                .iter()
                .position(|f: &FunctionInfo| f.name == name_str)
            {
                let mut function_info = functions.remove(pos);
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
                function_info.ident.replace(name.clone());
                funcs.push(function_info);
            } else if let Some(pos) = classes.iter().position(|c: &ClassInfo| c.name == name_str) {
                let mut class_info = classes.remove(pos);
                if !class_info.is_exported {
                    return Err(syn::Error::new(
                        proc_macro2::Span::call_site(),
                        format!("Class '{}' not exported in file '{}'", name, file.display()),
                    ));
                }
                class_info.ident.replace(name.clone());
                classes.push(class_info);
            } else {
                return Err(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    format!(
                        "Function or Class '{}' not found in file '{}'",
                        name,
                        file.display()
                    ),
                ));
            }
        }
        Ok((classes, funcs))
    }
    match import_spec {
        ImportSpec::All => Ok((
            classes.into_iter().filter(|e| e.is_exported).collect(),
            functions.into_iter().filter(|e| e.is_exported).collect(),
        )),
        ImportSpec::Single(name) => named_helper(&vec![name.clone()], classes, functions, file),
        ImportSpec::Named(names) => named_helper(names, classes, functions, file),
    }
}

fn generate_class_wrapper(
    class: &ClassInfo,
    asset_path: &LitStr,
    function_id_hasher: &blake3::Hasher,
) -> TokenStream2 {
    let class_ident = class
        .ident
        .clone()
        .unwrap_or_else(|| Ident::new(class.name.as_str(), proc_macro2::Span::call_site()));

    let doc_comment = if class.doc_comment.is_empty() {
        quote! {}
    } else {
        let doc_lines: Vec<_> = class
            .doc_comment
            .iter()
            .map(|line| quote! { #[doc = #line] })
            .collect();
        quote! { #(#doc_lines)* }
    };

    let mut parts: Vec<TokenStream2> = Vec::new();
    for method in &class.methods {
        let func_info = FunctionInfo {
            name: method.name.clone(),
            ident: None,
            params: method.params.clone(),
            js_return_type: method.js_return_type.clone(),
            rust_return_type: method.rust_return_type.clone(),
            is_exported: true,
            is_async: method.is_async,
            doc_comment: method.doc_comment.clone(),
        };

        let inner_function = generate_invocation(
            Some(FunctionClassContext {
                class_name: class.name.clone(),
                ident: class_ident.clone(),
                is_static: method.is_static,
            }),
            &func_info,
            asset_path,
            function_id_hasher,
        );

        let method_name = format_ident!("{}", method.name);
        let method_params: Vec<_> = method
            .params
            .iter()
            .filter_map(|param| {
                if param.is_drop() {
                    return None;
                }
                let param_name = format_ident!("{}", param.name);
                let type_tokens = param.rust_type.to_tokens();
                Some(quote! { #param_name: #type_tokens })
            })
            .collect();

        let (return_type_tokens, generic_tokens) = return_type_tokens(
            &method.rust_return_type,
            class.ident.as_ref().map(|e| e.span()),
        );

        let method_doc = if method.doc_comment.is_empty() {
            quote! {}
        } else {
            let doc_lines: Vec<_> = method
                .doc_comment
                .iter()
                .map(|line| quote! { #[doc = #line] })
                .collect();
            quote! { #(#doc_lines)* }
        };

        let param_names: Vec<_> = method
            .params
            .iter()
            .map(|p| format_ident!("{}", p.name))
            .collect();

        let part = if method.is_static {
            quote! {
                #method_doc
                #[allow(non_snake_case)]
                pub async fn #method_name #generic_tokens(#(#method_params),*) -> #return_type_tokens {
                    #inner_function
                    #method_name(#(#param_names),*).await
                }
            }
        } else {
            quote! {
                #method_doc
                #[allow(non_snake_case)]
                pub async fn #method_name #generic_tokens(&self, #(#method_params),*) -> #return_type_tokens {
                    #inner_function
                    #method_name(&self.0, #(#param_names),*).await
                }
            }
        };

        parts.push(part);
    }

    quote! {
        #doc_comment
        #[derive(Clone, Debug)]
        pub struct #class_ident(dioxus_use_js::JsValue);

        impl #class_ident {
            pub fn new(js_value: dioxus_use_js::JsValue) -> Self {
                Self(js_value)
            }
        }

        impl #class_ident {
            #(#parts)*
        }

        impl AsRef<dioxus_use_js::JsValue> for #class_ident {
            fn as_ref(&self) -> &dioxus_use_js::JsValue {
                &self.0
            }
        }
    }
}

struct FunctionClassContext {
    class_name: String,
    ident: Ident,
    is_static: bool,
}

fn generate_invocation(
    class: Option<FunctionClassContext>,
    func: &FunctionInfo,
    asset_path: &LitStr,
    function_id_hasher: &blake3::Hasher,
) -> TokenStream2 {
    let is_class_method = class.as_ref().is_some_and(|e| !e.is_static);
    let mut params = func.params.clone();
    if is_class_method {
        let new_param = ParamInfo {
            name: "_m_".to_owned(),
            js_type: None,
            rust_type: RustType::JsValue(JsValue {
                is_option: false,
                is_input: true,
            }),
        };
        params.insert(0, new_param);
    }
    // If we have callbacks, we cant do a simpl return, we have to do message passing
    let mut callback_name_to_index: HashMap<String, u64> = HashMap::new();
    let mut callback_name_to_info: IndexMap<String, &RustCallback> = IndexMap::new();
    let mut index: u64 = 0;
    let mut needs_drop = false;
    let mut has_callbacks = false;
    for param in &params {
        if let RustType::Callback(callback) = &param.rust_type {
            callback_name_to_index.insert(param.name.to_owned(), index);
            index += 1;
            callback_name_to_info.insert(param.name.to_owned(), callback);
            has_callbacks = true;
            needs_drop = true;
        } else if param.is_drop() {
            needs_drop = true;
        }
    }
    let func_name_str = &func.name;
    let func_name_static_ident = quote! { FUNC_NAME };

    let send_calls: Vec<TokenStream2> = params
        .iter()
        .flat_map(|param| {
            if param.is_drop() {
                return None;
            }
            let param_name = format_ident!("{}", param.name);
            match &param.rust_type {
                RustType::Regular(_) => Some(quote! {
                    eval.send(#param_name).map_err(|e| dioxus_use_js::JsError::Eval { func: #func_name_static_ident, error: e })?;
                }),
                RustType::JsValue(js_value) => {
                    if js_value.is_option {
                        Some(quote! {
                            #[allow(deprecated)]
                            eval.send(#param_name.map(|e| e.internal_get())).map_err(|e| dioxus_use_js::JsError::Eval { func: #func_name_static_ident, error: e })?;
                        })
                    } else {
                        Some(quote! {
                            #[allow(deprecated)]
                            eval.send(#param_name.internal_get()).map_err(|e| dioxus_use_js::JsError::Eval { func: #func_name_static_ident, error: e })?;
                        })
                    }
                },
                RustType::Callback(_) => {
                    None
                },
            }
        })
        .collect();

    // Note we use `func.params` here
    let call_params = &func
        .params
        .iter()
        .map(|p| p.name.as_str())
        .collect::<Vec<&str>>()
        .join(", ");
    let prepare = if has_callbacks {
        assert!(needs_drop);
        "let _i_=\"**INVOCATION_ID**\";let _l_={};window[_i_]=_l_;let _g_ = 0;let _a_=true;const _c_=(c, v)=>{if(!_a_){return Promise.reject(new Error(\"Channel already destroyed\"));}_g_+=1;if(_g_>Number.MAX_SAFE_INTEGER){_g_= 0;}let o, e;let p=new Promise((rs, rj)=>{o=rs;e=rj});_l_[_g_]=[o, e];dioxus.send([c,_g_,v]);return p;};"
    } else if needs_drop {
        "let _i_=\"**INVOCATION_ID**\";"
    } else {
        ""
    };
    let param_declarations = &params
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
                    "let _{param_name}T_=await dioxus.recv();let {param_name}=null;if(_{param_name}T_!==null){{{param_name}=window[_{param_name}T_]}};",
                )
            }
            else {
                format!(
                    "let _{param_name}T_=await dioxus.recv();let {param_name}=window[_{param_name}T_];",
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
    let func_call_full_path = if is_class_method {
        let var_name = &params.first().unwrap().name;
        format!("{var_name}.{func_name_str}")
    } else if let Some(class) = &class {
        let class_name = &class.class_name;
        format!("{class_name}.{func_name_str}")
    } else {
        func_name_str.to_owned()
    };
    let call_function = match &func.rust_return_type {
        RustType::Regular(_) => {
            format!("return [true, {maybe_await} {func_call_full_path}({call_params})];")
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
                    "if (_v_===null||_v_===undefined){{console.error(\"The result of `{func_call_full_path}` was null or undefined, but a value is needed for JsValue\");return [true,null];}}"
                )
            };
            format!(
                "const _v_={maybe_await} {func_call_full_path}({call_params});{check}let _j_=\"__js-value-\"+crypto.randomUUID();window[_j_]=_v_;return [true,_j_];"
            )
        }
    };
    let drop_declare = if needs_drop {
        // Note the additional `d` also added by `SignalDrop`
        "let _d_;let _dp_=new Promise((r)=>_d_=r);window[_i_+\"d\"]=_d_;"
    } else {
        ""
    };
    let drop_handle = if needs_drop {
        if has_callbacks {
            "(async()=>{await _dp_;dioxus.close();_a_=false;let w=window[_i_];delete window[_i_];for(const[o, e] of Object.values(w)){e(new Error(\"Channel destroyed\"));}})();"
        } else {
            "(async()=>{await _dp_;dioxus.close();})();"
        }
    } else {
        assert!(
            !has_callbacks,
            "If this is true then needing drop should be true"
        );
        ""
    };
    let finally = if needs_drop {
        ""
    } else {
        "finally{dioxus.close();}"
    };
    let asset_path_string = asset_path.value();
    // Note: eval will fail if returning undefined. undefined happens if there is no return type
    let js = if is_class_method {
        format!(
            "{prepare}{drop_declare}{param_declarations}{drop_handle}try{{{call_function}}}catch(e){{console.warn(\"Executing `{func_call_full_path}` threw:\", e);return [false,null];}}{finally}"
        )
    } else if let Some(class) = &class {
        let class_name = &class.class_name;
        format!(
            "const{{{class_name}}}=await import(\"{asset_path_string}\");{prepare}{drop_declare}{param_declarations}{drop_handle}try{{{call_function}}}catch(e){{console.warn(\"Executing `{func_call_full_path}` threw:\", e);return [false,null];}}{finally}"
        )
    } else {
        assert_eq!(func_call_full_path.as_str(), func_name_str);
        format!(
            "const{{{func_name_str}}}=await import(\"{asset_path_string}\");{prepare}{drop_declare}{param_declarations}{drop_handle}try{{{call_function}}}catch(e){{console.warn(\"Executing `{func_call_full_path}` threw:\", e);return [false,null];}}{finally}"
        )
    };
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
    let js_format = js.replace("{", "{{").replace("}", "}}");
    let js_format = if is_class_method {
        assert!(!js_format.contains(&asset_path_string));
        js_format
    } else {
        js_format.replace(&asset_path_string, "{}")
    };
    let js_format = if needs_drop {
        js_format.replace("**INVOCATION_ID**", "{}")
    } else {
        js_format
    };
    let js_eval_statement = if needs_drop {
        let js_line = if is_class_method {
            quote! {
            let js = format!(#js_format, &invocation_id);
            }
        } else {
            quote! {
                const MODULE: Asset = asset!(#asset_path);
                let js = format!(#js_format, MODULE, &invocation_id);
            }
        };
        let function_id = {
            let mut hasher = function_id_hasher.clone();
            hasher.update(func_call_full_path.as_bytes());
            let mut output_reader = hasher.finalize_xof();
            let mut truncated_bytes = vec![0u8; 10];
            use std::io::Read;
            output_reader.read_exact(&mut truncated_bytes).unwrap();
            let function_id =
                base64::engine::general_purpose::STANDARD_NO_PAD.encode(truncated_bytes);
            function_id
        };
        quote! {
            static INVOCATION_NUM: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
            // Each invocation id guarentees a unique namespace for the callback invocation for requests/responses and on drop everything there can be cleaned up and outstanding promises rejected.
            let invocation_id = format!("__{}{}", #function_id, INVOCATION_NUM.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
            #js_line
            let mut eval = dioxus::document::eval(js.as_str());
        }
    } else {
        if is_class_method {
            quote! {
                let js = #js_format;
                let mut eval = dioxus::document::eval(js);
            }
        } else {
            quote! {
                const MODULE: Asset = asset!(#asset_path);
                let js = format!(#js_format, MODULE);
                let mut eval = dioxus::document::eval(js.as_str());
            }
        }
    };

    // Generate parameter types with extracted type information
    let param_types: Vec<_> = params
        .iter()
        .filter_map(|param| {
            if param.is_drop() {
                return None;
            }
            let param_name = format_ident!("{}", param.name);
            let type_tokens = param.rust_type.to_tokens();
            Some(quote! { #param_name: #type_tokens })
        })
        .collect();

    let (return_type_tokens, generic_tokens) = return_type_tokens(
        &func.rust_return_type,
        func.ident.as_ref().map(|e| e.span()),
    );

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
        .ident
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
                        func: #func_name_static_ident,
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
                    async move {
                        let responder = dioxus_use_js::CallbackResponder::new(&invocation_id);
                        let _signal_drop = dioxus_use_js::SignalDrop::new(invocation_id.clone());
                        loop {
                            let result = eval.recv::<dioxus_use_js::SerdeJsonValue>().await;
                            let value = match result {
                                Ok(v) => v,
                                Err(e) => {
                                    // Though we still may be able to accept more callback requests,
                                    // We shutdown otherwise the invocation of this callback will be awaiting forever
                                    // (since we don't know where the request came from so we cannot cancel it).
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
    } else if needs_drop {
        // We can't use `use_drop` because the rule of hooks, so like the callback case, we just
        // spawn a future that will never finish, but the eval will drop and fire off the signal
        // when the component drops.
        quote! {
            dioxus::prelude::spawn(async move {
                let _signal_drop = dioxus_use_js::SignalDrop::new(invocation_id);
                let f = dioxus_use_js::PendingFuture;
                f.await;
            });
        }
    } else {
        quote! {}
    };

    let end_statement = quote! {
        let value = eval.await.map_err(|e| {
            dioxus_use_js::JsError::Eval {
                func: #func_name_static_ident,
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
                    func: #func_name_static_ident,
                    error: dioxus::document::EvalError::Serialization(e),
                }
            })
            #void_output_mapping;
        } else {
             return Err(dioxus_use_js::JsError::Threw { func: #func_name_static_ident });
        }
    };

    quote! {
        #doc_comment
        #[allow(non_snake_case)]
        pub async fn #func_name #generic_tokens(#(#param_types),*) -> #return_type_tokens {
            const #func_name_static_ident: &str = #func_name_str;
            #js_in_comment
            #js_eval_statement
            #(#send_calls)*
            #callback_spawn
            #end_statement
        }
    }
}

fn return_type_tokens(
    return_type: &RustType,
    span: Option<proc_macro2::Span>,
) -> (proc_macro2::TokenStream, Option<proc_macro2::TokenStream>) {
    let span = span.unwrap_or_else(|| proc_macro2::Span::call_site());
    let parsed_type = return_type.to_tokens();
    if return_type.to_string() == DEFAULT_GENERIC_OUTPUT {
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

    let (js_all_functions, js_all_classes) = match parse_script_file(&js_file_path, true) {
        Ok(result) => result,
        Err(e) => return TokenStream::from(e.to_compile_error()),
    };

    let (js_classes_to_generate, js_functions_to_generate) = match get_types_to_generate(
        js_all_classes,
        js_all_functions,
        &import_spec,
        &js_file_path,
    ) {
        Ok((classes, funcs)) => (classes, funcs),
        Err(e) => {
            return TokenStream::from(e.to_compile_error());
        }
    };

    let (functions_to_generate, classes_to_generate) = if let Some(ts_file_path) = ts_source_path {
        let ts_file_path = std::path::Path::new(&manifest_dir).join(ts_file_path.value());
        let (ts_all_functions, ts_all_classes) = match parse_script_file(&ts_file_path, false) {
            Ok(result) => result,
            Err(e) => return TokenStream::from(e.to_compile_error()),
        };

        let (ts_classes_to_generate, ts_functions_to_generate) = match get_types_to_generate(
            ts_all_classes,
            ts_all_functions,
            &import_spec,
            &ts_file_path,
        ) {
            Ok((classes, funcs)) => (classes, funcs),
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

        // Validate classes match between TS and JS
        for ts_class in ts_classes_to_generate.iter() {
            if let Some(js_class) = js_classes_to_generate
                .iter()
                .find(|c| c.name == ts_class.name)
            {
                if ts_class.methods.len() != js_class.methods.len() {
                    return TokenStream::from(syn::Error::new(
                        proc_macro2::Span::call_site(),
                        format!(
                            "Class '{}' has different method count in JS and TS files. Bundle may be out of date",
                            ts_class.name
                        ),
                    )
                    .to_compile_error());
                }
            } else {
                return TokenStream::from(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    format!(
                        "Class '{}' is defined in TS file but not in JS file. Bundle may be out of date",
                        ts_class.name
                    ),
                )
                .to_compile_error());
            }
        }

        (ts_functions_to_generate, ts_classes_to_generate)
    } else {
        (js_functions_to_generate, js_classes_to_generate)
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

    let call_site_span = proc_macro::Span::call_site();
    let file = call_site_span.file();
    let line_number = call_site_span.line();
    let column_number = call_site_span.column();
    let mut unhashed_id = file;
    unhashed_id.push_str(":");
    unhashed_id.push_str(&line_number.to_string());
    unhashed_id.push_str(":");
    unhashed_id.push_str(&column_number.to_string());
    unhashed_id.push_str(":");
    let mut function_id_hasher = blake3::Hasher::new();
    function_id_hasher.update(unhashed_id.as_bytes());

    let function_wrappers: Vec<TokenStream2> = functions_to_generate
        .iter()
        .map(|func| generate_invocation(None, func, &js_bundle_path, &function_id_hasher))
        .collect();

    let class_wrappers: Vec<TokenStream2> = classes_to_generate
        .iter()
        .map(|class| generate_class_wrapper(class, &js_bundle_path, &function_id_hasher))
        .collect();

    let expanded = quote! {
        #(#function_wrappers)*
        #(#class_wrappers)*
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

    #[test]
    fn test_class_parsing() {
        let ts_content = r#"
            /**
             * A test class
             */
            export class MyClass {
                constructor(name: string, value: number) {}
                
                /**
                 * Instance method
                 */
                greet(greeting: string): string {
                    return greeting;
                }
                
                /**
                 * Async method
                 */
                async fetchData(url: string): Promise<string> {
                    return "data";
                }
                
                /**
                 * Static method
                 */
                static create(): MyClass {
                    return new MyClass("test", 0);
                }
            }
        "#;

        let source_map = SourceMap::default();
        let fm = source_map.new_source_file(
            swc_common::FileName::Custom("test.ts".to_string()).into(),
            ts_content.to_string(),
        );
        let comments = SingleThreadedComments::default();

        let syntax = Syntax::Typescript(swc_ecma_parser::TsSyntax {
            tsx: false,
            decorators: false,
            dts: false,
            no_early_errors: false,
            disallow_ambiguous_jsx_like: true,
        });

        let lexer = Lexer::new(
            syntax,
            Default::default(),
            StringInput::from(&*fm),
            Some(&comments),
        );

        let mut parser = Parser::new_from(lexer);
        let module = parser.parse_module().unwrap();

        let mut visitor = JsVisitor::new(comments, source_map);
        module.visit_with(&mut visitor);

        // Dedup classes (as done in parse_script_file)
        visitor
            .classes
            .dedup_by(|e1, e2| e1.name.as_str() == e2.name.as_str());

        // Verify we parsed the class
        assert_eq!(visitor.classes.len(), 1);
        let class = &visitor.classes[0];
        assert_eq!(class.name, "MyClass");
        assert_eq!(class.is_exported, true);

        // Verify methods
        assert_eq!(class.methods.len(), 3);

        let greet = &class.methods[0];
        assert_eq!(greet.name, "greet");
        assert_eq!(greet.is_async, false);
        assert_eq!(greet.is_static, false);
        assert_eq!(greet.params.len(), 1);
        assert_eq!(greet.params[0].name, "greeting");
        assert_eq!(greet.params[0].rust_type.to_string(), "&str");
        assert_eq!(greet.rust_return_type.to_string(), "String");

        let fetch_data = &class.methods[1];
        assert_eq!(fetch_data.name, "fetchData");
        assert_eq!(fetch_data.is_async, true);
        assert_eq!(fetch_data.is_static, false);
        assert_eq!(fetch_data.params.len(), 1);
        assert_eq!(fetch_data.rust_return_type.to_string(), "String");

        let create = &class.methods[2];
        assert_eq!(create.name, "create");
        assert_eq!(create.is_async, false);
        assert_eq!(create.is_static, true);
        assert_eq!(create.params.len(), 0);
        // Note: The return type MyClass would be parsed as JsValue or unknown type
    }
}
