use inwelling::*;

use proc_macro2::Span;
use quote::*;
use std::collections::HashMap;
use std::{env, fs, path::PathBuf};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    visit::Visit,
    Token,
};

fn main() {
    let mut row_map = RowMap::new();
    let mut row_collector = DynRowCollector(&mut row_map);

    for section in inwelling(Opts {
        watch_manifest: true,
        watch_rs_files: true,
        dump_rs_paths: true,
    })
    .sections
    {
        for rs_path in section.rs_paths.unwrap() {
            let contents = String::from_utf8(fs::read(rs_path).unwrap()).unwrap();
            let syntax = syn::parse_file(&contents)
                .expect(".rs files should contain valid Rust source code.");
            row_collector.visit_file(&syntax);
        }
    }

    let mut output = String::new();
    for (dyn_row_name, lens_bounds) in row_map {
        let generics = (0..lens_bounds.len())
            .flat_map(|n| vec![format_ident!("K{}", n), format_ident!("V{}", n)])
            .collect::<Punctuated<_, Token![,]>>();
        let bounds = lens_bounds
            .into_iter()
            .enumerate()
            .map(|(n, bound)| (format_ident!("K{}", n), format_ident!("V{}", n), bound))
            .map(|(k, v, bound)| quote! { lens_rs::#bound<#k, #v> })
            .collect::<Punctuated<_, Token![+]>>();
        let dyn_row_ident = syn::Ident::new(&dyn_row_name, Span::call_site());

        output += &quote! {
            #[allow(non_camel_case_types)]
            pub trait #dyn_row_ident<#generics>: #bounds { }
        }
        .to_string();
        output += &"\n";
        output += &quote! {
            impl<T: ?Sized, #generics> #dyn_row_ident<#generics> for T
            where
                T: #bounds,
            { }
        }
        .to_string();
        output += &"\n";
    }

    let out_path = PathBuf::from(env::var("OUT_DIR").expect("$OUT_DIR should exist."));
    std::fs::write(out_path.join("dyn_row.rs"), output).expect("optics.rs should be generated.");
}

#[derive(Clone, Debug)]
enum Mutability {
    Ref(Token![ref]),
    Mut(Token![mut]),
    Move,
}

#[derive(Clone, Debug)]
enum TypeSuffix {
    Empty,
    Star(Token![*]),
    Question(Token![?]),
}

#[derive(Clone, Debug)]
enum Key {
    Ident(syn::Ident),
    Type {
        _bracket_token: syn::token::Bracket,
        key_type: syn::Type,
    },
}

/// ref a: A?, mut b: B*, c: C, [K]: V
#[derive(Clone, Debug)]
struct RowTypeField {
    mutability: Mutability,
    key: Key,
    _colon_token: Token![:],
    field_type: syn::Type,
    suffix: TypeSuffix,
}

/// row! { a: A, b: B, c: C, .. : Trait1 + Trait2 + 'a }
#[derive(Clone, Debug)]
struct RowType {
    fields: Punctuated<RowTypeField, Token![,]>,
    _dot2token: Token![..],
    _colon_token: Option<Token![:]>,
    bounds: Punctuated<syn::TypeParamBound, Token![+]>,
}

impl Parse for Mutability {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![ref]) {
            Ok(Mutability::Ref(input.parse()?))
        } else if lookahead.peek(Token![mut]) {
            Ok(Mutability::Mut(input.parse()?))
        } else if lookahead.peek(syn::Ident) || lookahead.peek(syn::token::Bracket) {
            Ok(Mutability::Move)
        } else {
            Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "expected `ref`, `mut` or nothing",
            ))
        }
    }
}

impl Parse for TypeSuffix {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![*]) {
            Ok(TypeSuffix::Star(input.parse()?))
        } else if lookahead.peek(Token![?]) {
            Ok(TypeSuffix::Question(input.parse()?))
        } else {
            Ok(TypeSuffix::Empty)
        }
    }
}

impl Parse for Key {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Bracket) {
            let content;
            let _bracket_token = syn::bracketed!(content in input);
            Ok(Self::Type {
                _bracket_token,
                key_type: content.parse()?,
            })
        } else {
            Ok(Self::Ident(input.parse()?))
        }
    }
}

impl ToTokens for Key {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Key::Ident(id) => tokens.extend(quote! { lens_rs::Optics![#id] }),
            Key::Type { key_type, .. } => key_type.to_tokens(tokens),
        }
    }
}

impl Parse for RowTypeField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            mutability: input.parse()?,
            key: input.parse()?,
            _colon_token: input.parse()?,
            field_type: input.parse()?,
            suffix: input.parse()?,
        })
    }
}

impl Parse for RowType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut fields = Punctuated::new();
        while !input.is_empty() && !input.peek(Token![..]) {
            let row_field = input.call(RowTypeField::parse)?;
            fields.push_value(row_field);
            if input.is_empty() {
                break;
            }
            let punct: Token![,] = input.parse()?;
            fields.push_punct(punct);
        }

        let _dot2token = if fields.empty_or_trailing() && input.peek(Token![..]) {
            input.parse()?
        } else {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "expected `..` token",
            ));
        };

        let _colon_token = if input.peek(Token![:]) {
            Some(input.parse()?)
        } else {
            return Ok(Self {
                fields,
                _dot2token,
                _colon_token: None,
                bounds: Default::default(),
            });
        };

        Ok(Self {
            fields,
            _dot2token,
            _colon_token,
            bounds: Punctuated::parse_terminated(&input)?,
        })
    }
}

fn join_dyn_row_field(
    mut fields: Vec<RowTypeField>,
) -> (String, Vec<Key>, Vec<syn::Type>, Vec<syn::Ident>) {
    fields.sort_by_key(|field| map_trait(&field.suffix, &field.mutability));

    let mut dyn_row_name = "_dyn_row".to_string();
    let mut fields_key = Vec::new();
    let mut fields_ty = Vec::new();
    let mut optics_trait = Vec::new();
    for field in fields {
        let trait_name = map_trait(&field.suffix, &field.mutability);
        dyn_row_name += &format!("_{}_", trait_name);
        optics_trait.push(syn::Ident::new(trait_name, proc_macro2::Span::call_site()));
        fields_ty.push(field.field_type);
        fields_key.push(field.key);
    }

    (dyn_row_name, fields_key, fields_ty, optics_trait)
}

fn map_trait(suffix: &TypeSuffix, mutability: &Mutability) -> &'static str {
    match (suffix, mutability) {
        (TypeSuffix::Empty, Mutability::Ref(_)) => "LensRef",
        (TypeSuffix::Empty, Mutability::Mut(_)) => "LensMut",
        (TypeSuffix::Empty, Mutability::Move) => "Lens",
        (TypeSuffix::Star(_), Mutability::Ref(_)) => "TraversalRef",
        (TypeSuffix::Star(_), Mutability::Mut(_)) => "TraversalMut",
        (TypeSuffix::Star(_), Mutability::Move) => "Traversal",
        (TypeSuffix::Question(_), Mutability::Ref(_)) => "PrismRef",
        (TypeSuffix::Question(_), Mutability::Mut(_)) => "PrismMut",
        (TypeSuffix::Question(_), Mutability::Move) => "Prism",
    }
}

type RowMap = HashMap<String, Vec<syn::Ident>>;

struct DynRowCollector<'a>(&'a mut RowMap);

impl<'a> DynRowCollector<'a> {
    fn parse_dyn_row(&mut self, input: proc_macro2::TokenStream) {
        let row_type = syn::parse2::<RowType>(input).expect("dyn_row invalid");
        let fields: Vec<RowTypeField> = row_type.fields.into_iter().collect::<Vec<_>>();
        let (dyn_row_ident, _, _, optics_trait) = join_dyn_row_field(fields);
        self.0.entry(dyn_row_ident).or_insert(optics_trait);
    }
}

impl<'a> Visit<'_> for DynRowCollector<'a> {
    fn visit_macro(&mut self, mac: &syn::Macro) {
        syn::visit::visit_macro(self, mac);

        if mac.path.leading_colon.is_none() && mac.path.segments.len() == 1 {
            let seg = mac.path.segments.first().unwrap();
            if seg.arguments == syn::PathArguments::None && seg.ident == "dyn_row" {
                self.parse_dyn_row(mac.tokens.clone().into());
            }
        }
    }
}
