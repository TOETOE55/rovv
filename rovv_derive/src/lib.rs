use proc_macro2::Span;
use quote::*;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Token,
};

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

/// transform
///
/// ```rust
/// row! { ref a: A, mut b: B, c: C, .. : Trait1 + Trait2 + 'a }
/// ```
///
/// to
///
/// ```rust
/// impl LensRef<Optic![a], A> + LensMut<Optic![b], B> + Lens<Optic![c], C> + Trait1 + Trait2 + 'a
/// ```
#[proc_macro]
pub fn row(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let row_type = parse_macro_input!(input as RowType);
    let fields: Vec<RowTypeField> = row_type.fields.into_iter().collect::<Vec<_>>();
    let (row_name, key, fields_ty, _) = join_row_field(fields);
    let row_ident = syn::Ident::new(&row_name, Span::call_site());
    let bounds = row_type.bounds.into_iter().collect::<Vec<_>>();

    proc_macro::TokenStream::from(quote! {
        impl rovv::#row_ident<#(#key, #fields_ty),*> #(+ #bounds)*
    })
}

/// transform
///
/// ```rust
/// dyn_row! { ref a: A, mut b: B, c: C, .. : Trait1 + Trait2 + 'a }
/// ```
///
/// to
///
/// ```rust
/// dyn LensRef<Optic![a],  A> + LensMut<Optic![b], B> + Lens<Optic![c], C> + Trait1 + Trait2 + 'a
/// ```
#[proc_macro]
pub fn dyn_row(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let row_type = parse_macro_input!(input as RowType);
    let fields: Vec<RowTypeField> = row_type.fields.into_iter().collect::<Vec<_>>();
    let (row_name, key, fields_ty, _) = join_row_field(fields);
    let row_ident = syn::Ident::new(&row_name, Span::call_site());
    let bounds = row_type.bounds.into_iter().collect::<Vec<_>>();

    proc_macro::TokenStream::from(quote! {
        dyn rovv::#row_ident<#(#key, #fields_ty),*> #(+ #bounds)*
    })
}

fn join_row_field(
    mut fields: Vec<RowTypeField>,
) -> (String, Vec<Key>, Vec<syn::Type>, Vec<syn::Ident>) {
    fields.sort_by_key(|field| map_trait(&field.suffix, &field.mutability));

    let mut row_name = "_row".to_string();
    let mut fields_key = Vec::new();
    let mut fields_ty = Vec::new();
    let mut optics_trait = Vec::new();
    for field in fields {
        let trait_name = map_trait(&field.suffix, &field.mutability);
        row_name += &format!("_{}_", trait_name);
        optics_trait.push(syn::Ident::new(trait_name, Span::call_site()));
        fields_ty.push(field.field_type);
        fields_key.push(field.key);
    }

    (row_name, fields_key, fields_ty, optics_trait)
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
