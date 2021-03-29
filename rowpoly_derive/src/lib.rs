use proc_macro2::TokenStream;
use quote::*;
use syn::parse::{Parse, ParseStream};
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::Token;

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

/// ref a: A?, mut b: B*, c: C
#[derive(Clone, Debug)]
struct RowTypeField {
    mutability: Mutability,
    field_name: syn::Ident,
    _colon_token: Token![:],
    field_type: syn::Type,
    suffix: TypeSuffix,
}

/// row! { a: A, b: B, c: C, ..T }
#[derive(Clone, Debug)]
struct RowType {
    fields: Punctuated<RowTypeField, Token![,]>,
    _dot2token: Token![..],
    mixin: Option<syn::Type>,
}

impl Parse for Mutability {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![ref]) {
            Ok(Mutability::Ref(input.parse()?))
        } else if lookahead.peek(Token![mut]) {
            Ok(Mutability::Mut(input.parse()?))
        } else if lookahead.peek(syn::Ident) {
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

impl Parse for RowTypeField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            mutability: input.parse()?,
            field_name: input.parse()?,
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

        let mixin = if input.peek(syn::token::Type) {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self {
            fields,
            _dot2token,
            mixin,
        })
    }
}

impl ToTokens for Mutability {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Mutability::Ref(tok) => tok.to_tokens(tokens),
            Mutability::Mut(tok) => tok.to_tokens(tokens),
            Mutability::Move => {}
        }
    }
}

impl ToTokens for RowTypeField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.field_name.to_tokens(tokens);
        self._colon_token.to_tokens(tokens);
        self.field_type.to_tokens(tokens);
    }
}

impl ToTokens for RowType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.fields.to_tokens(tokens);
    }
}

#[proc_macro]
pub fn row(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let row_type = parse_macro_input!(input as RowType);
    let bound = row_type
        .fields
        .into_iter()
        .map(|x: RowTypeField| {
            let field_name = x.field_name;
            let field_type = x.field_type;
            match (x.suffix, x.mutability) {
                (TypeSuffix::Empty, Mutability::Ref(_)) => {
                    quote! { lens_rs:: LensRef<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Empty, Mutability::Mut(_)) => {
                    quote! { lens_rs:: LensMut<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Empty, Mutability::Move) => {
                    quote! { lens_rs:: Lens<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Star(_), Mutability::Ref(_)) => {
                    quote! { lens_rs:: TraversalRef<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Star(_), Mutability::Mut(_)) => {
                    quote! { lens_rs:: TraversalMut<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Star(_), Mutability::Move) => {
                    quote! { lens_rs:: Traversal<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Question(_), Mutability::Ref(_)) => {
                    quote! { lens_rs:: PrismRef<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Question(_), Mutability::Mut(_)) => {
                    quote! { lens_rs:: PrismMut<Optics![#field_name], Image = #field_type> }
                }
                (TypeSuffix::Question(_), Mutability::Move) => {
                    quote! { lens_rs:: Prism<Optics![#field_name], Image = #field_type> }
                }
            }
        })
        .collect::<Punctuated<proc_macro2::TokenStream, Token![+]>>();

    let impl_ty = row_type
        .mixin
        .map(|ty| quote! { impl #bound + std::ops::Deref<Output = #ty> + std::ops::DerefMut<Output = #ty> })
        .unwrap_or_else(|| quote! { impl #bound });
    proc_macro::TokenStream::from(impl_ty)
}
