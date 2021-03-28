extern crate proc_macro;

use quote::*;
use syn::{
    parenthesized, parse_macro_input, parse_quote, visit::Visit, Data, DeriveInput, Fields,
    ItemEnum, ItemStruct, Token,
};
use syn::punctuated::Punctuated;
use syn::parse::{Parse, ParseStream};
use proc_macro2::TokenStream;

#[derive(Clone, Debug)]
enum TypeSuffix {
    Empty,
    Star(Token![*]),
    Question(Token![?]),
}
/// a: A, b: B?, c: C*
#[derive(Clone, Debug)]
struct RowField {
    field_name: syn::Ident,
    _colon_token: Token![:],
    field_type: syn::Type,
    suffix: TypeSuffix,
}

///  ..T
#[derive(Clone, Debug)]
struct Mixin {
    _dot2_token: Token![..],
    mixin_type: syn::Type,
}

/// Row! { a: A, b: B?, c: C*, ..T }
#[derive(Clone, Debug)]
struct RowType {
    fields: Punctuated<RowField, Token![,]>,
    mixin: Option<Mixin>
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

impl Parse for RowField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            field_name: input.parse()?,
            _colon_token: input.parse()?,
            field_type: input.parse()?,
            suffix: input.parse()?
        })
    }
}

impl Parse for Mixin {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            _dot2_token: input.parse()?,
            mixin_type: input.parse()?
        })
    }
}

impl Parse for RowType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut fields = Punctuated::new();
        while !input.is_empty() && !input.peek(Token![..]) {
            let row_field = input.call(RowField::parse)?;
            fields.push_value(row_field);
            if input.is_empty() {
                break;
            }
            let punct: Token![,] = input.parse()?;
            fields.push_punct(punct);
        }

        let mixin = if fields.empty_or_trailing() && input.peek(Token![..]) {
            Some(Mixin {
                _dot2_token: input.parse()?,
                mixin_type: input.parse()?
            })
        } else {
            None
        };

        Ok(Self {
            fields,
            mixin
        })
    }
}

impl ToTokens for TypeSuffix {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TypeSuffix::Empty => {},
            TypeSuffix::Star(s) => s.to_tokens(tokens),
            TypeSuffix::Question(q) => q.to_tokens(tokens)
        }
    }
}

impl ToTokens for RowField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.field_name.to_tokens(tokens);
        self._colon_token.to_tokens(tokens);
        self.field_type.to_tokens(tokens);
        self.suffix.to_tokens(tokens);
    }
}

impl ToTokens for Mixin {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self._dot2_token.to_tokens(tokens);
        self.mixin_type.to_tokens(tokens);
    }
}

impl ToTokens for RowType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.fields.to_tokens(tokens);
        self.mixin.to_tokens(tokens);
    }
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn Row(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let row_type = parse_macro_input!(input as RowType);
    proc_macro::TokenStream::from(quote! {})
}

