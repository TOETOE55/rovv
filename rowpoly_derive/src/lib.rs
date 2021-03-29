use proc_macro2::Span;
use quote::*;
use std::collections::HashMap;
use std::fs;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::visit::Visit;
use syn::Token;
use syn::{parse_macro_input, parse_quote};

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

#[derive(Clone, Debug)]
struct LifetimeBounds {
    _lt_token: Token![<],
    lifetimes: Punctuated<syn::Lifetime, Token![,]>,
    _gt_token: Token![>],
}

/// row! { <'a> a: A, b: B, c: C, ..T }
#[derive(Clone, Debug)]
struct RowType {
    lifetime_bounds: Option<LifetimeBounds>,
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

impl Parse for LifetimeBounds {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _lt_token = input.parse()?;
        if input.peek(Token![>]) {
            return Ok(Self {
                _lt_token,
                lifetimes: Default::default(),
                _gt_token: input.parse()?,
            });
        }

        Ok(Self {
            _lt_token,
            lifetimes: Punctuated::parse_separated_nonempty(input)?,
            _gt_token: input.parse()?,
        })
    }
}

impl Parse for RowType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lifetime_bounds = if input.peek(Token![<]) {
            Some(input.parse()?)
        } else {
            None
        };

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
            lifetime_bounds,
            fields,
            _dot2token,
            mixin,
        })
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
        .chain(Some(quote! { rowpoly::Empty }))
        .chain(
            row_type
                .lifetime_bounds
                .into_iter()
                .flat_map(|bound| bound.lifetimes)
                .map(|lifetime| quote! { #lifetime }),
        )
        .collect::<Punctuated<proc_macro2::TokenStream, Token![+]>>();

    let impl_ty = row_type
        .mixin
        .map(|ty| quote! { impl #bound + std::ops::Deref<Output = #ty> + std::ops::DerefMut<Output = #ty> })
        .unwrap_or_else(|| quote! { impl #bound });

    // println!("{}", impl_ty.to_string());
    proc_macro::TokenStream::from(impl_ty)
}

#[proc_macro]
pub fn dyn_row(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let row_type = parse_macro_input!(input as RowType);
    let fields: Vec<RowTypeField> = row_type.fields.into_iter().collect::<Vec<_>>();
    let (dyn_row_name, _, fields_ty, _) = join_dyn_row_field(fields);
    let dyn_row_ident = syn::Ident::new(&dyn_row_name, Span::call_site());
    let lifetime_bounds = row_type
        .lifetime_bounds
        .map(|bounds| bounds.lifetimes)
        .unwrap_or_else(|| Punctuated::new())
        .into_iter()
        .collect::<Vec<_>>();

    proc_macro::TokenStream::from(quote! {
        dyn rowpoly::#dyn_row_ident<#(#fields_ty),*> #(+ #lifetime_bounds)*
    })
}

fn join_dyn_row_field(
    mut fields: Vec<RowTypeField>,
) -> (String, Vec<syn::Ident>, Vec<syn::Type>, Vec<syn::Ident>) {
    fields.sort_by_key(|field| field.field_name.clone());

    let mut dyn_row_name = "_dyn_row".to_string();
    let mut fields_name = Vec::new();
    let mut fields_ty = Vec::new();
    let mut optics_trait = Vec::new();
    for field in fields {
        let field_name = field.field_name;
        let optic_bound = match (field.suffix, field.mutability) {
            (TypeSuffix::Empty, Mutability::Ref(_)) => {
                dyn_row_name += &format!("_LensRef_{}", field_name.to_string());
                syn::Ident::new("LensRef", Span::call_site())
            }
            (TypeSuffix::Empty, Mutability::Mut(_)) => {
                dyn_row_name += &format!("_LensMut_{}", field_name.to_string());
                syn::Ident::new("LensMut", Span::call_site())
            }
            (TypeSuffix::Empty, Mutability::Move) => {
                dyn_row_name += &format!("_Lens_{}", field_name.to_string());
                syn::Ident::new("Lens", Span::call_site())
            }
            (TypeSuffix::Star(_), Mutability::Ref(_)) => {
                dyn_row_name += &format!("_TraversalRef_{}", field_name.to_string());
                syn::Ident::new("TraversalRef", Span::call_site())
            }
            (TypeSuffix::Star(_), Mutability::Mut(_)) => {
                dyn_row_name += &format!("_TraversalMut_{}", field_name.to_string());
                syn::Ident::new("TraversalMut", Span::call_site())
            }
            (TypeSuffix::Star(_), Mutability::Move) => {
                dyn_row_name += &format!("_Traversal_{}", field_name.to_string());
                syn::Ident::new("Traversal", Span::call_site())
            }
            (TypeSuffix::Question(_), Mutability::Ref(_)) => {
                dyn_row_name += &format!("_PrismRef_{}", field_name.to_string());
                syn::Ident::new("PrismRef", Span::call_site())
            }
            (TypeSuffix::Question(_), Mutability::Mut(_)) => {
                dyn_row_name += &format!("_PrismMut_{}", field_name.to_string());
                syn::Ident::new("PrismMut", Span::call_site())
            }
            (TypeSuffix::Question(_), Mutability::Move) => {
                dyn_row_name += &format!("_Prism_{}", field_name.to_string());
                syn::Ident::new("Prism", Span::call_site())
            }
        };
        fields_ty.push(field.field_type);
        fields_name.push(field_name);
        optics_trait.push(optic_bound);
    }

    (dyn_row_name, fields_name, fields_ty, optics_trait)
}

type RowMap = HashMap<String, (Vec<syn::Ident>, Vec<syn::Ident>)>;

struct DynRowCollector<'a>(&'a mut RowMap);

impl<'a> DynRowCollector<'a> {
    fn parse_dyn_row(&mut self, input: proc_macro::TokenStream) {
        let row_type = syn::parse::<RowType>(input).expect("dyn_row invalid");
        let fields: Vec<RowTypeField> = row_type.fields.into_iter().collect::<Vec<_>>();
        let (dyn_row_ident, fields_name, _, optics_trait) = join_dyn_row_field(fields);
        self.0
            .entry(dyn_row_ident)
            .or_insert((fields_name, optics_trait));
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

#[doc(hidden)]
#[proc_macro]
pub fn scan_dyn_row_from_source_files(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut iter = input.into_iter();
    let mut row_map = RowMap::new();

    loop {
        let token_tree = iter.next();
        match token_tree {
            Some(proc_macro::TokenTree::Literal(literal)) => {
                let file_name = literal.to_string();
                let file_name = file_name.trim_matches('"');
                let contents =
                    String::from_utf8(fs::read(std::path::Path::new(file_name)).unwrap()).unwrap();
                let syntax = syn::parse_file(&contents)
                    .expect(".rs files should contain valid Rust source code.");
                DynRowCollector(&mut row_map).visit_file(&syntax);

                if let Some(token_tree) = iter.next() {
                    if let proc_macro::TokenTree::Punct(punct) = token_tree {
                        if punct.to_string() != "," {
                            panic!(
                                "scan_dyn_row_from_source_files!(): expect `,`, got `{}`",
                                punct.to_string()
                            );
                        }
                    }
                } else {
                    break;
                }
            }
            None => break,
            _ => panic!(
                "scan_dyn_row_from_source_files!(): expect string literal, got `{:?}`",
                token_tree
            ),
        }
    }

    let mut trait_items = Vec::<syn::ItemTrait>::with_capacity(row_map.len());
    let mut impl_items = Vec::<syn::ItemImpl>::with_capacity(row_map.len());
    for (dyn_row_name, (fields_name, optics_trait)) in row_map {
        let generics = (0..fields_name.len())
            .map(|n| format_ident!("T{}", n))
            .collect::<Punctuated<_, Token![,]>>();
        let bounds = generics
            .iter()
            .zip(fields_name)
            .zip(optics_trait)
            .map(|((gen, name), optic)| (gen, name, optic))
            .map(|(gen, name, optic)| {
                quote! {
                    lens_rs::#optic<lens_rs::Optics![#name], Image = #gen>
                }
            })
            .collect::<Punctuated<_, Token![+]>>();
        let dyn_row_ident = syn::Ident::new(&dyn_row_name, Span::call_site());

        trait_items.push(parse_quote! {
            pub trait #dyn_row_ident<#generics>: #bounds {}
        });

        impl_items.push(parse_quote! {

            impl<T, #generics> #dyn_row_ident<#generics> for T
            where
                T: #bounds
            {}

        });
    }

    quote!(
        #(#trait_items)*
        #(#impl_items)*
    )
    .into()
}
