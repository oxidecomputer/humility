// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use quote::quote_spanned;
use syn::spanned::Spanned;

#[proc_macro_derive(Load)]
pub fn load_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let ts = match &input.data {
        syn::Data::Struct(data) => match &data.fields {
            syn::Fields::Named(fields) => {
                gen_named_struct(&input.ident, fields)
            }
            syn::Fields::Unnamed(fields) => {
                gen_unnamed_struct(&input.ident, fields)
            }
            syn::Fields::Unit => {
                unimplemented!(
                    "unit struct not implemented as DWARF rep \
                                    not clear at time of writing"
                );
            }
        },
        syn::Data::Enum(data) => gen_enum(&input.ident, data),
        _ => unimplemented!("unsupported type for derive"),
    };

    proc_macro::TokenStream::from(ts)
}

fn gen_enum(
    ident: &syn::Ident,
    data: &syn::DataEnum,
) -> proc_macro2::TokenStream {
    let mut arms = vec![];
    for variant in &data.variants {
        let var_ident = &variant.ident;
        match &variant.fields {
            syn::Fields::Unit => {
                arms.push(quote_spanned!(variant.ident.span()=>
                    (stringify!(#var_ident), _) => Ok(Self::#var_ident),
                ));
            }
            syn::Fields::Unnamed(fields) => {
                let len = fields.unnamed.len();
                let field_lets = fields.unnamed.iter().enumerate().map(|(i, fld)| {
                    let name = syn::Ident::new(&format!("field_{}", i), fld.span());
                    let fty = &fld.ty;
                    quote_spanned!(fld.span()=>
                        let #name: #fty = humility::reflect::Load::from_value(&contents[#i])?;
                    )
                });
                let field_lets =
                    field_lets.collect::<proc_macro2::TokenStream>();

                let field_uses =
                    fields.unnamed.iter().enumerate().map(|(i, fld)| {
                        let name = syn::Ident::new(
                            &format!("field_{}", i),
                            fld.span(),
                        );
                        quote_spanned!(fld.span()=> #name,)
                    });
                let field_uses =
                    field_uses.collect::<proc_macro2::TokenStream>();

                arms.push(quote_spanned!(variant.ident.span()=>
                    (stringify!(#var_ident), Some(contents)) => {
                        let contents = contents.as_tuple()?;
                        if contents.len() != #len {
                            anyhow::bail!("wrong tuple size for {}: {:?}",
                                stringify!(#ident), contents);
                        }
                        #field_lets
                        Ok(Self::#var_ident(#field_uses))
                    }
                ));
            }
            syn::Fields::Named(fields) => {
                let field_name_strs = fields.named.iter().map(|fld| {
                    let name = fld.ident.as_ref().unwrap();
                    quote_spanned!(fld.span()=> stringify!(#name), )
                });
                let field_name_strs =
                    field_name_strs.collect::<proc_macro2::TokenStream>();

                let field_defs = fields.named.iter().map(|fld| {
                    let name = &fld.ident;
                    quote_spanned!(fld.span()=>
                        #name: humility::reflect::Load::from_value(&contents[stringify!(#name)])?,
                    )
                });
                let field_defs =
                    field_defs.collect::<proc_macro2::TokenStream>();

                arms.push(quote_spanned!(variant.ident.span()=>
                    (stringify!(#var_ident), Some(contents)) => {
                        let contents = contents.as_struct()?;
                        contents.check_members(&[#field_name_strs])?;

                        Ok(Self::#var_ident {
                            #field_defs
                        })
                    }
                ));
            }
        }
    }

    let arms = arms.into_iter().collect::<proc_macro2::TokenStream>();

    quote_spanned!(ident.span() =>
        impl humility::reflect::Load for #ident {
            fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
                let v = v.as_enum()?;
                match (v.disc(), v.contents()) {
                    #arms
                    _ => anyhow::bail!("unexpected shape for {}: {:?}",
                        stringify!(#ident), v),
                }
            }
        }
    )
}

fn gen_named_struct(
    ident: &syn::Ident,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_name_strs = fields.named.iter().map(|fld| {
        let name = fld.ident.as_ref().unwrap();
        quote_spanned!(fld.span()=> stringify!(#name), )
    });
    let field_name_strs = field_name_strs.collect::<proc_macro2::TokenStream>();

    let field_defs = fields.named.iter().map(|fld| {
        let name = &fld.ident;
        quote_spanned!(fld.span()=>
            #name: humility::reflect::Load::from_value(&v[stringify!(#name)])?,
        )
    });
    let field_defs = field_defs.collect::<proc_macro2::TokenStream>();

    quote_spanned!(ident.span()=>
        impl humility::reflect::Load for #ident {
            fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
                let v = v.as_struct()?;
                v.check_members(&[#field_name_strs])?;
                Ok(Self {
                    #field_defs
                })
            }
        }
    )
}

fn gen_unnamed_struct(
    ident: &syn::Ident,
    fields: &syn::FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let len = fields.unnamed.len();
    let field_lets = fields.unnamed.iter().enumerate().map(|(i, fld)| {
        let name = syn::Ident::new(&format!("field_{}", i), fld.span());
        let fty = &fld.ty;
        quote_spanned!(fld.span()=>
            let #name: #fty = humility::reflect::Load::from_value(&v[#i])?;
        )
    });
    let field_lets = field_lets.collect::<proc_macro2::TokenStream>();

    let field_uses = fields.unnamed.iter().enumerate().map(|(i, fld)| {
        let name = syn::Ident::new(&format!("field_{}", i), fld.span());
        quote_spanned!(fld.span()=> #name,)
    });
    let field_uses = field_uses.collect::<proc_macro2::TokenStream>();

    quote_spanned!(ident.span()=>
        impl humility::reflect::Load for #ident {
            fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
                let v = v.as_tuple()?;
                if v.len() != #len {
                    anyhow::bail!("wrong tuple size for {}: {:?}",
                        stringify!(#ident), v);
                }
                #field_lets
                Ok(Self(#field_uses))
            }
        }
    )
}
