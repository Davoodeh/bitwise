//! Holds `Bitwise*` derive variants.
//!
//! https://github.com/JelteF/derive_more/issues/197

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Fields,
    FieldsUnnamed, GenericArgument, Index, Path, PathArguments, Type, TypeArray, TypePath,
};

/// Implments bitwise boolean operations for custom unnamed structs.
///
/// ```rust
/// #[derive(BitwiseAnd)]
/// struct X(bool, bool, bool);
///
/// // The macro generates the code below (not only And, but also Xor and Or):
/// impl std::ops::BitAnd for X {
///     type Output = X;
///
///     fn bitand(self, rhs: Self) -> Self::Output {
///         Self(
///             self.0 & rhs.0,
///             self.1 & rhs.1,
///             self.2 & rhs.2,
///         )
///     }
/// }
///
/// impl std::ops::BitAndAssign for X {
///    fn bitand_assign(&mut self, rhs: Self) {
///        self.0 &= rhs.0;
///        self.1 &= rhs.1;
///        self.2 &= rhs.2;
///    }
/// }
/// ```
#[proc_macro_derive(BitwiseBool)]
pub fn bitwise_bool(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let fields = match input.data {
        Data::Struct(DataStruct {
            fields:
                Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields, ..
                }),
            ..
        }) => fields,
        _ => panic!("This macro only works on unnamed structs"),
    };

    // Check if all of the given unnamed types are boolean and if so make a
    // range to use in quote!.
    let mut range = Vec::<Index>::with_capacity(fields.len());
    for (i, v) in fields.into_iter().enumerate() {
        match v.ty {
            Type::Path(TypePath {
                path: Path { segments, .. },
                ..
            }) if segments.len() == 1 && segments[0].ident.to_string() == "bool".to_string() => {
                range.push(Index::from(i))
            }
            _ => panic!("All the arguments must be bool"),
        }
    }

    let tokens = quote! {
        impl std::ops::BitAnd for #ident {
            type Output = #ident;

            fn bitand(self, rhs: Self) -> Self::Output {
                Self(
                    #(self.#range & rhs.#range),*
                )
            }
        }

        impl std::ops::BitOr for #ident {
            type Output = #ident;

            fn bitor(self, rhs: Self) -> Self::Output {
                Self(
                    #(self.#range | rhs.#range),*
                )
            }
        }

        impl std::ops::BitXor for #ident {
            type Output = #ident;

            fn bitxor(self, rhs: Self) -> Self::Output {
                Self(
                    #(self.#range ^ rhs.#range),*
                )
            }
        }

        impl std::ops::BitAndAssign for #ident {
            fn bitand_assign(&mut self, rhs: Self) {
                #(self.#range &= rhs.#range);*
            }
        }

        impl std::ops::BitOrAssign for #ident {
            fn bitor_assign(&mut self, rhs: Self) {
                #(self.#range |= rhs.#range);*
            }
        }

        impl std::ops::BitXorAssign for #ident {
            fn bitxor_assign(&mut self, rhs: Self) {
                #(self.#range ^= rhs.#range);*
            }
        }
    };

    TokenStream::from(tokens)
}

/// `BitwiseBool` but with one unnamed vec instead of mutliple boolean fields.
///
/// The generate code is not exactly as simple as in `BitwiseBool`, however the
/// functionality remains exactly the same.
#[proc_macro_derive(BitwiseBoolVec)]
pub fn bitwise_bool_vec(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let fields = match input.data {
        Data::Struct(DataStruct {
            fields:
                Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields, ..
                }),
            ..
        }) => fields,
        _ => panic!("This macro only works on unnamed structs"),
    };

    // Check if the unnamed fields are just one Vec<bool> or panic
    let mut not_right_type = false;
    if fields.len() != 1 {
        not_right_type = true;
    } else {
        match &fields[0].ty {
            Type::Path(TypePath {
                path: Path { segments, .. },
                ..
            }) if segments.len() == 1 && segments[0].ident.to_string() == "Vec".to_owned() => {
                match &segments[0].arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => match &args[0] {
                        GenericArgument::Type(Type::Path(TypePath {
                            path: Path { segments, .. },
                            ..
                        })) if segments.len() == 1
                            && segments[0].ident.to_string() == "bool".to_owned() => {}
                        _ => not_right_type = true,
                    },
                    _ => not_right_type = true,
                }
            }
            _ => not_right_type = true,
        }
    }
    if not_right_type {
        panic!("Only one Vec<bool> is supported for `BitwiseBoolVec` fields.")
    }

    let make_min_max = quote! {
        let (min, max, max_is_self): (&mut Vec<bool>, &mut Vec<bool>, bool) =
            if self.0.len() < rhs.0.len() {
                (&mut self.0, &mut rhs.0, false)
            } else {
                (&mut rhs.0, &mut self.0, true)
            };
    };

    let return_max = quote! {
        if max_is_self {
            self
        } else {
            rhs
        }
    };
    let assign_max = quote! {
        if !max_is_self {
            self.0 = rhs.0;
        }
    };

    let and = quote! {
        #make_min_max
        for i in 0..min.len() {
            max[i] &= min[i];
        }
        for i in min.len()..max.len() {
            max[i] = false;
        }
    };
    let or = quote! {
        #make_min_max
        for i in 0..min.len() {
            max[i] |= min[i];
        }
    };
    let xor = quote! {
        #make_min_max
        for i in 0..min.len() {
            max[i] ^= min[i];
        }
        for i in min.len()..max.len() {
            max[i] ^= false;
        }
    };

    let tokens = quote! {
        impl std::ops::BitAnd for #ident {
            type Output = #ident;

            fn bitand(mut self, mut rhs: Self) -> Self::Output {
                #and
                #return_max
            }
        }

        impl std::ops::BitOr for #ident {
            type Output = #ident;

            fn bitor(mut self, mut rhs: Self) -> Self::Output {
                #or
                #return_max
            }
        }

        impl std::ops::BitXor for #ident {
            type Output = #ident;

            fn bitxor(mut self, mut rhs: Self) -> Self::Output {
                #xor
                #return_max
            }
        }

        impl std::ops::BitAndAssign for #ident {
            fn bitand_assign(&mut self, mut rhs: Self) {
                #and
                #assign_max
            }
        }

        impl std::ops::BitOrAssign for #ident {
            fn bitor_assign(&mut self, mut rhs: Self) {
                #or
                #assign_max
            }
        }

        impl std::ops::BitXorAssign for #ident {
            fn bitxor_assign(&mut self, mut rhs: Self) {
                #xor
                #assign_max
            }
        }
    };

    TokenStream::from(tokens)
}

/// `BitwiseBool` but with one unnamed array instead of mutliple boolean fields.
///
/// The generate code is not exactly as simple as in `BitwiseBool`, however the
/// functionality remains exactly the same.
#[proc_macro_derive(BitwiseBoolArray)]
pub fn bitwise_bool_array(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let fields = match input.data {
        Data::Struct(DataStruct {
            fields:
                Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields, ..
                }),
            ..
        }) => fields,
        _ => panic!("This macro only works on unnamed structs"),
    };

    // Check if the unnamed fields are just one Vec<bool> or panic
    let mut not_right_type = false;
    if fields.len() != 1 {
        not_right_type = true;
    } else {
        match &fields[0].ty {
            Type::Array(TypeArray { elem, .. }) => match &**elem {
                Type::Path(TypePath {
                    path: Path { segments, .. },
                    ..
                }) if segments.len() == 1 && segments[0].ident.to_string() == "bool".to_owned() => {
                }
                _ => not_right_type = true,
            },
            _ => not_right_type = true,
        }
    }
    if not_right_type {
        panic!("Only one [bool; N] is supported for `BitwiseBoolArray` fields.")
    }

    let tokens = quote! {
        impl std::ops::BitAnd for #ident {
            type Output = #ident;

            fn bitand(mut self, mut rhs: Self) -> Self::Output {
                for i in 0..self.0.len() {
                    self.0[i] &= rhs.0[i];
                }
                self
            }
        }

        impl std::ops::BitOr for #ident {
            type Output = #ident;

            fn bitor(mut self, mut rhs: Self) -> Self::Output {
                for i in 0..self.0.len() {
                    self.0[i] |= rhs.0[i];
                }
                self
            }
        }

        impl std::ops::BitXor for #ident {
            type Output = #ident;

            fn bitxor(mut self, mut rhs: Self) -> Self::Output {
                for i in 0..self.0.len() {
                    self.0[i] ^= rhs.0[i];
                }
                self
            }
        }

        impl std::ops::BitAndAssign for #ident {
            fn bitand_assign(&mut self, mut rhs: Self) {
                for i in 0..self.0.len() {
                    self.0[i] &= rhs.0[i];
                }
            }
        }

        impl std::ops::BitOrAssign for #ident {
            fn bitor_assign(&mut self, mut rhs: Self) {
                for i in 0..self.0.len() {
                    self.0[i] |= rhs.0[i];
                }
            }
        }

        impl std::ops::BitXorAssign for #ident {
            fn bitxor_assign(&mut self, mut rhs: Self) {
                for i in 0..self.0.len() {
                    self.0[i] ^= rhs.0[i];
                }
            }
        }
    };

    TokenStream::from(tokens)
}
