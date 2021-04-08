#[cfg(test)]
mod tests {
    use lens_rs::*;
    // use structx::*;
    use rovv::*;
    use std::ops::Deref;

    #[derive(Copy, Clone, Debug, Lens)]
    struct Foo<A, B> {
        #[optic]
        a: A,
        #[optic]
        b: B,
    }

    #[derive(Clone, Debug, Lens)]
    struct Bar {
        #[optic]
        a: String,
        #[optic]
        c: i32,
    }

    fn with_field_a(t: row! { a: String, .. }) -> String {
        t.view(optics!(a))
    }

    fn with_field_ref_a(t: &row! { ref a: String, .. : ?Sized }) -> &str {
        t.view_ref(optics!(a))
    }

    fn _with_field_mut_a(t: &mut row! { mut a: String, .. }) {
        *t.view_mut(optics!(a)) += "suffix";
    }

    fn dyn_with_field_ref_a(r: &dyn_row! { a: String, .. }) -> &str {
        r.view_ref(optics!(a))
    }

    fn to_field_a() -> row! { a: String, .. } {
        Bar {
            a: "this is Bar".to_string(),
            c: 1,
        }
    }

    fn to_dyn_field_a() -> Box<dyn_row! { a: String, .. }> {
        Box::new(Foo {
            a: "this is Foo".to_string(),
            b: Some(0),
        })
    }

    fn may_with_field_c(t: &row! { c: i32?, .. }) -> Option<i32> {
        Some(*t.preview_ref(optics!(c))?)
    }

    fn row_with_bound(r: &row! { a: String, .. : Clone }) -> row! { a: String, .. : Clone } {
        r.clone()
    }

    // function foo<Type>(key: keyof Type, r: { [K in keyof Type]: number }): number
    fn row_keyof<K, V, Type: Lens<K, V>>(_: &Type, key: K, r: &row! { [K]: i32, .. }) -> i32 {
        *r.view_ref(key)
    }

    #[test]
    fn test_row() {
        let foo = Foo {
            a: "this is Foo".to_string(),
            b: (),
        };
        let bar = Bar {
            a: "this is Bar".to_string(),
            c: 0,
        };

        assert_eq!(&*with_field_a(foo.clone()), "this is Foo");
        assert_eq!(&*with_field_a(bar.clone()), "this is Bar");

        assert_eq!(with_field_ref_a(&foo), "this is Foo");
        assert_eq!(with_field_ref_a(&bar), "this is Bar");

        assert_eq!(may_with_field_c(&bar), Some(0));
        assert_eq!(may_with_field_c(&foo), None);

        assert_eq!(*row_with_bound(&foo).view_ref(optics!(a)), "this is Foo");

        assert_eq!(&*with_field_a(to_field_a()), "this is Bar");
        assert_eq!(with_field_ref_a(to_dyn_field_a().deref()), "this is Foo");
        assert_eq!(dyn_with_field_ref_a(to_dyn_field_a().deref()), "this is Foo");

        assert_eq!(row_keyof(&foo, optics!(a), &Foo { a: 1, b: () }), 1);
    }
}
