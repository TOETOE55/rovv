#[cfg(test)]
mod tests {
    use lens_rs::*;
    use rowpoly::*;
    // derive struct
    #[derive(Copy, Clone, Debug, Optic, Lens)]
    struct Foo<A, B> {
        #[optic]
        a: A,
        #[optic]
        b: B,
    }

    #[derive(Clone, Debug, Optic, Lens)]
    struct Bar {
        #[optic]
        a: String,
        #[optic]
        c: i32,
    }

    fn with_many_string(t: &row! { _mapped: String*, .. }) -> Vec<&str> {
        t.traverse_ref(optics!(_mapped))
            .into_iter()
            .map(|s| s.as_ref())
            .collect()
    }

    fn may_with_field_some(t: &row! { Some: String?, .. }) -> Option<&str> {
        t.preview_ref(optics!(Some)).map(|s| s.as_ref())
    }

    fn with_field_a(t: &row! { a: String, .. }) -> &str {
        t.view_ref(optics!(a))
    }

    fn to_field_a() -> row! { a: String, .. } {
        Bar {
            a: "this is Bar".to_string(),
            c: 0,
        }
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

        assert_eq!(with_field_a(&foo), "this is Foo");
        assert_eq!(with_field_a(&bar), "this is Bar");

        assert_eq!(to_field_a().view_ref(optics!(a)), "this is Bar");

        assert_eq!(
            may_with_field_some(&Some("this is Some".to_string())),
            Some("this is Some")
        );
        assert_eq!(may_with_field_some(&Option::<String>::None), None);

        assert_eq!(
            with_many_string(&vec!["this is vec".to_string()]),
            vec!["this is vec"]
        );
    }
}
