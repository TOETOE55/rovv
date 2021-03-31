# rovv guide

Several days ago, I implemented a row type (a poor-man's row polymorphism) for Rust "by the way".

You could think of it as the syntax sugar for [`lens-rs`](https://github.com/TOETOE55/lens-rs).

In `lens-rs`, you can describe a type `T` with field `.a` as the example:

```rust

    fn field_a<T>(t: &T) -> &str
    where
        T: LensRef<Optics![a], Image = String>,
    {
        t.view_ref(optics!(a))
    }
    ​
    let foo = Foo {
        a: String::from("this is Foo"),
        b: 0,
    }
    ​
    let bar = Bar {
        a: String::from("this is Bar"),
        c: 1,
    }
    ​
    assert_eq!(field_a(&foo), "this is Foo");
    assert_eq!(field_a(&bar), "this is Bar");

```

Now you can rewrite it in `rovv`: 

```rust

    fn field_a<T>(r: &row! { ref a: String, ..}) -> &str {
        r.view_ref(optics!(a))
    }
    ​
    fn bar(s: &str) -> row! { a: &str, .. } {
        Foo {
            a: s,
            b: 1,
        }
    }

```

Macro `row!` has provided the following syntax:

```rust

    // multiple fields
    row! { a: A, b: B, c: C, .. }
    ​
    // describe the mutability of field
    row! {
        a: A,     // you can move `.a`，rovv has impl Lens<Optics![a], Image = A> for the row
        ref b: B, // can only borrow `.b` as immutable，rovv has impl LensRef<Optics![b], Image = B> for the row
        mut c: C, // can borrow `.c` as mutable，has impl LensMut<Optics![c], Image = C> for the row
        ..
    }
    ​
    // describe the number of the field
    row! {
        a: A?, // has 0 or 1 `.a`，has impl Prism<Optics![a], Image = A>
        ..     
    }
    ​
    row! {
        a: A*, // has many `.a`，has impl Traversal<Optics![a], Image = A>
        ..     
    }
    ​
    // row outlives 'a (and 'b)
    row! { <'a, 'b>
        a: &'a A,
        b: &mut 'b B,
        ..
    }

```

In addition to `row!` macro, the `dyn_row!` macro has been also provided with the same syntax:

```rust

    fn dyn_field_a<T>(r: &dyn_row! { ref a: String, ..}) -> &str {
        r.view_ref(optics!(a))
    }
    ​
    fn bar<'a>(s: &'a str) -> Box<dyn_row! { <'a> a: &'a str, .. }> {
        Box::new(Foo {
            a: s,
            b: 2,
        })
    }

```

The differences are:

* `row!` is the sugar of impl trait. (`impl Lens<Optics![a], Image = String> + ...`) 
* `dyn_row!` is the sugar of dyn trait. (`dyn Lens<Optics![a], Image = String> + ...`, some magic here) 

Don't forget to implement `Lens` for `Foo` and `Bar` before using `rovv`:

```rust

    #[derive(Copy, Clone, Debug, Optic, Lens)]
    struct Foo<A, B> {
        #[optic]
        a: A,
        #[optic]
        b: B,
    }
    ​
    #[derive(Clone, Debug, Optic, Lens)]
    struct Bar {
        #[optic]
        a: String,
        #[optic]
        c: i32,
    }

``` 

And don't forget add

```toml

    [package.metadata.inwelling]
    rovv = true

```

in Cargo.toml