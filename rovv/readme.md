# rovv

`rovv` is a crate to provide anonymous row type, which fields can be access by [`lens-rs`](https://github.com/TOETOE55/lens-rs)

## Example

A row with field `a: String`

```rust
fn with_field_a(r: row! { a: String, .. }) -> String {
    r.view(optics!(a))
}

fn to_field_a() -> row! { a: String, .. } {
    Bar {
        a: "this is Bar".to_string(),
        c: 0,
    }
}

let foo = Foo {
    a: "this is Foo".to_string(),
    b: (),
};
let bar = Bar {
    a: "this is Bar".to_string(),
    c: 0,
};

assert_eq!(with_field_a(foo.clone()).as_str(), "this is Foo");
assert_eq!(with_field_a(bar.clone()).as_str(), "this is Bar");
```

A row may have field `Some: String`

```rust
row! { Some: String?, .. }
dyn_row! { Some: String?, .. } // dynamic version of row!
```

A row have multiple fields `_mapped: String`

```rust
row! { _mapped: String*, .. }
dyn_row! { _mapped: String*, .. }
```

A row with a mutable or immutable field: 

```rust
row! { ref a: A, mut b: B, .. }
dyn_row! { ref a: A, mut b: B, .. }
```

A row bound by lifetimes `'a`

```rust
row! { <'a> ref a: A, mut b: B, .. }
dyn_row! { <'a> ref a: A, mut b: B, .. }
```

In fact, 

* `row! { <'a> ref a: A, mut b: B, c: C, .. }` is `impl LensRef<Optic![a], Image = A> + LensMut<Optic![b], Image = B> + Lens<Optic![c], Image = C> + 'a`
* `dyn_row! { <'a> ref a: A, mut b: B, c: C, .. }` is `dyn LensRef<Optic![a], Image = A> + LensMut<Optic![b], Image = B> + Lens<Optic![c], Image = C> + 'a`

There are some magic in `dyn_row!`, so please don't forget
```toml
[package.metadata.inwelling]
rovv = true
```
in `Cargo.toml`