# Overview

`rovv` is a crate to provide a "poor-man's" row-polymorphism for rust base on [`lens-rs`](https://crates.io/crates/lens-rs).

## What is row poly?

> In programming language type theory, row polymorphism is a kind of polymorphism that allows one to write programs that are polymorphic on record field types (also known as rows, hence row polymorphism).
>
> -- wikipedia

Considering a function in PureScript:

```purescript
\r -> r.x
```

you can pass a record into the function above

```purescript
{ x: 1 }
```

or even

```purescript
{ x: 1, y: 2, z: 3 }
```

That is what "row-poly" means: a record can be passed into the function above as long as it contains a field `.x`.

The type of the function is:

```purescript
{ x: a | l } -> a
-- The label `l` represents the rest fields of a record.
```

Now you can do the same(not exactly) in rust.

## Usage

restrict the parameter `r` contains a field `.x`

```rust
fn take_x<T>(r: row! { x: T, .. }) -> T {
    r.view(optics!(x))
}

// &row! { x: T, .. } is ok
fn take_x_ref<T>(r: &row! { ref x: T, .. }) -> &T {
    r.view_ref(optics!(x))
}

// &mut row! { x: T, .. } is ok
fn take_x_mut<T>(r: &mut row! { mut x: T, .. }) -> &mut T {
    r.view_mut(optics!(x))
}

let foo = Foo { // have been derived Lens for Foo
    x: String::from("this is Foo"),
    y: 1234
}

let bar = Bar { // have been derived Lens for Bar
    x: 0,
    z: Some(1)
}

assert_eq!(&*take_x_ref(&foo), "this is Foo");
assert_eq!(take_x(bar), 0);
```

You can also describe a type *may* have a field:

```rust
fn or_take_y<T>(r: row! { y: T?, .. }) -> Option<T> {
    r.preview(optics!(y))
}

assert_eq!(or_take_y(foo), Some(1234));
assert_eq!(or_take_y(bar), None);
```


## Desugar

The function `take_x` is equivalent to

```rust
fn take_x<T, R>(r: R) -> T
where
    R: Lens<Optics![x], T>
{
    r.view(optics!(x))
}
```

In fact the `row! { .. }` will be desugared to the impl trait, the placeholder of a type satisfied the lens trait.
And the `dyn_row! { .. }` will be desugared to the dyn trait, the dynamic version of `row!`.

```rust
fn sum_field(n: i32) -> Box<dyn_row! { Ok: i32?, Err: String?, Some: ()? , ..}> {
    match n%3 {
        0 => Box::new(Result::<_, String>::Ok(0)),
        1 => Box::new(Result::<i32, _>::Err(String::from("no!"))),
        2 => Box::new(Some(())),
        _ => Box::new(Option::<()>::None)
    }
}
```

## Limitations

* Cannot pass the genreric arguments explicitly into the function when `row!` is used in argument position now,
  because the `row!` is the impl trait.
* `dyn_row!` will lose some polymorphism e.g. `dyn_row! { x: i32, .. }` does not satisfy `dyn_row! { ref x: i32, .. }`,
  because the trait object cannot convert to the others, thought Trait1: Trait2.
* Cannot move out of a field from a `dyn_row!`.

## Cargo.toml

Please add the following in your Cargo.toml

```toml
[dependencies]
lens-rs = "0.3"
rovv = "0.2"

[package.metadata.inwelling]
lens-rs_generator = true
rovv = true
```

Enjoy it!