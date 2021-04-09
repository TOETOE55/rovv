# rovv_derive

parse `row!` and `dyn_row!`, and transform them to
* `impl LensRef<Optic![a], A> + LensMut<Optic![b], B> + Lens<Optic![c], C>`
* `dyn LensRef<Optic![a], A> + LensMut<Optic![b], B> + Lens<Optic![c], C>`