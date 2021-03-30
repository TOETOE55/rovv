# rovv_derive

parse `row!` and `dyn_row!`, and transform them to
* `impl LensRef<Optic![a], Image = A> + LensMut<Optic![b], Image = B> + Lens<Optic![c], Image = C> + 'a`
* `dyn LensRef<Optic![a], Image = A> + LensMut<Optic![b], Image = B> + Lens<Optic![c], Image = C> + 'a`