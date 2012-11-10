### License needed

This library needs a license. EPLv1 is probably the way to go, since that seems to be the common license for most open source Clojure libraries and is the license for Clojure itself.

### Optional type mismatch checking for `plus` and `plus*`

In the same spirit as the return type checker, `plus` / `plus*` should optionally check whether the type of the first monadic value is the same type as the rest of the monadic values. The implementation is probably proper to `plus-step` and `plus-step*`, rather than `plus` and `plus*` *per se*. 

### Tests for monadic function return and monadic plus type checking

Need a small battery of tests demonstrating that exceptions are thrown as expected (i.e. for various mismatch scenarios) when type checking is activated.

### Should there be lazy "star versions" of `comprehend`, `seq`, `chain`?

Need to determine whether those monadic functions are "lazy enough" or whether lazy versions should be implemented in a manner similar to the `plus*` lazy variant of `plus`.

### Should transformer factories accept a flag indicating whether to use the "inner" or "outer" `plus-step` / `plus-step*`?

This is the way the algo.monads handles the ambiguity for certain transformers, whereas protocol-monads is presently hardwired to use the "inner" `plus-step` / `plus-step*` for the `list`, `vector` and `hash-set` transformers.

See: [https://gist.github.com/4050049](https://gist.github.com/4050049) [https://gist.github.com/4050099](https://gist.github.com/4050099).

### Transformers need implementations for `plus-step*`

Need to implement `plus-step*` for each monad transformer.

### Use sentinel `::lazy-seq` to indicate `lazy-seq` as monadic value factory function

At present, it's not possible to do `(set-t lazy-seq)` since `lazy-seq` (and `monads.core/lazy-seq*`, `monads.core/lazy-seq`) is a macro. However, it should be possible to do something like `(def lazy ::lazy-seq)` in `monads.core`, and then have some logic in the transformer factories that checks whether the `mv-factory` argument equals `monads.core/lazy` and setup `do-result-m` to use `monads.core/lazy-seq*`.

### Should transformer factories perform `nil` -> `maybe-zero-val` short-circuit when `mv-factory` is `monads.core/maybe`?

This seems like a reasonable thing to do, as it provides parity with the `monads.core/maybe` factory function, but it's not entirely clear whether that's the "correct" thing to do.
