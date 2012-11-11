### License needed

This library needs a license. [EPLv1](http://opensource.org/licenses/eclipse-1.0.php) is probably the way to go, since that seems to be the common license for most open source Clojure libraries and is the license for Clojure itself.

### Optional type mismatch checking for `plus` and `plus*`

In the same spirit as the return type checker, `plus` / `plus*` should optionally check whether the type of the first monadic value is the same type as the rest of the monadic values. The implementation is probably proper to `plus-step` and `plus-step*`, rather than `plus` and `plus*` *per se*. 

### Tests for monadic function return and monadic plus type checking

Need a small battery of tests demonstrating that exceptions are thrown as expected (i.e. for various mismatch scenarios) when type checking is activated.

### Should there be lazy "star versions" of `comprehend`, `seq`, `chain`?

Need to determine whether those monadic functions are "lazy enough" or whether lazy versions should be implemented in a manner similar to the `plus*` lazy variant of `plus`.

### Should transformer factories accept a flag indicating whether to use the "inner" or "outer" `plus-step` / `plus-step*`?

This is the way the algo.monads handles the ambiguity for certain transformers, whereas protocol-monads is presently hardwired to use the "inner" `plus-step` / `plus-step*` for the `list`, `vector` and `hash-set` transformers.

See:
* [Monadic plus for list transformer with set monad](https://gist.github.com/4050049)
* [Monadic plus for list transformer with list monad](https://gist.github.com/4050099)

### Transformers need implementations for `plus-step*`

Need to implement `plus-step*` for each monad transformer.

### Use sentinel `::lazy-seq` to indicate `lazy-seq` as monadic value factory function

At present, it's not possible to do `(set-t lazy-seq)` or similar since `lazy-seq` (and `monads.core/lazy-seq*`, `monads.core/lazy-seq`) is a macro. However, it should be possible to do something like `(def lazy ::lazy-seq)` in `monads.core`, and then have some logic in the transformer factories that checks whether the `mv-factory` argument equals `monads.core/lazy` and setup `do-result-m` to use `monads.core/lazy-seq*`.

### Should transformer factories perform `nil` -> `maybe-zero-val` short-circuit when `mv-factory` is `monads.core/maybe`?

This seems like a reasonable thing to do, as it provides parity with the `monads.core/maybe` factory function, but it's not entirely clear whether that's the "correct" thing to do.

### Duplicate monad and transformer description-comments in docstrings for the factory functions

The docstrings were uniformly moved into comments under the banners for the monads and transformers, but there should also be proper docstrings for the various factory functions. The "aliases" for the factory functions for `list`, `hash-set`, `vec`, `vector` and `lazy-seq` could be similarly augmented with docstrings by way of manually constructed metadata maps.

### Generalization using proxy classes and macros and maps?

There is enough repetition and uniformity among the various `extend-type` and `deftype` operations which define the macros and transformers such that it seems feasible to consider a generalization whereby the monads and transformers would be defined with usage of `proxy`, some additional macros, and `hash-map` formatting conventions (basically, a DSL for defining protocol-monads).

Such an implementation would cut allow for higher reuse of protocol method implementations, etc. and would also be more conducive to user-defined protocol-monads and transformers.

So the motivation is sound, but the idea is fairly speculative (would have to make an attempt and see how it goes); also, the implications for performance aren't clear at this time.

### `monads.core/do` macro should be adapted to provide more helpful exceptions

Per Jim's suggestion. See: [Clojure: Debugging Println, __LINE_NUMBER__ and __FILE_NAME__](http://stackoverflow.com/questions/10957257/clojure-debugging-println-line-number-and-file-name).

### Support `if-then-else` syntax/logic per the work of Roman Gonzalez

See: [Adding if-then-else statement for monadic environments](https://github.com/roman/protocol-monads/commit/9b708792e4679dcfc2b2345c2750458620fa720a).

Also stored in a branch of my fork: [roman-master-with-if-then-else](https://github.com/michaelsbradleyjr/protocol-monads/tree/roman-master-with-if-then-else).

### Port all the examples from algo.monads

The porting work was begun, but then put on hold while additional changes were being made to the library. The examples should be basically the same, but some adapted or additional ones should focus on the implications of optional type checking.

### Need a write-up documenting my changes

Put together a gist documenting my changes to the original library, and my motivations for those changes. Submit an issue for Jim's repo linking to the gist as a prelude to making a pull request.

### Remove algo.monads dependency when done porting examples

algo.monads was setup as a dependency to make it easier to play with and adapt the examples from algo.monads, but it should be removed at some point.

### Need a branch for the purpose of publishing my fork on Clojars

The branch would have a `project.clj` suited to publishing my fork on Clojars.

### Additional monads and transfomers

There has already been a suggestion-request for the [Reader monad](http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-Reader.html) in the `#clojure` IRC channel on Freenode.

Additional monads and transformers can be looked at and ported as time allows and need arises. Hopefully, some motivated users of the library can contribute such additions.

### Documentation

Web-based documentation for the library's API needs to be generated, i.e. in the usual manner with [autodoc](http://tomfaulhaber.github.com/autodoc/) or something similar.

It would also be good to develop a "manual" or "tutorial" which walks through all the examples while giving further guidance and insights as to the library's usage.

Another document is envisioned, which would run along similar lines to Jim Duey's and Konrad Hinsen's monad tutorials, but couched in terms which demonstrate how the protocol-monads library is built up from the basic monad concepts (`bind`, `result`, `do`, `plus`, etc.) and by leveraging Java/Clojure types and Clojure's protocols.
