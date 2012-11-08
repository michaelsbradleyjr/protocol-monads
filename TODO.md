### License needed

This library needs a license. EPLv1 is probably the way to go, since that seems to be the common license for most open source Clojure libraries and is the license for Clojure itself.

### Optional type mismatch checking for `plus` and `plus*`

In the same spirit as the return type checker, `plus` / `plus*` should optionally check whether the type of the first monadic value is the same as the types of the rest of the monadic values. The implementation is probably proper to `plus-step` and `plus-step*`, rather than `plus` and `plus*` *per se*. 

### Should there be lazy "star versions" of `comprehend`, `seq`, `chain`?

Need to determine whether those monadic functions are "lazy enough" or whether lazy versions should be implemented in a manner similar to the `plus*` lazy variant of `plus`.
