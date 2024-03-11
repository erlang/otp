Similar to `update_element/3`, but instead of returning `false` if
there is no object with the specified `Key` in the table, the given default
object `Default` is used as the object to be updated. The value in place of
the key is ignored and replaced by the specified `Key`.

Aside from the situations listed for `update_element/3`, this function
fails in the following additional situations:

- The default object arity is smaller than `<keypos>`.
- The element to update is also the key.
