Similar to `update_counter/3`, but instead of failing with reason `badarg` if
there is no object with the specified `Key` in the table, the given default
object `Default` is used as the object to be updated. The value in place of
the key is ignored and replaced by the specified `Key`.

Aside from the situations listed for `update_counter/3`, this function
fails in the following additional situations:

- The default object arity is smaller than `<keypos>`.
- Any field from the default object that is updated is not an integer.
