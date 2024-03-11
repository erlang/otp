Returns a list of all objects with key `Key` in table `Table`.

- For tables of type `set`, `bag`, or `duplicate_bag`, an object is returned
  only if the specified key _matches_ the key of the object in the table.
- For tables of type `ordered_set`, an object is returned if the specified key
  _compares equal_ to the key of an object in the table.

The difference is the same as between `=:=` and `==`.

As an example, one can insert an object with `t:integer/0` `1` as a key in an
`ordered_set` and get the object returned as a result of doing a
[`lookup/2`](`lookup/2`) with `t:float/0` `1.0` as the key to search for.

For tables of type `set` or `ordered_set`, the function returns either the empty
list or a list with one element, as there cannot be more than one object with
the same key. For tables of type `bag` or `duplicate_bag`, the function returns
a list of arbitrary length.

Notice that the sequential order of object insertions is preserved; the first
object inserted with the specified key is the first in the resulting list, and
so on. See also the note about
[list insertion order](`m:ets#insert_list_order`).
