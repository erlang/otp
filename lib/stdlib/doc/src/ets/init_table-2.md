Replaces the existing objects of table `Table` with objects created by calling
the input function `InitFun`, see below. This function is provided for
compatibility with the `dets` module, it is not more efficient than filling a
table by using `insert/2`.

When called with argument `read`, the function `InitFun` is assumed to return
`end_of_input` when there is no more input, or `{Objects, Fun}`, where `Objects`
is a list of objects and `Fun` is a new input function. Any other value `Value`
is returned as an error `{error, {init_fun, Value}}`. Each input function is
called exactly once, and if an error occur, the last function is called with
argument `close`, the reply of which is ignored.

If the table type is `set` and more than one object exists with a given key, one
of the objects is chosen. This is not necessarily the last object with the given
key in the sequence of objects returned by the input functions. This holds also
for duplicated objects stored in tables of type `bag`.
