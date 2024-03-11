Returns information about the table dumped to file by `tab2file/2` or
`tab2file/3`.

The following items are returned:

- **`name`** - The name of the dumped table. If the table was a named table, a
  table with the same name cannot exist when the table is loaded from file with
  `file2tab/2`. If the table is not saved as a named table, this field has no
  significance when loading the table from file.

- **`type`** - The ETS type of the dumped table (that is, `set`, `bag`,
  `duplicate_bag`, or `ordered_set`). This type is used when loading the table
  again.

- **`protection`** - The protection of the dumped table (that is, `private`,
  `protected`, or `public`). A table loaded from the file gets the same
  protection.

- **`named_table`** - `true` if the table was a named table when dumped to file,
  otherwise `false`. Notice that when a named table is loaded from a file, there
  cannot exist a table in the system with the same name.

- **`keypos`** - The `keypos` of the table dumped to file, which is used when
  loading the table again.

- **`size`** - The number of objects in the table when the table dump to file
  started. For a `public` table, this number does not need to correspond to the
  number of objects saved to the file, as objects can have been added or deleted
  by another process during table dump.

- **`extended_info`** - The extended information written in the file footer to
  allow stronger verification during table loading from file, as specified to
  `tab2file/3`. Notice that this function only tells _which_ information is
  present, not the values in the file footer. The value is a list containing one
  or more of the atoms `object_count` and `md5sum`.

- **`version`** - A tuple `{Major,Minor}` containing the major and minor version
  of the file format for ETS table dumps. This version field was added beginning
  with STDLIB 1.5.1. Files dumped with older versions return `{0,0}` in this
  field.

An error is returned if the file is inaccessible, badly damaged, or not produced
with `tab2file/2` or `tab2file/3`.
