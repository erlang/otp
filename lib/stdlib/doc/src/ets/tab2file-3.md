Dumps table `Table` to file `Filename`.

When dumping the table, some information about the table is dumped to a header
at the beginning of the dump. This information contains data about the table
type, name, protection, size, version, and if it is a named table. It also
contains notes about what extended information is added to the file, which can
be a count of the objects in the file or a MD5 sum of the header and records in
the file.

The size field in the header might not correspond to the number of records in
the file if the table is public and records are added or removed from the table
during dumping. Public tables updated during dump, and that one wants to verify
when reading, needs at least one field of extended information for the read
verification process to be reliable later.

Option `extended_info` specifies what extra information is written to the table
dump:

- **`object_count`** - The number of objects written to the file is noted in the
  file footer, so file truncation can be verified even if the file was updated
  during dump.

- **`md5sum`** - The header and objects in the file are checksummed using the
  built-in MD5 functions. The MD5 sum of all objects is written in the file
  footer, so that verification while reading detects the slightest bitflip in
  the file data. Using this costs a fair amount of CPU time.

Whenever option `extended_info` is used, it results in a file not readable by
versions of ETS before that in STDLIB 1.15.1

If option `sync` is set to `true`, it ensures that the content of the file is
written to the disk before `tab2file` returns. Defaults to `{sync, false}`.
