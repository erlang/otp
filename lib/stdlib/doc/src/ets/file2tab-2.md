Reads a file produced by `tab2file/2` or `tab2file/3` and creates the
corresponding table `Table`.

The only supported option is `{verify,boolean()}`. If verification is turned on
(by specifying `{verify,true}`), the function uses whatever information is
present in the file to assert that the information is not damaged. How this is
done depends on which `extended_info` was written using `tab2file/3`.

If no `extended_info` is present in the file and `{verify,true}` is specified,
the number of objects written is compared to the size of the original table when
the dump was started. This can make verification fail if the table was `public`
and objects were added or removed while the table was dumped to file. To avoid
this problem, either do not verify files dumped while updated simultaneously or
use option `{extended_info, [object_count]}` to `tab2file/3`, which extends the
information in the file with the number of objects written.

If verification is turned on and the file was written with option
`{extended_info, [md5sum]}`, reading the file is slower and consumes radically
more CPU time than otherwise.

`{verify,false}` is the default.
