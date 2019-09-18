Simple C GC
===========

This is the readme file for Simple C GC. Simple C GC is a simple
garbage collection system for the C programming language inspired by
[Daniel Holden's article about Cello's garbage collection
system](http://libcello.org/learn/garbage-collection).

Usage
-----

The interface of Simple C GC consists of only two functions:

```C
/**
 * This function starts code that will be garbage collected
 *
 * @param main The function that will be started and garbage collected
 * @param argc Passed to main
 * @param argv Passed to main
 * @param my_malloc Simple C GC will use this to allocated memory
 * @param my_free Simple C GC will use this to free memory
 */
int scgc_start_gced_code(int (*main)(int, char *[]),
                         int argc,
                         char *argv[],
                         void* (*my_malloc)( size_t ),
                         void (*my_free)( void* ));

/**
 * Allocate a new memory block that will be garbage collected
 *
 * @param size The size of the new memory block
 */
void* scgc_new(size_t size);
```

An example that illustrates how these functions can be used in
practice is located in the `test.c` file.


Notes
-----

The garbage collector assumes that all data that should be garbage
collected is pointed to directly or indirectly from the "C stack" and
that the "C stack" is implemented as a continuous block of memory.

Compile and Test
----------------

    make test

License
-------

   Copyright 2019 Kjell Winblad (kjellwinblad@gmail.com, http://winsh.me)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.