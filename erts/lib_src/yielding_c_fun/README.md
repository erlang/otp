Yielding C Fun
==============

Introduction
------------

Yielding C Fun (YCF) is a tool that transforms functions written in a
subset of the C programming language so that they become yieldable. A
yieldable function can be suspended/yielded/paused/trapped (either
automatically or where the user has inserted a particular statement)
and then be resumed at a later point. Yileldable functions are also
called [coroutines](https://en.wikipedia.org/wiki/Coroutine).

Difference Between Yielding C Fun and Coroutine Libraries
---------------------------------------------------------

Several libraries implement [coroutine support for the C programming
language](https://en.wikipedia.org/wiki/Coroutine#Implementations_for_C)
(e.g., \[[1], [2], [3], [4], [5], [6], [7], [8], [9], [10], [11],
[12], [13]\]). These libraries either rely on platform-specific code
or do not save call stack variables. Yielding C Fun (YCF) does not
have any of these two limitations. YCF can accomplish this as it is a
source-to-source transformation tool and not only a library.

YCF has been created to make it easier to implement yielding Erlang
[NIFs](http://erlang.org/doc/tutorial/nif.html) and
[BIFs](http://erlang.org/pipermail/erlang-questions/2009-October/046899.html)
(i.e., Erlang functions that are written in C). Below are examples of
YCF features that are useful when implementing yielding Erlang NIFs
and BIFs:

 * YSF automatically generates a destroy function for each yieldable
   function. The destroy function frees resources that are used by a
   suspended function. The destroy function is useful when a suspended
   function needs to abort (e.g., when the Erlang process that invoked
   the function has died).

 * YCF can automatically insert code that yields functions after a
   user specifiable number of loop iterations and goto statements.

 * YCF has a hook system that lets the user insert code that is
   triggered when certain events happen (e.g., when a function
   yields).

The main limitations of YCF are that it cannot handle all valid C code
and that it cannot make library functions without source code
yieldable. Pointers to stack-allocated data are also not allowed (YCF
has a memory allocation function called `YCF_STACK_ALLOC` to work
around this issue).

Requirements
------------

* A C99 compatible C compiler
* make (optional but useful for building)

Compile and Test
----------------

Build the executable `$YCF_ROOT/bin/yielding_c_fun.bin`:

    make

Build the executable and run all tests:

    make test

Getting Started
---------------

A brief introduction tutorial can be found
[here](doc/thread_tutorial.md). This tutorial is a perfect place to
start!

The "[test/examples/](test/examples/)" folder in this repository
contains many small examples that are useful when learning about
YCF. YCF's automatic tests use these examples as well. The driver for
these tests is located in `test/test.sh`.

[This Erlang NIF example](test/examples/sha256_erlang_nif/) shows how
one can use YCF to write a yielding Erlang NIF library.


Command Line Parameters
-----------------------

```
Usage: yielding_c_fun [-h]
       yielding_c_fun [-use_gc [-print_gc_info]]
                      [-log_max_mem_usage log_file]
                      [(( -f | -frec | -fnoauto ) function_name)...
                       [-output_file_name output_file]
                       [-header_file_name header_file]
                       [-debug]
                       [-only_yielding_funs]
                       [-static_aux_funs]
                       input_c_file]]
```

* `-h`

  Print help text

* `-use_gc`

  Use garbage collection. The garbage collection system assumes that
  the C call stack consists of a continuous memory block and is
  therefore not enabled by default even though this assumption is
  valid on all major platforms. YCF does not reclaim any allocated
  memory if the `-use_gc` flag is not set.

* `-print_gc_info`

  (For debugging) Print garbage collection information to `stderr`

* `-log_max_mem_usage log_file`

  (For debugging) Print the peak memory usage of the tool to the file
  `log_file`

* `-fnoauto function_name`

  Generate a yieldable version of the function named
  function_name. The user can use `YCF_YIELD()`,
  `YCF_YIELD_NO_REDS()`, and `YCF_CONSUME_REDS(N)` to control
  when and where the function should yield. See the section titled
  "Special Statements and Macros" for more information.

* `-f function_name`

  Generate a yieldable version of the function named
  `function_name`. The generated function automatically decrements the
  reduction counter by one at the beginning of loop bodies and before
  goto statements. The function yields automatically if the reduction
  counter reaches a value that is zero or smaller after it has been
  decremented.

* `-frec function_name`

  Same as the -f option with the exception that the generated function
  also decrements one reduction before calls to other yieldable
  functions and before returning. The function yields automatically if
  the reduction counter reaches a value that is zero or smaller after
  it has been decremented.

* `-output_file_name output_file`

  Output the generated code to a file named output_file. The output
  is printed to standard output if this parameter is unspecified.

* `-header_file_name header_file`

  Generate a header file containing only declarations for the generated
  functions and write the header file to the file named header_file.

* `-debug`

  Generate debug code that executes when a function yields. The debug
  code searches the call stack of the yielding functions for pointers
  to data that is allocated on the call stack. The program crashes
  with an error message if any such pointer is found.

* `-only_yielding_funs`

  Print only the generated functions and struct declarations. The
  default behavior is to insert the generated functions into a copy of
  the input source file.

* `-static_aux_funs`

  Make the generated auxiliary functions static (i.e., local to the C
  compilation unit)

* `input_c_file`

  The source file containing the functions that YCF shall create
  yieldable versions of. YCF does not do any macro expansions. There
  are several restrictions on the code that YCF can handle that are
  described in the "Code Restrictions" section below.

Generated Functions
-------------------

YCF generates three functions for each function name that it is
given. These functions have the original function name as prefix and
different suffixes. Descriptions of the functions that YCF generates
follows below:


```c

/* Shall return a pointer to a memory block of size size bytes. */
typedef void* (*ycf_yield_alloc_type) (size_t size ,void* ctx);
/* Shall free the memory block which block points to. */
typedef void (*ycf_yield_free_type) (void* block,void* ctx);

return_type_of_orginal_fun
original_fun_name_ycf_gen_yielding(
               long * ycf_nr_of_reductions,
               void ** ycf_yield_state,
               void * ycf_extra_context,
               ycf_yield_alloc_type ycf_yield_alloc,
               ycf_yield_free_type ycf_yield_free,
               void * ycf_yield_alloc_free_context,
               size_t ycf_stack_alloc_size_or_max_size,
               void* ycf_stack_alloc_data
               paremeters_of_orginal_function);
```

The generated function with suffix `_ycf_gen_yielding` initiates the
call of a yieldable function. Its parameters and return types are
described below:

* `return_type_of_orginal_fun`

  The return type is the same as the return type of the original
  function. The return value is the return value of the function if
  the `_ycf_gen_yielding` function returns without yielding and is
  uninitialized otherwise.

* `long * ycf_nr_of_reductions`

  (input/output parameter) Gives the yieldable function the number of
  reductions it can consume before yielding and is also used to write
  back the number of reductions that are left when the function
  returns or yields.

* `void ** ycf_yield_state`

  (input/output parameter) Should be a pointer to a pointer to NULL
  when the `_ycf_gen_yielding` function is called. The value pointed
  to by ycf_yield_state is NULL when the `_ycf_gen_yielding` function
  has returned if it did not yield and points to the yield state
  otherwise.

* `void * ycf_extra_context`

  This parameter is useful if the yieldable function needs to access
  data that may change when it resumes after having been yielded. The
  extra context can be accessed from within the yieldable function
  with the `YCF_GET_EXTRA_CONTEXT()` function.

* `ycf_yield_alloc_type ycf_yield_alloc`

  A memory allocator function that is used by the yieldable function
  to allocate memory (e.g., to save the state when the function
  yields).

* `ycf_yield_free ycf_yield_free`

  A memory free function that should free a memory block that has been
  allocated with ycf_yield_alloc.

* `void * ycf_yield_alloc_free_context`

  A context that is passed as the second argument to `ycf_yield_alloc`
  and `ycf_yield_free`.

* `size_t ycf_stack_alloc_size_or_max_size`

  The max number of total bytes that can be allocated with the special
  allocator `YCF_STACK_ALLOC(n)`. This can be set to 0 if
  `YCF_STACK_ALLOC(n)` is unused. See the documentation of
  `YCF_STACK_ALLOC(n)` below for more information.

* `void* ycf_stack_alloc_data`

  A pointer to a data block that will be used by
  `YCF_STACK_ALLOC(n)`. The value of `ycf_stack_alloc_data` should be
  `NULL` or a pointer to a data block that is least
  `ycf_stack_alloc_size_or_max_size` bytes large if
  `YCF_STACK_ALLOC(n)` is used within the yieldable function or any
  yieldable function that is called by the yieldable function. The
  `ycf_yield_alloc` and `ycf_yield_free` functions will be used to
  automatically alloc and free a data block when needed, if
  `ycf_stack_alloc_data` is set to `NULL`. The value of
  `ycf_stack_alloc_data` does not matter if `YCF_STACK_ALLOC(n)` is
  unused.

* `paremeters_of_orginal_function`

  Parameters that the original function takes will be placed in the
  end of the parameter list of the `ycf_gen_yielding` function.


```c
return_type_of_orginal_fun
original_fun_name_ycf_gen_continue(
                     long * ycf_nr_of_reduction,
                     void ** ycf_yield_state,
                     void * ycf_extra_context);
```

The generated function with the suffix `_ycf_gen_continue` is used to
resume a yielded function. The descriptions of the parameters and
return type for the `_ycf_gen_yielding` function above are valid for
the `_ycf_gen_continue` function as well, with the exception that the
parameter `ycf_yield_state` should point to a pointer to a yield state
(created in the previous call to `_ycf_gen_yielding` or
`_ycf_gen_continue`).

```c
void original_fun_name_ycf_gen_destroy(void * ycf_yield_state);
```

The `_gen_destroy` function frees the state of a yieldable function
that has been suspended. Note that the parameter `ycf_yield_state`
points directly to the yield state, unlike the parameter of the
`_ycf_gen_yielding` and `_ycf_gen_continue` functions with the same
name.



The `YCF_YIELD_CODE_GENERATED` Macro
------------------------------------

YCF also generates code that defines the macro
`YCF_YIELD_CODE_GENERATED`. This macro may be useful if one wants to
compile one version of a program with yieldable functions and another
without yieldable functions.

Special Statements and Macros
-----------------------------

Some special statements and macros can be used from within a yieldable
function. Descriptions of those follow below:

* `YCF_YIELD();`

  The `YCF_YIELD();` statement sets the reduction counter to zero
  and yields the function when it is executed.

* `YCF_YIELD_NO_REDS();`

  The `YCF_YIELD_NO_REDS();` statement yields the function
  without changing the reduction counter when it is executed.

* `YCF_CONSUME_REDS(N);`

  The `YCF_CONSUME_REDS(N);` statement decrements the
  reductions counter by N and yields if the reduction counter is less
  than or equal to zero after the decrement.

* `YCF_STACK_ALLOC(N)`

  The `YCF_STACK_ALLOC(N)` macro uses an allocator that is included in
  the code generated by YCF to allocate a block with `N` bytes and
  return a pointer to these bytes. A block that has been allocated
  with `YCF_STACK_ALLOC(N)` is automatically freed when the function
  that allocated the block returns. Memory blocks that are allocated
  with `YCF_STACK_ALLOC(N)` do not move when a yieldable function
  yields and then resumes again. In contrast, data that is allocated
  directly on the call stack may move when a function yields and
  resumes. `YCF_STACK_ALLOC(N)` can thus be useful if one wants to
  port C code that has variables that point to data that is allocated
  on the call stack. The parameters `ycf_stack_alloc_size_or_max_size`
  and `ycf_stack_alloc_data` of the `_ycf_gen_yielding` function need
  to be set correctly if `YCF_STACK_ALLOC(N)` is used. Please see the
  description of the `_ycf_gen_yielding` function in the "Generated
  Functions" section above for details about those parameters. Notice
  also that the `-debug` flag that is described in the "Command Line
  Parameters" section above can be useful when one wants to find out
  if a function points to data that is allocated on the call stack of
  a yieldable function.

* `YCF_GET_EXTRA_CONTEXT()`

  The `YCF_GET_EXTRA_CONTEXT()` macro returns the value of the
  `ycf_extra_context` parameter that was passed to the latest call of
  one of the corresponding `_ycf_gen_yielding` or `_ycf_gen_continue`
  functions. See the "Generated Functions" section above for
  information about the parameters of `_ycf_gen_yielding` and
  `_ycf_gen_continue` functions.

* `YCF_NR_OF_REDS_LEFT()`

  The `YCF_NR_OF_REDS_LEFT()` macro returns the current value of
  the reduction counter (a value of type `long`).

* `YCF_SET_NR_OF_REDS_LEFT(NEW_NR_OF_REDS_LEFT)`

  The `YCF_SET_NR_OF_REDS_LEFT(NEW_NR_OF_REDS_LEFT)` macro sets
  the value that the reduction counter (which stores a value of type
  `long`) to `NEW_NR_OF_REDS_LEFT`.

* `YCF_MAX_NR_OF_REDS`

  The `YCF_MAX_NR_OF_REDS` macro returns the maximum value that the
  reduction counter may have.

Code Restrictions
-----------------

YCF cannot parse all valid C code. The code restrictions that
yieldable functions need to follow are described below. It is
recommended to check that the generated code is correct.

* **Declarations**

  Variable declarations and parameters of yieldable functions need to
  be in the following form:

  ```
  "(optional) type descriptor (i.e., struct, union or enum)"
  
  "type name"

  "(optional) one or more star characters (i.e., *)"

  "variable name"
  
  "(optional) one or more square brackets with a number inside (e.g, [3])"
  
  "(optional) one or more empty square brackets (e.g, [])"
  
  "(optional) equal sign followed by an expression (automatic array
  initialization and struct initialization of the form
  {.filed_name=value...} are not allowed)"

  "semicolon"
  ```
  
  Here are some examples of declarations that are **correct**:
  
  ```c
  int var1;
  int var2 = 1;
  int var3 = var2 + 1;
  int var4 = function(var3);
  int * var5 = malloc(sizeof(int*));
  int ** var6 = malloc(sizeof(int*)*10);
  int ***** var7;
  struct struct_name var8;
  struct struct_name var9 = function2();
  double var10[128];
  double var11[128][];
  double * var12[128];
  ```
  
  Here are examples of declarations that are **incorrect**:
  
  ```c
  int var1, var2;
  int var1 = 1, var2 = 10;
  void (*printer_t)(int);
  ```
  
  Note that one has to use a `typedef` to be able to declare a
  function pointer variable.

* **Pointers**

  Pointers to call-stack-allocated data are not allowed. The
  `YCF_YIELD_ALLOC(N)` function, which is described in the "Special
  Statements and Macros" section above, can be used to work around
  this limitation. The `-debug` flag that is described in the "Command
  Line Parameters" section above, can be useful when one wants to find
  out if a yieldable function points to call-stack-allocated data.


* **Macros**

  YCF does not expand macros so macros in functions that YCF
  transforms should not "hide" variables or any other code that is
  relevant for yielding.

* **Calls to a Yieldable Function from Another Yieldable Function**

  Calls to a yieldable function from another yieldable function need
  to be in a form that YCF recognizes. Such calls need to be in one of
  the following forms:

  * As a separate statement:
  
    Examples:
    ```c
    my_fun(my_param_expression + 1, 10);
    my_fun2();
    ```
  
  * A separate assignment statement to a variable. The function call
    expression may be negated but is not allowed to be nested in other
    types of expressions.
    
    Examples of **correct** ways of calling yieldable functions:
    ```c
    int var_name_1 = my_fun();
    int var_name_2 = !my_fun();
    var_name_3 = my_fun();
    var_name_4 = !my_fun();
    ```
    
    Examples of **incorrect** ways of calling yieldable functions:
    ```c
    int var_name_1 = (my_fun());
    var_name_2 = 1 + my_fun();
    t->name = my_fun();
    *ptr = my_fun();
    ```

  * As the expression of `while`-statements, `do-while`-statements or
    'if`-statements:

    Examples of **correct** ways of calling yieldable functions:
    ```c
    if(my_fun()) printf("hej\n");
    if(0) else if(my_fun()) printf("hej\n");
    while(!my_fun()) printf("hej\n");
    do { printf("hej\n"); } while(my_fun());
    var_name_3 = my_fun();
    var_name_4 = !my_fun();
    ```
    
    Examples of **incorrect** ways of calling yieldable functions:
    ```
    if(3+my_fun()) printf("hej\n");
    if(hej=my_fun()) printf("hej\n");
    ```
    
    YCF ignores calls to yieldable functions that are not in any of
    the forms described above, so it is important to check the
    generated source code.

Hooks
-----

It is possible to insert special hooks in yieldable functions. Hooks
execute when certain events are happening. Hooks may read and write to
variables (changes to variables are visible after the code block has
executed). Hooks can be placed anywhere one can place a normal
statement within the function body. There are two ways to write hooks:

**Hook Style 1:**

```c
  YCF_SPECIAL_CODE_START(ON_EVENT_NAME);
  printf("This will be printed when EVENT_NAME is happening\n");
  YCF_SPECIAL_CODE_END();
```

**Hook Style 2:**

```c
  /*special_code_start:ON_EVENT_NAME*/
  if(0){
    printf("This will be printed when EVENT_NAME is happening\n");
  }
  /*special_code_end*/
```

The following hook events are currently available:

* `ON_SAVE_YIELD_STATE`

  Triggered when the function yields.

* `ON_RESTORE_YIELD_STATE`

  Triggered before a function resumes after a yield.

* `ON_DESTROY_STATE`

  Triggered if and when the corresponding `_ycf_gen_destroy` function
  is executing.

* `ON_DESTROY_STATE_OR_RETURN`

  Triggered if and when the corresponding `_ycf_gen_destroy` function
  for the function is executing or when the function is returning.

* `ON_RETURN`

  Triggered when the function is returning.

License
-------

Yielding C Fun is released under the [Apache License
2.0](http://www.apache.org/licenses/LICENSE-2.0).


> Copyright Ericsson AB and Kjell Winblad 2019. All Rights Reserved.
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>     http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.



[1]: http://swtch.com/libtask/ "libtask"
[2]: http://xmailserver.org/libpcl.html
[3]: https://web.archive.org/web/20060110123338/http://www.goron.de/~froese/coro/
[4]: https://github.com/halayli/lthread
[5]: http://dekorte.com/projects/opensource/libcoroutine/
[6]: http://code.google.com/p/libconcurrency/libconcurrency
[7]: http://software.schmorp.de/pkg/libcoro.html
[8]: https://github.com/Adaptv/ribs2
[9]: http://libdill.org/
[10]: https://github.com/hnes/libaco
[11]: https://byuu.org/library/libco/
[12]: http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
[13]: https://github.com/jsseldenthuis/coroutine
