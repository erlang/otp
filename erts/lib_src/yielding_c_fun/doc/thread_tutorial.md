Yielding C Fun Thread Example Tutorial
======================================

This tutorial goes through how Yielding C Fun can be used to simulate
multi-threading in a single thread. You can find the source code that
we use in the tutorial below or in
[../test/examples/thread_example.c](../test/examples/thread_example.c).

```c
#include <stdio.h>
#include <stdlib.h>

#define YCF_YIELD_NO_REDS()

static int f_2(char* name, int n) {
  for(int y = 0; y < 2; y++) {
    printf("%s f_2: y=%d\n", name, y);
  }
  return n*2;
}

static void f_1(char* name, int x) {
  YCF_YIELD_NO_REDS();
  while (x > 0) {
    int f_2_ret = f_2(name, x);
    printf("%s f_1: x=%d f_2_ret=%d\n", name, x, f_2_ret);
    x--;
  }
  printf("%s f_1: DONE\n", name);
}

static void* ycf_alloc(size_t size, void* context){
  (void)context;
  return malloc(size);
}

static void ycf_free(void* data, void* context){
  (void)context;
  free(data);
}

int main( int argc, const char* argv[] ) {
#ifdef YCF_YIELD_CODE_GENERATED
  long t1_nr_of_reds = 1;
  void* t1_state = NULL;
  long t2_nr_of_reds = 2;
  void* t2_state = NULL;
  long t3_nr_of_reds = 1000;
  void* t3_state = NULL;
  /* Start t1, t2  and t3*/
  f_1_ycf_gen_yielding(&t1_nr_of_reds, &t1_state, NULL,
                       ycf_alloc, ycf_free, NULL, 0, NULL,
                       "t1", 2);
  f_1_ycf_gen_yielding(&t2_nr_of_reds, &t2_state, NULL,
                       ycf_alloc, ycf_free, NULL, 0, NULL,
                       "t2", 4);
  f_1_ycf_gen_yielding(&t3_nr_of_reds, &t3_state, NULL,
                       ycf_alloc, ycf_free, NULL, 0, NULL,
                       "t3", 2);
  printf("THREADS STARTED\n");
  /* Execute t1, t2 and t3*/
  while (t1_state != NULL ||
         t2_state != NULL ||
         t3_state != NULL) {
    t1_nr_of_reds = 1;
    t2_nr_of_reds = 2;
    if (t1_state != NULL) {
      printf("SCHEDULING THREAD: t1\n");
      f_1_ycf_gen_continue(&t1_nr_of_reds, &t1_state, NULL);
      if (t1_state == NULL) {
        printf("THREAD t1 DONE (number of reductions left = %ld)\n",
               t1_nr_of_reds);
      }
    }
    if (t2_state != NULL) {
      printf("SCHEDULING THREAD: t2\n");
      f_1_ycf_gen_continue(&t2_nr_of_reds, &t2_state, NULL);
      if (t2_state == NULL) {
        printf("THREAD t2 DONE (number of reductions left = %ld)\n",
               t2_nr_of_reds);
      }
    }
    if (t3_state != NULL) {
      printf("SCHEDULING THREAD: t3\n");
      f_1_ycf_gen_continue(&t3_nr_of_reds, &t3_state, NULL);
      if (t3_state == NULL) {
        printf("THREAD t3 DONE (number of reductions left = %ld)\n",
               t3_nr_of_reds);
      }
    }
  }
#endif
  (void)f_1;
  printf("DONE\n");
  return 0;
}
```

To run this example, you need to compile YCF itself, if you have not
done so already:

    cd directory_where_the_ycf_source_code_is_located
    YCF_ROOT=`pwd`
    make

Now, you can transform `test/examples/thread_example.c` by executing
the following command:

    "$YCF_ROOT"/bin/yielding_c_fun.bin \
            -f f_1 -f f_2 \
            -output_file_name modified_thread_example.c \
            "$YCF_ROOT"/test/examples/thread_example.c

A new file should now exist in your current directory called
`modified_thread_example.c`. This file contains a transformed
version of `test/examples/thread_example.c`. The parameters `-f
f_1` and `-f f_2` tells YCF to generate yieldable versions of the
functions named `f_1` and `f_2`.

Before you inspect the generated file to see what the tool has done,
it is smart to format the generated source code with `clang-format` or
some other tool to make the code more readable:

    clang-format -i modified_thread_example.c

You can now open the generated source code in your favorite editor:

    emacs modified_thread_example.c &

The yieldable functions get the suffix `_ycf_gen_yielding`. For
example, the yielding version of `f_1` is called
`f_1_ycf_gen_yielding`. As you can see, several parameters have been
added to the yieldable versions of `f_1`. We explain the first two of
these parameters here. [The main documentation of YCF](../README.md)
explains the rest of the parameters. The first parameter is an
input/output parameter that tells the yieldable function how many
reductions that can be "consumed" before it yields. The first
parameter can also be used by the caller to get the number of
reductions that have been consumed. The second parameter is also an
input/output parameter that should be a pointer to a `NULL` pointer
when the yieldable function is first started. The pointer that is
pointed to by the second parameter is set to a value which is
different from `NULL` when the function yields.

Let us now compile and run the generated source code so we can see
what is happening:


    cc -g modified_thread_example.c

```
$ ./a.out
THREADS STARTED
SCHEDULING THREAD: t1
SCHEDULING THREAD: t2
SCHEDULING THREAD: t3
t3 f_2: y=0
t3 f_2: y=1
t3 f_1: x=2 f_2_ret=4
t3 f_2: y=0
t3 f_2: y=1
t3 f_1: x=1 f_2_ret=2
t3 f_1: DONE
THREAD t3 DONE (number of reductions left = 994)
SCHEDULING THREAD: t1
SCHEDULING THREAD: t2
t2 f_2: y=0
t2 f_2: y=1
t2 f_1: x=4 f_2_ret=8
SCHEDULING THREAD: t1
t1 f_2: y=0
SCHEDULING THREAD: t2
t2 f_2: y=0
SCHEDULING THREAD: t1
t1 f_2: y=1
t1 f_1: x=2 f_2_ret=4
SCHEDULING THREAD: t2
t2 f_2: y=1
t2 f_1: x=3 f_2_ret=6
SCHEDULING THREAD: t1
SCHEDULING THREAD: t2
t2 f_2: y=0
t2 f_2: y=1
t2 f_1: x=2 f_2_ret=4
SCHEDULING THREAD: t1
t1 f_2: y=0
SCHEDULING THREAD: t2
t2 f_2: y=0
SCHEDULING THREAD: t1
t1 f_2: y=1
t1 f_1: x=1 f_2_ret=2
t1 f_1: DONE
THREAD t1 DONE (number of reductions left = 1)
SCHEDULING THREAD: t2
t2 f_2: y=1
t2 f_1: x=1 f_2_ret=2
t2 f_1: DONE
THREAD t2 DONE (number of reductions left = 2)
DONE
$
```

We can see that all three calls to the yieldable version of `f_1`
yield before printing anything. This is due to the
`YCF_YIELD_NO_REDS()` statement at the beginning of `f_1`'s
body. We can also see that the "thread" named `t3` runs to completion
without yielding when we call `f_1_ycf_gen_continue` with `t3_state`
in the while loop and that `t3` consumed (1000-994=6) reductions. This
is expected as we gave `t3` 1000 reductions when we started it with
the `f_1_ycf_gen_yielding` function. The "threads" `t1` and `t2` get
much fewer reductions (i.e., 1 and 2) each time they get to execute
and they are therefore interleaved.

You can now experiment with other transformation options (e.g.,
`-frec` and `-fnoauto`) and the special statements that you can use
inside functions that can yield. All these options and statements are
documented [here](../README.md).

The best way to figure out how YCF works under the hood is probably to
step through the transformed program in a debugger.

Good Luck!