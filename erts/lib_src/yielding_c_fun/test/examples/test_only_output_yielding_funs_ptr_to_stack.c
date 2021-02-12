
#include <stdio.h>
#include <stdlib.h>

void* ycf_debug_global_stack_start_ptr = NULL;
void* ycf_debug_get_stack_start(){
    return ycf_debug_global_stack_start_ptr;
}
#define YCF_YIELD()

int fun2(char* to_stack_in_other_fun){
    char value = *to_stack_in_other_fun;
    YCF_YIELD();
    if (value == 42) {
        return 1;
    } else {
        return 0;
    }
}

#include "tmp.ycf.h"

static int fun(char x){
  char array[1];
  array[0] = 42;
  char *to_stack = &array[0];
  x = x + 1; /* x == 2*/
  printf("ptr to: %d", (int)(*to_stack));
  x = x + 1; /* x == 3*/
  {
      int hej = 0;
      hej = fun2(to_stack);
      YCF_YIELD();
      to_stack = NULL;
      return x + hej;
  }
}

void* allocator(size_t size, void* context){
  (void)context;
  return malloc(size);
}

void freer(void* data, void* context){
  (void)context;
  free(data);
}

int main( int argc, const char* argv[] )
{
#ifdef YCF_YIELD_CODE_GENERATED
  void* wb = NULL;
  long nr_of_reductions = 1;
#endif
  int ret = 0;
  (void)fun;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    ycf_debug_global_stack_start_ptr = &wb;
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,1);
    ycf_debug_global_stack_start_ptr = NULL;
    if(wb != NULL){
      printf("TRAPPED\n");
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  fun(1);
#endif
  printf("RETURNED %d\n", ret);
  if(ret != 4){
    return 1;
  }else{
    return 0;
  }
}

