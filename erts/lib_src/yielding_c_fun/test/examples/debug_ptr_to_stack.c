#include <stdio.h>
#include <stdlib.h>

#define YCF_YIELD()

int fun(){
  int array[1];
  int* ptr;
  array[0] = 1;
  ptr = &array[0];
  (void)ptr;
  YCF_YIELD();
  array[0] = 1;
  return array[0];
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
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL);
    if(wb != NULL){
      printf("TRAPPED\n");
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  fun();
#endif
  printf("RETURNED %d\n", ret);
  if(ret != 1){
    return 1;
  }else{
    return 0;
  }
}

