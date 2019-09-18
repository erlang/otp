#include <stdio.h>
#include <stdlib.h>

#define YCF_YIELD_NO_REDS()

int empty_fun(){
  return 1;
}

int fun(char x){
  x = x + 1; /* x == 2*/
  YCF_YIELD_NO_REDS();
  x = x + 1; /* x == 3*/
  {
  int hej = !!empty_fun();
  (void)hej;
  return x;
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
  long nr_of_reductions = 777;
#endif
  int ret = 0;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,1);
    if(wb != NULL){
      if(nr_of_reductions != 777){
        printf("SHOULD NOT HAPPEN\n");
      }
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
  if(ret != 3){
    return 1;
  }else{
    return 0;
  }
}

