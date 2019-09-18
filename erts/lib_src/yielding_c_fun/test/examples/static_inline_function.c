#include <stdio.h>
#include <stdlib.h>

#if __STDC_VERSION__ >= 199901L

#else
#define inline
#endif

#define YCF_YIELD()

static
inline
int fun(char x);


static
inline
int fun(char x){
  x = x + 1; /* x == 2*/
  {
  int y;
  y = 1;
  {
  int z = 1;
  YCF_YIELD();
  x = x + y + z; /* x == 4*/
  return x;
  }
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
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,1);
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

