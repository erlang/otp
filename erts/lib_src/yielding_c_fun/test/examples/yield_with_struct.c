#include <stdio.h>
#include <stdlib.h>

typedef struct my_struct {
  struct my_struct* my_struct;
  int len;
} my_struct;


#define YCF_YIELD()

int fun(char x){
  my_struct s;
  my_struct* sp;
  int len = 55;
  s.len = 10;
  s.my_struct = NULL;
  sp = malloc(sizeof(my_struct));
  sp->len = 10;
  sp->my_struct = NULL;
  YCF_YIELD();
  return
    s.len == 10 &&
    s.my_struct == NULL &&
    sp->len == 10 &&
    sp->my_struct == NULL &&
    len == 55;
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
  if(ret != 1){
    return 1;
  }else{
    return 0;
  }
}

