#include <stdio.h>
#include <stdlib.h>

#define YCF_YIELD()


int fun(char x){
  int y = 10;
  int i = 0;
  printf("BEFORE YCF_YIELD()\n");
  y = y + x;/*y == 11*/
  for (i = 0; i < 100; i++){
    printf("ITER %d\n", i);
    y = y + x;
    YCF_YIELD();
  }
  printf("AFTER YCF_YIELD()\n");/*y == 111*/
  y = y*3;
  return y;/*y == 333*/
}

int main( int argc, const char* argv[] )
{
#ifdef YCF_YIELD_CODE_GENERATED
  void* wb = NULL;
#endif
  int ret = 0;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    ret = fun(1,&wb,1);
    if(wb != NULL){
      printf("TRAPPED\n");
    }
  }while(wb != NULL);
#else
  fun(1);
#endif
  printf("RETURNED %d\n", ret);
  return 0;
}


