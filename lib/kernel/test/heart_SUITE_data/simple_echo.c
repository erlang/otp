#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VXWORKS
int simple_echo(void){
#else
int main(void){
#endif
  int x;
  while((x = getchar()) != EOF){
    putchar(x);
    fflush(stdout);
  }
  return 0;
}
      
