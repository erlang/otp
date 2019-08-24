#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void){
  int x;
  while((x = getchar()) != EOF){
    putchar(x);
    fflush(stdout);
  }
  return 0;
}
