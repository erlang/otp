/*
    This program prints out a verbose explanation of a given regular expression.
*/

#include <stdio.h>
#include "re.h"


int main(int argc, char** argv)
{
  if (argc == 2)
  {
    re_print(re_compile(argv[1]));
  }
  else
  {
    printf("\nUsage: %s <PATTERN> \n", argv[0]);
  }   
  return -2; 
}

