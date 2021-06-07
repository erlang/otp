/*
    This program tries to match a given regular expression with text given as input to stdin.
    If the text is a match for the pattern, the program returns 0.
    If the text doesn't match the pattern, the program returns -2.
    
    This program is used in random testing to test a lot of random text and regex together.
    See ./scripts/regex_test.py and the Makefile for this project for the gritty details.
*/

#include <stdio.h>
#include "re.h"


int main(int argc, char** argv)
{
  if (argc == 3)
  {
    int m = re_match(argv[1], argv[2]);
    if (m != -1) 
      return 0;
  }
  else
  {
    printf("\nUsage: %s <PATTERN> <TEXT> \n", argv[0]);
  }   
  return -2; 
}

