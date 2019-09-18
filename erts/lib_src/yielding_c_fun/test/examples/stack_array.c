/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB and Kjell Winblad 2019. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Description:
 *
 * Author: Kjell Winblad
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define YCF_YIELD()


void fun(int x3content[]){
  int x1[2];
  int x2[2][2];
  int *x3[2];
  int z = 42;
  (void)z;
  x1[0] = 1;
  x1[1] = 2;
  x2[0][0] = 3;
  x2[0][1] = 4;
  x2[1][0] = 5;
  x2[1][1] = 6;
  x3[0] = x3content;
  x3[1] = x3content;
  x3[0][0] = 7;
  x3[0][1] = 8;
  x3[1][0] = 9;
  x3[1][1] = 10;
  printf("%d %d %d %d %d %d %d %d %d %d\n",
         x1[0],
         x1[1],
         x2[0][0],
         x2[0][1],
         x2[1][0],
         x2[1][1],
         x3[0][0],
         x3[0][1],
         x3[1][0],
         x3[1][1]);
  YCF_YIELD();
  printf("%d %d %d %d %d %d %d %d %d %d\n",
         x1[0],
         x1[1],
         x2[0][0],
         x2[0][1],
         x2[1][0],
         x2[1][1],
         x3[0][0],
         x3[0][1],
         x3[1][0],
         x3[1][1]);
  return;
}

void fun_reset(int x3content[]){
  int x1[2];
  int x2[2][2];
  int *x3[2];
  (void)x2;
  (void)x1;
  x1[0] = 42;
  x1[1] = 42;
  x2[0][0] = 42;
  x2[0][1] = 42;
  x2[1][0] = 42;
  x2[1][1] = 42;
  x3[0] = x3content;
  x3[1] = x3content;
  x3[0][0] = 42;
  x3[0][1] = 42;
  x3[1][0] = 42;
  x3[1][1] = 42;
  return;
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
#endif
  int x3content[2];
  long nr_of_reductions = 1;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL, x3content);
    fun_reset(x3content);
    if(wb != NULL){
      printf("TRAPPED\n");
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  fun(x3content);
#endif
  printf("RETURNED\n");
  return 0;
}

