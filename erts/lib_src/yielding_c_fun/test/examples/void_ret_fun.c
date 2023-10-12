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

#define YCF_YIELD()


void fun(int* y){
  int x = 1;
  x = x + 1; /* x == 2*/
  YCF_YIELD();
  x = x + 1; /* x == 3*/
  *y = x;
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
  int nr_of_trapps = 0;
  int ret = 0;
  long nr_of_reductions = 1;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,&ret);
    if(wb != NULL){
      printf("TRAP\n");
      nr_of_trapps++;
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
  if(nr_of_trapps != 1){
    printf("ERROR\n");
    exit(1);
  }
#else
  fun(&ret);
#endif
  printf("RETURNED %d\n", ret);
  if(ret != 3){
    printf("ERROR\n");
    exit(1);
  }
  return 0;
}

