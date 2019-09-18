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


void fun(int level){
  void* my_mem = malloc(1000);
  /*special_code_start:ON_DESTROY_STATE*/
  if(0){
    printf("FREE AT LEVEL %d\n", level);
    free(my_mem);
  }
  /*special_code_end*/
  /*special_code_start:ON_DESTROY_STATE_OR_RETURN*/
  if(0){
    printf("I got destroyed or returned %d\n", level);
  }
  /*special_code_end*/
  printf("LEVEL %d\n", level);
  if (level == 10) {
    YCF_YIELD();
    printf("SHOULD NOT BE PRINTED 1\n");
    return;
  }else {
    fun(level + 1);
    printf("SHOULD NOT BE PRINTED 2\n");
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
  do {
    fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,1);
    if(wb != NULL){
      printf("TRAPPED\n");
      fun_ycf_gen_destroy(wb);
      printf("DESTROYED\n");
      wb = NULL;
      break;
    }
  } while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  fun(1);
#endif
  printf("RETURNED\n");
  return 0;
}

