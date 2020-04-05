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

int fun(char x){
  int y = 10;
  /*special_code_start:ON_SAVE_YIELD_STATE*/
  if(0){
    printf("y=%d\n", y);
    y = 42;
  }
  /*special_code_end*/
  /*special_code_start:ON_RETURN*/
  if(0){
    printf("I returned y=%d\n", y);
  }
  /*special_code_end*/
  /*special_code_start:ON_DESTROY_STATE_OR_RETURN*/
  if(0){
    printf("I got destroyed or returned y=%d\n", y);
  }
  /*special_code_end*/
  /*special_code_start:ON_RESTORE_YIELD_STATE*/
  if(0){
    x = 9;
  }
  /*special_code_end*/
  if(0){
    /*special_code_start:ON_SAVE_YIELD_STATE*/
    if(0){
      int z = 10;
      printf("y=%d z=%d\n", y, z);
    }
    /*special_code_end*/
  }
  if(y != 10 || x != 1){
    /*special_code_start:ON_RESTORE_YIELD_STATE*/
    if(0){
      printf("y=%d x=%d\n", y, x);
      x = x*2;
      printf("y=%d x=%d\n", y, x);
    }
    /*special_code_end*/
    /*special_code_start:ON_RESTORE_YIELD_STATE*/
    if(0){
      printf("y=%d x=%d\n", y, x);
      x = x/2;
      printf("y=%d x=%d\n", y, x);
    }
    /*special_code_end*/
    printf("ERROR BEFORE YIELD\n");
    exit(1);
  }
  YCF_YIELD();
  if(y != 42 || x != 9){
    printf("ERROR AFTER YIELD\n");
    exit(1);
  }
  printf("SUCCESS\n");
  return x;
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
  int ret = 0;
  long nr_of_reductions = 1;
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
  ret = fun(1);
#endif
  printf("RETURNED %d\n", ret);
  if(ret != 9){
    return 1;
  }else{
    return 0;
  }
}

