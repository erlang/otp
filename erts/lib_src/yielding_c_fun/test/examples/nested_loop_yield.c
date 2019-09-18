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


int fun(){
  int x;
  int y;
  int z;
  int outer = 0;
  int inner = 0;
  for(x = 0; x < 2; x++){
    for(y = 0; y < 2; y++){ /* 2 times */
      for(z = 0; z < 2; z++){ /* 4 times */
        YCF_YIELD(); /* 8 times */
        outer++;
        printf("outer %d: x=%d y=%d z=%d\n", outer, x, y, z);
        {
          int x;
          int y;
          int z;
          for(x = 0; x < 2; x++){ /* 8 times */
            for(y = 0; y < 2; y++){ /* 16 times */
              for(z = 0; z < 2; z++){ /* 32 times */
                YCF_YIELD(); /* 64 times */
                inner++;
                printf("inner %d: x=%d y=%d z=%d\n", inner, x, y, z);
              }
            }
          }
        }
      }
    }
  }
  return inner + outer;
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
  int nr_of_traps = 0;
  long nr_of_reductions = 1;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL);
    if(wb != NULL){
      nr_of_traps++;
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  ret = fun();
#endif
  printf("NR OF TRAPS %d\n", nr_of_traps);
  printf("RETURNED %d\n", ret);
  if(ret != 72 || nr_of_traps != ret){
    return 1;
  }else{
    return 0;
  }
}
