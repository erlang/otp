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


unsigned long fun(char x, char* y, char **z, unsigned long a, unsigned long* b, unsigned long** c){
  unsigned int f = 3 + 3;
  x = x + 1;
  y = y + 1;
  z = z + 1;
  a = a + 1;
  b = b + 1;
  c = c + 1;
  YCF_YIELD();
  f = f - 6;
  x = x + 1;
  y = y + 1;
  z = z + 1;
  a = a + 1;
  b = b + 1;
  c = c + 1;
  return (unsigned long)((long)(x + y) + ((z + a) + ((long)b + (long)c))) + f;
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
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,1,(void*)1,(void*)1,1,(void*)1,(void*)1);
    if(wb != NULL){
      printf("TRAPPED\n");
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  ret = fun(1,(void*)1,(void*)1,1,(void*)1,(void*)1);
#endif
  printf("RETURNED %d\n", ret);
  if(ret != fun(1,(void*)1,(void*)1,1,(void*)1,(void*)1)){
    return 1;
  }else{
    return 0;
  }
}

