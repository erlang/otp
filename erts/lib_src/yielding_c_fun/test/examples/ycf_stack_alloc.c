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
#define YCF_CONSUME_REDS(X)
#ifndef YCF_YIELD_CODE_GENERATED
#define YCF_STACK_ALLOC(X) malloc(1)
#endif

int sub_fun(int x){
  int i;
  int count = 0;
  char* my_data = YCF_STACK_ALLOC(5);
  my_data[0] = 's';
  my_data[1] = 'u';
  my_data[2] = 'b';
  my_data[3] = '\n';
  my_data[4] = '\0';
  for(i = 0; i < 2; i++){
    count = count + 2;
    YCF_YIELD();
    printf("%s", my_data);
  }
  return count + x;
}

int fun(int x){
  int ret;
  char* my_data = YCF_STACK_ALLOC(5);
  char* my_data2 = YCF_STACK_ALLOC(5);
  char* my_data3;
  my_data[0] = 'h';
  my_data[1] = 'e';
  my_data[2] = 'j';
  my_data[3] = '\n';
  my_data[4] = '\0';
  my_data2[0] = 's';
  my_data2[1] = 'o';
  my_data2[2] = 's';
  my_data2[3] = '\n';
  my_data2[4] = '\0';
  YCF_YIELD();
  printf("%s", my_data);
  printf("%s", my_data2);
  ret = sub_fun(x);
  ret = sub_fun(x);
  my_data3 = YCF_STACK_ALLOC(5);
  printf("BEFORE OVERWRITE %s", my_data3);
  my_data3[0] = 'e';
  my_data3[1] = 'r';
  my_data3[2] = 'l';
  my_data3[3] = '\n';
  my_data3[4] = '\0';
  printf("AFTER OVERWRITE %s", my_data3);
  return ret;
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
  int nr_of_yields = 0;
  long nr_of_reductions;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    nr_of_reductions = 101;
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,15,NULL,1);
    if(wb != NULL){
      printf("TRAPPED %ld\n", nr_of_reductions);
      nr_of_yields++;
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  ret = fun(1);
#endif
  printf("Number of yields %d\n", nr_of_yields);
  printf("RETURNED %d\n", ret);
  if(ret != 5){
    return 1;
  }else{
    return 0;
  }
}

