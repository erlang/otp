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


int rec_inc(int to_inc, int nr_of_incs){
  int res;
  printf("nr_of_incs %d\n", nr_of_incs);
  if(nr_of_incs == 0) {
    return to_inc;
  } else {
    res = rec_inc(to_inc + 1, nr_of_incs -1);
    return res;
  }
}


int fun(){
  int i;
  int v1;
  int v2 = 13;
  int v3 = 7;
  int v4 = 5;
  int v5 = 2;
  printf("REC CALL START\n");
  v1 = rec_inc(42, 100); /* v1 == 142 */
  printf("REC CALL END\n");
  printf("FOR LOOP START\n");
  for(i = 0; i < 100; i++){
    v2 = v2 + 1;
  } /* v2 == 113 */
  printf("FOR LOOP END\n");
  printf("WHILE LOOP START\n");
  i = 0;
  while(i < 100){
    v3 = v3 + 1;
    i++;
  } /* v3 == 107 */
  printf("WHILE LOOP END\n");
  printf("DO WHILE LOOP START\n");
  i = 0;
  do {
    v4 = v4 + 1;
    i++;
  } while (i < 100); /* v4 == 105 */
  printf("DO WHILE LOOP END\n");
  printf("GOTO START\n");
  i = 0;
 my_label:
  v5 = v5 + 1;
  i++;
  if (i < 100) goto my_label; /* v5 == 102 */
  printf("GOTO END\n");
  return v1 + v2 + v3 + v4 + v5; /* 142 + 113 + 107 + 105 + 102 == 569 */
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
  long nr_of_reductions;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    nr_of_reductions = 10;
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL);
    if(wb != NULL){
      printf("TRAPPED %ld\n", nr_of_reductions);
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
  printf("REDS LEFT WHEN READY %ld\n", nr_of_reductions);
#else
  ret = fun();
#endif
  printf("RETURNED %d\n", ret);
  if(ret != 569){
    return 1;
  }else{
    return 0;
  }
}

