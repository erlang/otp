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
  YCF_YIELD();
  if(x == 1){
    /*empty control statement*/
  } else {
    /* another empty*/
  }
  if(x == 1){

  } else {

  }
  if(x == 1){
    int y = 4;
    YCF_YIELD();
    printf("s_1 %d %d\n", x, y);
  }
  YCF_YIELD();
  if(x == 1){
    int y = 4;
    YCF_YIELD();
    printf("s_2 %d %d\n", x, y);
  }else printf("Hej\n");
  YCF_YIELD();
  if(x == 1){
    int y = 4;
    YCF_YIELD();
    printf("s_3 %d %d\n", x, y);
  }else if(x == 1){
    int y = 4;
    printf("Hej %d %d\n", x, y);
  }
  YCF_YIELD();
  if(x == 2){
    int y = 4;
    printf("Hej %d %d\n", x, y);
  }else if(x == 1){
    int y = 4;
    YCF_YIELD();
    printf("s_4 %d %d\n", x, y);
  }
  YCF_YIELD();
  if(x == 2){
    int y = 4;
    printf("Hej %d %d\n", x, y);
  }else{
    int y = 4;
    YCF_YIELD();
    printf("s_5 %d %d\n", x, y);
  }
  YCF_YIELD();
  while(x == 1) {
    int y = 4;
    x = 2;
    YCF_YIELD();
    printf("s_6 %d %d\n", x, y);
  }
  YCF_YIELD();
  x = 1;
  YCF_YIELD();
  do x++; while(x == 1);
  YCF_YIELD();
  printf("s_7 %d\n", x);
  YCF_YIELD();
  x = 1;
  do {
    int y = 4;
    YCF_YIELD();
    printf("s_8 %d %d\n", x, y);
    YCF_YIELD();
    x++;
  } while(x == 2);
  YCF_YIELD();
  x = 1;
  YCF_YIELD();
  while(x==1){
    int y = 4;
    YCF_YIELD();
    printf("s_9 %d %d\n", x, y);
    YCF_YIELD();
    x++;
  }
  YCF_YIELD();
  x = 2;
  for(;;){
    int y = 4;
    YCF_YIELD();
    printf("s_10 %d %d\n", x, y);
    YCF_YIELD();
    break;
  }
  YCF_YIELD();
  for(x = 0;x < 10;x++){
    int y = 4;
    YCF_YIELD();
    printf("s_11 %d %d\n", x, y);
  }
  YCF_YIELD();
  x = 42;
  YCF_YIELD();
  switch(x){
    int y;
  case 42:
    y = 4;
    YCF_YIELD();
    printf("s_12 %d %d\n", x, y);
    YCF_YIELD();
  }
  YCF_YIELD();
  {
    x = 1;
    YCF_YIELD();
    if(x == 1) if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_1 %d %d\n", x, y);
    }
    YCF_YIELD();
    if(x == 1) switch(1){
        int y;
      case 1:;
        y = 4;
        YCF_YIELD();
        printf("s_2 %d %d\n", x, y);
    }else printf("Hej\n");
    YCF_YIELD();
    if(x == 1)  switch(1){
      int y;
      case 1:;
        y = 4;
      YCF_YIELD();
      printf("s_3 %d %d\n", x, y);
    }else if(x == 1){
      int y = 4;
      printf("Hej %d %d\n", x, y);
    }
    YCF_YIELD();
    if(x == 2){
      int y = 4;
      printf("Hej %d %d\n", x, y);
    }else if(x == 1) if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_4 %d %d\n", x, y);
    }
    YCF_YIELD();
    if(x == 2){
      int y = 4;
      printf("Hej %d %d\n", x, y);
    }else if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_5 %d %d\n", x, y);
    }
    YCF_YIELD();
    while(x == 1) if(1) {
      int y = 4;
      x = 2;
      YCF_YIELD();
      printf("s_6 %d %d\n", x, y);
    }
    YCF_YIELD();
    x = 1;
    YCF_YIELD();
    do if(1) x++; while(x == 1);
    YCF_YIELD();
    printf("s_7 %d\n", x);
    YCF_YIELD();
    x = 1;
    do if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_8 %d %d\n", x, y);
      YCF_YIELD();
      x++;
    } while(x == 2);
    YCF_YIELD();
    x = 1;
    YCF_YIELD();
    while(x==1) if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_9 %d %d\n", x, y);
      YCF_YIELD();
      x++;
    }
    YCF_YIELD();
    x = 2;
    for(;;) if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_10 %d %d\n", x, y);
      YCF_YIELD();
      break;
    }
    YCF_YIELD();
    for(x = 0;x < 10;x++) if(1){
      int y = 4;
      YCF_YIELD();
      printf("s_11 %d %d\n", x, y);
    }
    YCF_YIELD();
    x = 42;
    YCF_YIELD();
    switch(x){
      int y;
    case 42:
      y = 4;
      YCF_YIELD();
      printf("s_12 %d %d\n", x, y);
      YCF_YIELD();
    }
    YCF_YIELD();
  }
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
  int nr_of_trapps = 0;
  long nr_of_reductions = 1;
#ifdef YCF_YIELD_CODE_GENERATED
  do{
    ret = fun_ycf_gen_yielding(&nr_of_reductions,&wb,NULL,allocator,freer,NULL,0,NULL,1);
    if(wb != NULL){
      nr_of_trapps++;
    }
  }while(wb != NULL);
  if(wb != NULL){
    free(wb);
  }
#else
  ret = fun(1);
  nr_of_trapps = 86;
#endif
  printf("RETURNED %d %d\n", ret, nr_of_trapps);
  if(ret != 42){
    return 1;
  }else{
    return 0;
  }
}
