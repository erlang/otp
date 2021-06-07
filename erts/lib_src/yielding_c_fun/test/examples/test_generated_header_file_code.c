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

int A(int depth);
int B(int depth);

int A(int depth){
  int b;
  YCF_YIELD();
  depth++;
  printf("A ");
  YCF_YIELD();
  if(depth == 100){
    return 1;
  } else {
    b = B(depth);
  }
  YCF_YIELD();
  return b + 1;
}

int B(int depth){
  int a;
  YCF_YIELD();
  depth++;
  printf("B ");
  YCF_YIELD();
  if(depth == 100){
    YCF_YIELD();
    return 1;
  } else {
    a = A(depth);
  }
  YCF_YIELD();
  return a + 1;
}
