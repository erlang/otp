/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
 *

 */
#include "hash.h"

/* this is a general prime factoring function 
 * we get one prime factor each time we call it 
 * we only use it here to determine if n is prime, 
 * by checking if factor(n) == n .
 */
static int factor(int n)
{
  /* FIXME problem for threaded?! */
  static int a[] = { 0, 4, 1, 2, 0, 2 };
  static int m = 0;
  static int d = 0;

  if (n) {
    m = n;
    d = 2;
  }

  while ((d*d) <= m) {
    if (!(m%d)) {
      m /= d;
      return d;
    }
    d += a[d%6];
  }
  n = m; 
  m = 0;

  return n;
}

/* true if n prime */
int ei_isprime(int n)
{
  return (n == factor(n));
}
