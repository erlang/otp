/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
