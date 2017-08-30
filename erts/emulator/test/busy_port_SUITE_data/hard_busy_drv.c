/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
#define ERTS_TEST_BUSY_DRV_NAME "hard_busy_drv"
#define ERTS_TEST_BUSY_DRV_FLAGS  \
  ERL_DRV_FLAG_USE_PORT_LOCKING

#include "hs_busy_drv.c"
