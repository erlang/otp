/*
 * %CopyrightBegin%
 *
 * Copyright 2019 Kjell Winblad (kjellwinblad@gmail.com, http://winsh.me).
 * All Rights Reserved.
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

/**
 * Pulic interface for Simple C GC.
 *
 * @author Kjell Winblad
 *
 */

#ifndef SIMPLE_C_GC_H
#define SIMPLE_C_GC_H

#include <stddef.h>

/**
 * This function starts code that will be garbage collected
 *
 * @param main The function that will be started and garbage collected
 * @param argc Passed to main
 * @param argv Passed to main
 * @param my_malloc Simple C GC will use this to allocated memory
 * @param my_free Simple C GC will use this to free memory
 */
int scgc_start_gced_code(int (*main)(int, char *[]), int argc, char *argv[],
                         void *(*my_malloc)(size_t), void (*my_free)(void *));

/**
 * Allocate a new memory block that will be garbage collected
 *
 * @param size The size of the new memory block
 */
void *scgc_new(size_t size);

/**
 * Enables printing of garbage collection information
 */
void scgc_enable_print_gc_info(void);
#endif
