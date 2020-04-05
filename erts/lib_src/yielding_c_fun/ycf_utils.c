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
 * Author: Kjell Winblad
 */

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

#include "ycf_utils.h"
#include "lib/simple_c_gc/simple_c_gc.h"
#include <stdint.h>

bool ycf_use_gc = false;
bool ycf_track_memory = false;

size_t ycf_memory_usage = 0;
size_t ycf_max_memory_usage = 0;

void ycf_enable_gc(){
  ycf_use_gc = true;
}

void ycf_enable_memory_tracking(){
  ycf_track_memory = true;
}

void ycf_malloc_log(char* log_file, char* log_entry_id) {
    FILE* out = fopen(log_file, "a");
    fprintf(out,
            "(%s)\nMax memory consumption %zu bytes ~ %zu kilo bytes ~ %zu mega bytes (after=%zu)\n",
            log_entry_id,
            ycf_max_memory_usage,
            ycf_max_memory_usage / 1000,
            ycf_max_memory_usage / 1000000,
            ycf_memory_usage);
    fclose(out);
}

void* ycf_raw_malloc(size_t size) {
  if (ycf_track_memory) {
    void* block = malloc(size + sizeof(intptr_t));
    intptr_t* size_ptr = block;
    *size_ptr = size + sizeof(intptr_t);
    ycf_memory_usage = ycf_memory_usage + size + sizeof(intptr_t);
    if (ycf_memory_usage > ycf_max_memory_usage) {
      ycf_max_memory_usage = ycf_memory_usage;
    }
    if(block == NULL) {
      fprintf(stderr, "ycf_malloc failed: is there enough memory in the machine?\n");
      exit(1);
    }
    return (void*)(((char*)block) + sizeof(intptr_t));
  } else {
    void* block = malloc(size);
    if(block == NULL) {
      fprintf(stderr, "ycf_malloc failed: is there enough memory in the machine?\n");
      exit(1);
    }
    return block;
  }
}

void* ycf_malloc(size_t size) {
  if (ycf_use_gc) {
    return scgc_new(size);
  } else {
    return ycf_raw_malloc(size);
  }
}


void ycf_free(void* to_free) {
  if (ycf_track_memory) {
    char* to_free_cp = to_free;
    char* start = to_free_cp - sizeof(intptr_t);
    intptr_t* size_ptr = (intptr_t*)start;
    ycf_memory_usage = ycf_memory_usage - *size_ptr;
    free(start);
  } else {
    free(to_free);
  }
}



