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

#include "ycf_string.h"
#include "ycf_lists.h"
#include "ycf_utils.h"

bool ycf_string_is_equal(const char* str1, const char* str2)
{
    size_t str1_length = strlen(str1);
    size_t str2_length = strlen(str2);
    return
            str1_length == str2_length &&
            strncmp(str1, str2, str1_length) == 0;
}


ycf_string_item* ycf_string_item_new(char* str){
    ycf_string_item* item = ycf_malloc(sizeof(ycf_string_item));
    item->str = str;
    item->next = NULL;
    return item;
}

char* ycf_string_new(char* format, ...){
    va_list args;
    va_start (args, format);
    int n = vsnprintf(NULL, 0, format, args);
    va_end (args);
    char* new = ycf_malloc(n +1);
    va_start (args, format);
    vsnprintf(new, n +1, format, args);
    va_end (args);
    return new;
}


ycf_string_printable_buffer* ycf_string_printable_buffer_new(){
    const size_t init_size = 128;
    ycf_string_printable_buffer* b = ycf_malloc(sizeof(ycf_string_printable_buffer));
    b->current_pos = 0;
    b->buffer = ycf_malloc(init_size);
    b->size = init_size;
    return b;
}

void ycf_string_printable_buffer_printf(ycf_string_printable_buffer* buf, char* format, ...){
    va_list args;
    if(buf == NULL){
        va_start (args, format);
        vprintf(format, args);
        va_end (args);
        return;
    }
    va_start (args, format);
    int n = vsnprintf(NULL, 0, format, args) + 1;
    va_end (args);
    while(buf->current_pos + n + 64 > buf->size){
        char* new_buf = ycf_malloc(buf->size * 2);
        for(int i = 0; i < buf->size; i++){
            new_buf[i] = buf->buffer[i];
        }
        buf->buffer = new_buf;
        buf->size = buf->size * 2;
    }
    va_start (args, format);
    vsnprintf(&buf->buffer[buf->current_pos], n, format, args);
    va_end (args);
    buf->current_pos = buf->current_pos + n - 1;
}

bool ycf_string_item_list_contains(ycf_string_item_list* l, char* str){
  ycf_string_item* current = l->head;
  while(current != NULL){
    if(strcmp(current->str, str) == 0){
      return true;
    }
    current = current->next;
  }
  return false;
}


GENERATE_LIST_FUNCTIONS(ycf_string_item)
