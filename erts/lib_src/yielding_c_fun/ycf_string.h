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

#ifndef YIELDING_C_FUN_YCF_STRING_H
#define YIELDING_C_FUN_YCF_STRING_H

#include <stdlib.h>
#include "ycf_utils.h"

/* Types for printable buffers */

typedef struct {
    char* buffer;
    size_t current_pos;
    size_t size;
} ycf_string_printable_buffer;

/* Types for string lists */

typedef struct ycf_string_item {
    char* str;
    struct ycf_string_item* next;
} ycf_string_item;

typedef struct string_item_list {
    ycf_string_item* head;
    ycf_string_item* last;
} ycf_string_item_list;


bool ycf_string_is_equal(const char* str1, const char* str2);
ycf_string_item* ycf_string_item_new(char* str);
char* ycf_string_new(char* format, ...);

/* Functions for printable buffers */

ycf_string_printable_buffer* ycf_string_printable_buffer_new(void);
void ycf_string_printable_buffer_printf(ycf_string_printable_buffer* buf, char* format, ...);

/* Functions for string lists */

int ycf_string_item_list_get_item_position(ycf_string_item_list* list, ycf_string_item* node);

bool ycf_string_item_list_contains(ycf_string_item_list* l, char* str);

ycf_string_item* ycf_string_item_shallow_copy(ycf_string_item* n);
ycf_string_item* ycf_string_item_list_get_item_at_position(ycf_string_item_list* list, int pos);

void ycf_string_item_list_append(ycf_string_item_list* list, ycf_string_item* node);
void ycf_string_item_list_prepend(ycf_string_item_list* list, ycf_string_item* node);
void ycf_string_item_list_insert_before(ycf_string_item_list* list, ycf_string_item* before_this, ycf_string_item* to_insert);
void ycf_string_item_list_insert_after(ycf_string_item_list* list, ycf_string_item* after_this, ycf_string_item* to_insert);
void ycf_string_item_list_remove(ycf_string_item_list* list, ycf_string_item* to_remove);
void ycf_string_item_list_replace(ycf_string_item_list* list, ycf_string_item* to_replace, ycf_string_item* replace_with);
void ycf_string_item_list_concat(ycf_string_item_list* list1, ycf_string_item_list* list2);

ycf_string_item_list ycf_string_item_list_empty();
ycf_string_item_list ycf_string_item_list_shallow_copy(ycf_string_item_list n);
ycf_string_item_list ycf_string_item_list_copy_append(ycf_string_item_list list, ycf_string_item* node);
ycf_string_item_list ycf_string_item_list_copy_prepend(ycf_string_item_list list, ycf_string_item* node);
ycf_string_item_list ycf_string_item_list_copy_insert_before(ycf_string_item_list list, ycf_string_item* before_this, ycf_string_item* to_insert);
ycf_string_item_list ycf_string_item_list_copy_insert_after(ycf_string_item_list list, ycf_string_item* after_this, ycf_string_item* to_insert);
ycf_string_item_list ycf_string_item_list_copy_remove(ycf_string_item_list list, ycf_string_item* to_remove);
ycf_string_item_list ycf_string_item_list_copy_replace(ycf_string_item_list list, ycf_string_item* to_replace, ycf_string_item* replace_with);
ycf_string_item_list ycf_string_item_list_copy_concat(ycf_string_item_list list1, ycf_string_item_list list2);


#endif //YIELDING_C_FUN_YCF_STRING_H
