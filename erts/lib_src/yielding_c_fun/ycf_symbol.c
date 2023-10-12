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


#include <stdlib.h>
#include <string.h>

#include "ycf_symbol.h"
#include "ycf_string.h"
#include "ycf_utils.h"
#include "ycf_lists.h"



ycf_symbol* ycf_symbol_copy(ycf_symbol* symbol){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = symbol->source;
    new_symbol->start = symbol->start;
    new_symbol->stop = symbol->stop;
    new_symbol->type = symbol->type;
    new_symbol->whitespace_or_comment_before = symbol->whitespace_or_comment_before;
    return new_symbol;
}

ycf_symbol* ycf_symbol_copy_change_text(ycf_symbol* to_copy, char* new_text){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = new_text;
    new_symbol->start = 0;
    new_symbol->stop = strlen(new_text);
    new_symbol->type = to_copy->type;
    new_symbol->whitespace_or_comment_before = to_copy->whitespace_or_comment_before;
    return new_symbol;
}

ycf_symbol* ycf_symbol_new_something_else(char* text){
    ycf_symbol* new = ycf_malloc(sizeof(ycf_symbol));
    new->type = ycf_symbol_type_something_else;
    new->next = NULL;
    new->source = text;
    new->whitespace_or_comment_before = NULL;
    new->start = 0;
    new->stop = strlen(text);
    return new;
}

ycf_symbol* ycf_symbol_new_semicolon(){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = ";";
    new_symbol->start = 0;
    new_symbol->stop = strlen(";");
    new_symbol->type = ycf_symbol_type_semicolon;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}

ycf_symbol* ycf_symbol_new_identifier(char* name){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = name;
    new_symbol->start = 0;
    new_symbol->stop = strlen(new_symbol->source);
    new_symbol->type = ycf_symbol_type_identifier;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}

ycf_symbol* ycf_symbol_new_star(){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = "*";
    new_symbol->start = 0;
    new_symbol->stop = strlen("*");
    new_symbol->type = ycf_symbol_type_star;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}

ycf_symbol* ycf_symbol_new_parenthesis(){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = ")";
    new_symbol->start = 0;
    new_symbol->stop = strlen(")");
    new_symbol->type = ycf_symbol_type_semicolon;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}

ycf_symbol* ycf_symbol_new_open_curly_brace(){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = "\n{\n";
    new_symbol->start = 0;
    new_symbol->stop = strlen(new_symbol->source);
    new_symbol->type = ycf_symbol_type_open_curly_brace;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}

ycf_symbol* ycf_symbol_new_end_curly_brace(){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = "\n}\n";
    new_symbol->start = 0;
    new_symbol->stop = strlen(new_symbol->source);
    new_symbol->type = ycf_symbol_type_end_curly_brace;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}


ycf_symbol* ycf_symbol_new_comma(){
    ycf_symbol* new_symbol = ycf_malloc(sizeof(ycf_symbol));
    new_symbol->next = NULL;
    new_symbol->source = ",";
    new_symbol->start = 0;
    new_symbol->stop = strlen(",");
    new_symbol->type = ycf_symbol_type_semicolon;
    new_symbol->whitespace_or_comment_before = NULL;
    return new_symbol;
}


char* ycf_symbol_get_text(ycf_symbol* symbol){
    int size = symbol->stop - symbol->start;
    char* str = ycf_malloc(size+1);
    strncpy(str, &symbol->source[symbol->start], size);
    str[size] = 0;
    return str;
}

char* ycf_symbol_list_to_str(ycf_symbol_list* l){
    ycf_symbol* s = l->head;
    char* str = "";
    while(s != NULL){
        str = ycf_string_new(" %s %s ", str, ycf_symbol_get_text(s));
        s = s->next;
    }
    return str;
}

GENERATE_LIST_FUNCTIONS(ycf_symbol)
