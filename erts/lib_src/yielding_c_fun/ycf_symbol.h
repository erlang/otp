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

#ifndef YIELDING_C_FUN_YCF_SYMBOL_H
#define YIELDING_C_FUN_YCF_SYMBOL_H

/* Types for symbols */

typedef enum {
    ycf_symbol_type_comment,
    ycf_symbol_type_string_literal,
    ycf_symbol_type_macro_define,
    ycf_symbol_type_macro_command,
    ycf_symbol_type_whitespace,
    ycf_symbol_type_identifier,
    ycf_symbol_type_number,
    ycf_symbol_type_open_parenthesis,
    ycf_symbol_type_end_parenthesis,
    ycf_symbol_type_open_curly_brace,
    ycf_symbol_type_end_curly_brace,
    ycf_symbol_type_open_square_bracket,
    ycf_symbol_type_end_square_bracket,
    ycf_symbol_type_not_equal_sign,
    ycf_symbol_type_equal_sign,
    ycf_symbol_type_equal_equal_sign,
    ycf_symbol_type_star,
    ycf_symbol_type_neg,
    ycf_symbol_type_semicolon,
    ycf_symbol_type_comma,
    ycf_symbol_type_pointer_field_access,
    ycf_symbol_type_period,
    ycf_symbol_type_special_code_start,
    ycf_symbol_type_special_code_end,
    ycf_symbol_type_const,
    ycf_symbol_type_void,
    ycf_symbol_type_static,
    ycf_symbol_type_inline,
    ycf_symbol_type_volatile,
    ycf_symbol_type_consume_reds,
    ycf_symbol_type_return,
    ycf_symbol_type_if,
    ycf_symbol_type_else,
    ycf_symbol_type_goto,
    ycf_symbol_type_while,
    ycf_symbol_type_do,
    ycf_symbol_type_for,
    ycf_symbol_type_switch,
    ycf_symbol_type_break,
    ycf_symbol_type_continue,
    ycf_symbol_type_something_else
} ycf_symbol_type;


typedef struct ycf_symbol {
    ycf_symbol_type type;
    int start;
    int stop;
    char* source;
    struct ycf_symbol* whitespace_or_comment_before;
    struct ycf_symbol* next;
} ycf_symbol;

typedef struct symbol_list {
    struct ycf_symbol* head;
    struct ycf_symbol* last;
} ycf_symbol_list;

/* Functions for symbols */

ycf_symbol_list ycf_symbol_list_shallow_copy(ycf_symbol_list n);

ycf_symbol* ycf_symbol_new_something_else(char* text);
ycf_symbol* ycf_symbol_copy_change_text(ycf_symbol* to_copy, char* new_text);
ycf_symbol* ycf_symbol_new_semicolon(void);
ycf_symbol* ycf_symbol_new_star();
ycf_symbol* ycf_symbol_new_parenthesis(void);
ycf_symbol* ycf_symbol_new_comma(void);
ycf_symbol* ycf_symbol_new_open_curly_brace(void);
ycf_symbol* ycf_symbol_new_end_curly_brace(void);
ycf_symbol* ycf_symbol_new_identifier(char* name);
char* ycf_symbol_get_text(ycf_symbol* symbol);
ycf_symbol_list ycf_symbol_list_from_text(char* text);
void ycf_symbol_list_print(char* text);
char* get_symbol_type_text(ycf_symbol_type type);
ycf_symbol* ycf_symbol_copy(ycf_symbol* symbol);
char* ycf_symbol_text_between(ycf_symbol* s1, ycf_symbol* s2);
int ycf_symbol_is_text_eq(ycf_symbol* symbol, char* str);
char* ycf_symbol_list_to_str(ycf_symbol_list* l);
#endif //YIELDING_C_FUN_YCF_SYMBOL_H
