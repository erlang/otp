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

#ifndef YIELDING_C_FUN_YCF_NODE_FUNS_H
#define YIELDING_C_FUN_YCF_NODE_FUNS_H

#include "ycf_utils.h"
#include "ycf_string.h"
#include "ycf_symbol.h"

/* Types for nodes */
typedef enum {
    ycf_node_type_c_file,
    ycf_node_type_variable_definition,
    ycf_node_type_variable_definition_init,
    ycf_node_type_function_definition,
    ycf_node_type_function_declaration,
    ycf_node_type_code_scope,
    ycf_node_type_other,
    ycf_node_type_gen_typedef_struct,
    ycf_node_type_expression,
    ycf_node_type_statement,
    ycf_node_type_return_statement,
    ycf_node_type_assignment,
    ycf_node_type_yield,
    ycf_node_type_consume_reds,
    ycf_node_type_goto,
    ycf_node_type_parentheses_expression,
    ycf_node_type_function_call,
    ycf_node_type_assignment_function_call,
    ycf_node_type_while,
    ycf_node_type_do_while,
    ycf_node_type_for,
    ycf_node_type_switch,
    ycf_node_type_if,
    ycf_node_type_if_else,
    ycf_node_type_comma,
    ycf_node_type_array_bracket,
    ycf_node_type_macro_cmd,
    ycf_node_type_period_field_access,
    ycf_node_type_pointer_field_access,
    ycf_node_type_on_save_yield_state_code,
    ycf_node_type_on_restore_yield_state_code,
    ycf_node_type_on_destroy_state_code,
    ycf_node_type_on_return_code,
    ycf_node_type_on_destroy_state_or_return_code
} ycf_node_type;

struct ycf_node;

typedef struct {
    struct ycf_node* head;
    struct ycf_node* last;
} ycf_node_list;

typedef struct {
    ycf_node_list content;
} ycf_node_c_file;

typedef struct {
    ycf_node_list content;
    ycf_symbol* end_symbol;
} ycf_node_expression;

typedef struct {
    ycf_symbol* start_symbol;
    ycf_node_expression content;
    ycf_symbol* end_symbol;
} ycf_node_parentheses_expression;

typedef struct {
    ycf_symbol_list neg_symbols;
    ycf_symbol* identifier;
    ycf_symbol* start_symbol;
    ycf_node_list parameter_expressions;
    ycf_symbol* end_symbol;
} ycf_node_function_call;

typedef struct array_bracket {
    ycf_symbol* start;
    bool empty;
    ycf_node_expression content;
    ycf_symbol* end;
} ycf_node_array_bracket;


typedef struct {
    ycf_symbol_list type_specifiers;
    ycf_symbol* identifier;
    ycf_node_list array_brackets;
    ycf_symbol* end;
} ycf_node_definition;

typedef struct {
    ycf_node_definition definition;
    ycf_symbol_list initializer_expression;
    ycf_symbol* end;
} ycf_node_definition_init;

typedef struct {
    struct ycf_node* expression;
    ycf_symbol* end_symbol;
} ycf_node_statement;

typedef struct {
    ycf_node_list definition_nodes;
    ycf_node_list other_nodes;
    ycf_symbol* start;
    ycf_symbol* end;
} ycf_node_code_scope;

typedef struct {
    ycf_symbol* while_word;
    ycf_node_parentheses_expression expression;
    struct ycf_node* statement;
} ycf_node_while;

typedef struct {
    ycf_symbol* do_word;
    struct ycf_node* statement;
    ycf_symbol* while_word;
    ycf_node_parentheses_expression expression;
    ycf_symbol* end;
} ycf_node_do_while;

typedef struct {
    ycf_symbol* for_word;
    ycf_symbol* start_parentheses;
    struct ycf_node* init;
    struct ycf_node* stop_cond;
    ycf_symbol* stop_cond_end;
    struct ycf_node* end_exp;
    ycf_symbol* end_parentheses;
    struct ycf_node* statement;
} ycf_node_for;

typedef struct {
    ycf_symbol* switch_word;
    ycf_node_parentheses_expression expression;
    ycf_node_code_scope scope;
} ycf_node_switch;

typedef struct {
    ycf_symbol* if_word;
    ycf_node_parentheses_expression expression;
    struct ycf_node* if_statement;
} ycf_node_if;

typedef struct {
    ycf_node_if if_part;
    ycf_symbol* else_word;
    struct ycf_node* else_statement;
} ycf_node_if_else;

typedef struct {
    ycf_node_expression left_side;
    ycf_symbol* assignment_symbol;
    ycf_node_expression right_side;
    ycf_symbol* end;
} ycf_node_assignment;

/*typedef struct {
  ycf_symbol* id;
} ycf_node_identifier;*/

typedef struct {
    ycf_symbol* yield_symbol;
    ycf_symbol* end_symbol;
} ycf_node_yield;

typedef struct {
    ycf_symbol* consume_reds_symbol;
    ycf_node_parentheses_expression nr_of_reds_expression;
    ycf_symbol* end_symbol;
} ycf_node_consume_reds;

typedef struct {
    ycf_symbol* what;
} ycf_node_other;

typedef struct {
    ycf_node_definition definition;
    int ignore_param_ending;
    ycf_node_list parameters;
    ycf_symbol_list end;
} ycf_node_function_definition;

typedef struct {
    ycf_node_function_definition definition;
    ycf_node_code_scope body;
} ycf_node_function;

typedef struct {
    ycf_node_expression left_side;
    ycf_symbol* assignment_symbol;
    ycf_node_function_call fun_call;
} ycf_node_function_call_assignment;

typedef struct {
    ycf_node_list definition_nodes;
    char* name;
} ycf_node_gen_typedef_struct;

typedef struct {
    ycf_symbol* comma_symbol;
} ycf_node_comma;

typedef struct {
    ycf_symbol* symbol;
} ycf_node_macro_cmd;

typedef struct {
    struct ycf_symbol* start;
    ycf_node_if code;
    struct ycf_symbol* end;
} ycf_node_special_code_block;


typedef struct {
    struct ycf_symbol* goto_symbol;
    struct ycf_symbol* label_symbol;
    struct ycf_symbol* end_symbol;
} ycf_node_goto;

typedef struct {
    struct ycf_symbol* return_symbol;
    struct ycf_node* return_expression;
    struct ycf_symbol* end_symbol;
} ycf_node_return;

typedef struct {
    struct ycf_symbol* period;
    struct ycf_symbol* field_name;
} ycf_node_period_field_access;

typedef struct {
    struct ycf_symbol* pointer;
    struct ycf_symbol* field_name;
} ycf_pointer_field_access;

typedef struct ycf_node {
    ycf_node_type type;
    struct ycf_node* next;
    union {
        ycf_node_c_file c_file;
        ycf_node_definition definition;
        ycf_node_definition_init definition_init;
        ycf_node_code_scope code_scope;
        ycf_node_other other;
        ycf_node_function function;
        ycf_node_function_definition function_definition;
        ycf_node_assignment a;
        ycf_node_gen_typedef_struct gen_typedef_struct;
        ycf_node_yield yield;
        ycf_node_consume_reds consume_reds;
        ycf_node_expression expression;
        ycf_node_parentheses_expression parentheses_expression;
        ycf_node_function_call function_call;
        ycf_node_statement statement;
        ycf_node_while while_n;
        ycf_node_do_while do_while;
        ycf_node_for for_n;
        ycf_node_switch switch_n;
        ycf_node_if if_n;
        ycf_node_if_else if_else;
        ycf_node_function_call_assignment function_call_assignment;
        ycf_node_comma comma;
        ycf_node_array_bracket array_bracket;
        ycf_node_macro_cmd macro_cmd;
        ycf_node_special_code_block special_code_block;
        ycf_node_goto goto_n;
        ycf_node_return return_n;
        ycf_node_period_field_access period_field_access;
        ycf_pointer_field_access pointer_field_access;
    } u;
} ycf_node;

void ycf_node_search_and_replace_statements_in_scope(ycf_node_code_scope* s,
                                                     ycf_node* (*replacer) (ycf_node* canditate,
                                                                            ycf_node_code_scope* candidates_scope,
                                                                            void* context),
                                                     void* context);

char* ycf_node_to_string(ycf_node* n);
ycf_node* ycf_node_new_text_node(char* text);
ycf_node* ycf_node_new();
ycf_node* ycf_node_find_function(ycf_node* c_file_node, char* fun_name);
ycf_node* ycf_node_find_function_declaration(ycf_node* c_file_node, char* fun_name);
ycf_node* ycf_node_find_define_node(ycf_node* c_file_node, char* define_name);
bool ycf_node_is_void_ret_fun(ycf_node* f_node);
ycf_node* ycf_node_get_from_code_scope_text(char* code);
ycf_node* ycf_node_get_function_from_text(char* code);
void ycf_node_print(ycf_node* node, ycf_string_printable_buffer* b);
ycf_node* ycf_node_deep_copy(ycf_node *n);
ycf_node* ycf_node_from_string(char* src);
void ycf_node_insert_scopes_in_complex_statements(ycf_node_code_scope* s);
void ycf_node_normalize_for_var_declarations(ycf_node_code_scope* s);
void ycf_node_move_in_code_var_declarations_to_top(ycf_node_code_scope* s);
void ycf_node_remove_declarations_in_scope(ycf_node_code_scope* s);
ycf_node_list ycf_node_get_declarations_in_scope(ycf_node_code_scope* s);
void ycf_node_normalize_init_definitions_in_scope(ycf_node_code_scope* s);
ycf_node_list ycf_node_get_all_definitions_in_function(ycf_node_function* f);
void ycf_uniqify_local_vars_in_function(ycf_node* function);

ycf_node* ycf_node_new_assignment_from_definition_init(ycf_node_definition_init* n);
ycf_node* ycf_node_new_definition_from_definition_init(ycf_node_definition_init* n);
ycf_node* ycf_node_defenition_new(ycf_symbol_list type_spec,
                                  ycf_symbol* id,
                                  ycf_node_list array_brackets,
                                  ycf_symbol* end);
ycf_node* ycf_node_defenition_with_init_new(ycf_node_definition def,
                                            ycf_symbol_list expression,
                                            ycf_symbol* end);
ycf_node* ycf_node_c_file_new(ycf_node_list content);
ycf_node* ycf_node_function_def_new(ycf_node_definition def_node,
                                    ycf_node_list parameters,
                                    bool ignore_param_ending,
                                    ycf_symbol_list end);
ycf_node* ycf_node_yield_new(ycf_symbol* yield_symbol,
                             ycf_symbol* end);
ycf_node* ycf_node_consume_reds_new(ycf_symbol* consume_reds_symbol,
                                    ycf_node_parentheses_expression nr_of_reds_expression,
                                    ycf_symbol* end_symbol);
ycf_node* ycf_node_other_new(ycf_symbol* other_symbol);
ycf_node* ycf_node_scope_new(ycf_symbol* start,
                             ycf_node_list declaration_nodes,
                             ycf_node_list other_nodes,
                             ycf_symbol* end);
ycf_node* ycf_node_function_new(ycf_node_function_definition fun_def,
                                ycf_node_code_scope body);
ycf_node* ycf_node_function_call_new(ycf_symbol_list neg_symbols,
                                     ycf_symbol* ident,
                                     ycf_symbol* paran_start,
                                     ycf_node_list parameters,
                                     ycf_symbol* paran_end);
ycf_node* ycf_node_paran_expression_new(ycf_symbol* start, ycf_node_expression expr, ycf_symbol* end);
ycf_node* ycf_node_expression_new(ycf_node_list expr);
ycf_node* ycf_node_statement_new(ycf_node* expression,
                                 ycf_symbol* end_symbol);
ycf_node* ycf_node_while_new(ycf_symbol* while_word,
                             ycf_node_parentheses_expression expression,
                             ycf_node* statement);
ycf_node* ycf_node_do_while_new(ycf_symbol* do_word,
                                ycf_node* statm,
                                ycf_symbol* while_word,
                                ycf_node_parentheses_expression expression,
                                ycf_symbol* end);
ycf_node* ycf_node_for_new(ycf_symbol* for_word,
                           ycf_symbol* start_paran,
                           struct ycf_node* init,
                           ycf_node* stop_cond,
                           ycf_symbol* stop_cond_end,
                           ycf_node* end_exp,
                           ycf_symbol* end_paran,
                           ycf_node* statem);
ycf_node* ycf_node_switch_new(ycf_symbol* switch_word,
                              ycf_node_parentheses_expression expression,
                              ycf_node_code_scope scope);
ycf_node* ycf_node_if_new(ycf_symbol* if_word,
                          ycf_node_parentheses_expression expression,
                          struct ycf_node* if_statem);
ycf_node* ycf_node_if_else_new(ycf_node_if if_n,
                               ycf_symbol* else_word,
                               struct ycf_node* else_statement);
ycf_node* ycf_node_fun_call_assignment_new(ycf_node_expression left_side,
                                           ycf_symbol* assignment_symbol,
                                           ycf_node_function_call fun_call);
ycf_node* ycf_node_comma_new(ycf_symbol* comma_symbol);
ycf_node* ycf_node_array_bracket_new(ycf_symbol* start,
                                     bool empty,
                                     ycf_node_expression content,
                                     ycf_symbol* end);
ycf_node* ycf_node_macro_cmd_new(ycf_symbol* macro_symbol);
ycf_node* ycf_node_special_code_block_new(ycf_node_type type,
                                          ycf_symbol* start,
                                          ycf_node_if code,
                                          ycf_symbol* end);
ycf_node* ycf_node_goto_new(ycf_symbol* goto_symbol,
                            ycf_symbol* label_symbol,
                            ycf_symbol* end_symbol);
ycf_node* ycf_node_return_new(ycf_symbol* return_symbol,
                              ycf_node* return_expression,
                              ycf_symbol* end_symbol);
ycf_node* ycf_node_period_field_access_new(ycf_symbol* period,
                                           ycf_symbol* field_name);
ycf_node* ycf_pointer_field_access_new(ycf_symbol* pointer,
                                       ycf_symbol* field_name);

ycf_node_list ycf_node_definition_list_from_string(char* str);
void ycf_node_rename_function(ycf_node_function* f, char* new_name);
bool ycf_node_is_void_ret_ending_fun(ycf_node* f_node);
void ycf_node_remove_const_specifiers_from_declaration(ycf_node* declaration);
void ycf_node_remove_static_specifiers_from_declaration(ycf_node* declaration);
void ycf_node_remove_inline_specifiers_from_declaration(ycf_node* declaration);
void ycf_node_remove_array_size_info_from_declaration(ycf_node* declaration);
void ycf_node_modify_declarations(ycf_node_list declarations, void (*modifer)(ycf_node*));
ycf_symbol_list ycf_node_get_return_type(ycf_node* f_node);
ycf_symbol_list ycf_node_find_function_return_type(ycf_node* c_file_node, char* fun_name);


/* Functions for node lists */

int ycf_node_list_get_item_position(ycf_node_list* list, ycf_node* node);

ycf_node* ycf_node_shallow_copy(ycf_node* n);
ycf_node* ycf_node_list_get_item_at_position(ycf_node_list* list, int pos);

void ycf_node_list_append(ycf_node_list* list, ycf_node* node);
void ycf_node_list_prepend(ycf_node_list* list, ycf_node* node);
void ycf_node_list_insert_before(ycf_node_list* list, ycf_node* before_this, ycf_node* to_insert);
void ycf_node_list_insert_after(ycf_node_list* list, ycf_node* after_this, ycf_node* to_insert);
void ycf_node_list_remove(ycf_node_list* list, ycf_node* to_remove);
void ycf_node_list_replace(ycf_node_list* list, ycf_node* to_replace, ycf_node* replace_with);
void ycf_node_list_concat(ycf_node_list* list1, ycf_node_list* list2);

ycf_node_list ycf_node_list_empty();
ycf_node_list ycf_node_list_shallow_copy(ycf_node_list n);
ycf_node_list ycf_node_list_copy_append(ycf_node_list list, ycf_node* node);
ycf_node_list ycf_node_list_copy_prepend(ycf_node_list list, ycf_node* node);
ycf_node_list ycf_node_list_copy_insert_before(ycf_node_list list, ycf_node* before_this, ycf_node* to_insert);
ycf_node_list ycf_node_list_copy_insert_after(ycf_node_list list, ycf_node* after_this, ycf_node* to_insert);
ycf_node_list ycf_node_list_copy_remove(ycf_node_list list, ycf_node* to_remove);
ycf_node_list ycf_node_list_copy_replace(ycf_node_list list, ycf_node* to_replace, ycf_node* replace_with);
ycf_node_list ycf_node_list_copy_concat(ycf_node_list list1, ycf_node_list list2);
size_t ycf_node_list_length(ycf_node_list list);
char* ycf_node_list_to_string(ycf_node_list* l);
char* ycf_node_get_node_type_string(ycf_node_type t);

ycf_node_assignment* ycf_node_get_assignment(ycf_node* n);
void ycf_node_print_node_type(ycf_node_type t);
void ycf_node_normalize_function(ycf_node* fun);
void ycf_node_remove_unecessary_scopes(ycf_node_code_scope* s);

#endif //YIELDING_C_FUN_YCF_NODE_FUNS_H
