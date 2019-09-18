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
#include <stdio.h>
#include <stdint.h>
#include "ycf_symbol.h"
#include "ycf_node.h"
#include "ycf_string.h"
#include "ycf_lists.h"
#include "ycf_yield_fun.h"
#include "ycf_parser.h"

GENERATE_LIST_FUNCTIONS(ycf_node)

#define YCF_UNIQ_VARIABLE_NAME_NUMBER_PREFIX "_N"

static int ycf_uniq_variable_name_number_counter = 0;

static void uniqify_local_vars_in_node_rename_var_helper(ycf_node* n,
                                                         char* old_name,
                                                         char* new_name);
static void uniqify_local_vars_in_scope_rename_var_helper(ycf_node_code_scope* s,
                                                          char* old_name,
                                                          char* new_name);


static void uniqify_local_vars_in_expression_rename_var_helper(ycf_node_expression* n,
                                                               char* old_name,
                                                               char* new_name){
    ycf_node* current = n->content.head;
    while(current != NULL){
        uniqify_local_vars_in_node_rename_var_helper(current,
                                                     old_name,
                                                     new_name);
        current = current->next;
    }
}

static void uniqify_local_vars_in_paran_expression_rename_var_helper(ycf_node_parentheses_expression* n,
                                                                     char* old_name,
                                                                     char* new_name){
    uniqify_local_vars_in_expression_rename_var_helper(&n->content,
                                                       old_name,
                                                       new_name);
}

static void uniqify_local_vars_in_fun_call_rename_var_helper(ycf_node_function_call* n,
                                                             char* old_name,
                                                             char* new_name)
{
    ycf_node* e = n->parameter_expressions.head;
    while(e != NULL){
        uniqify_local_vars_in_node_rename_var_helper(e, old_name, new_name);
        e = e->next;
    }
    if(ycf_symbol_is_text_eq(n->identifier, old_name)){
        n->identifier = ycf_symbol_copy_change_text(n->identifier, new_name);
    }
}

static void uniqify_local_vars_in_assignment_fun_call_rename_var_helper(ycf_node_function_call_assignment* n,
                                                                        char* old_name,
                                                                        char* new_name)
{
    uniqify_local_vars_in_expression_rename_var_helper(&n->left_side, old_name, new_name);
    uniqify_local_vars_in_fun_call_rename_var_helper(&n->fun_call, old_name, new_name);
}

static void uniqify_local_vars_in_node_rename_var_helper(ycf_node* n,
                                                         char* old_name,
                                                         char* new_name){
    if(n == NULL){
        return;
    } else if(n->type == ycf_node_type_code_scope){
        uniqify_local_vars_in_scope_rename_var_helper(&n->u.code_scope, old_name, new_name);
    } else if(n->type == ycf_node_type_on_restore_yield_state_code ||
              n->type == ycf_node_type_on_save_yield_state_code ||
              n->type == ycf_node_type_on_destroy_state_code ||
              n->type == ycf_node_type_on_return_code ||
              n->type == ycf_node_type_on_destroy_state_or_return_code){
        uniqify_local_vars_in_scope_rename_var_helper(&n->u.special_code_block.code.if_statement->u.code_scope, old_name, new_name);
    } else if(n->type == ycf_node_type_other &&
              n->u.other.what->type == ycf_symbol_type_identifier &&
              ycf_symbol_is_text_eq(n->u.other.what, old_name)){
        n->u.other.what =
                ycf_symbol_copy_change_text(n->u.other.what, new_name);
    } else if(n->type == ycf_node_type_assignment){
        uniqify_local_vars_in_expression_rename_var_helper(&n->u.a.left_side, old_name, new_name);
        uniqify_local_vars_in_expression_rename_var_helper(&n->u.a.right_side, old_name, new_name);
    } else if (n->type == ycf_node_type_function_call){
        uniqify_local_vars_in_fun_call_rename_var_helper(&n->u.function_call, old_name, new_name);
    } else if (n->type == ycf_node_type_statement){
        ycf_node* e = n->u.statement.expression;
        uniqify_local_vars_in_node_rename_var_helper(e, old_name, new_name);
    } else if (n->type == ycf_node_type_return_statement && n->u.return_n.return_expression != NULL){
        ycf_node* e = n->u.return_n.return_expression;
        uniqify_local_vars_in_node_rename_var_helper(e, old_name, new_name);
    } else if (n->type == ycf_node_type_expression){
        uniqify_local_vars_in_expression_rename_var_helper(&n->u.expression, old_name, new_name);
    } else if (n->type == ycf_node_type_assignment_function_call){
        uniqify_local_vars_in_assignment_fun_call_rename_var_helper(&n->u.function_call_assignment, old_name, new_name);
    }else if (n->type == ycf_node_type_parentheses_expression){
        uniqify_local_vars_in_paran_expression_rename_var_helper(&n->u.parentheses_expression, old_name, new_name);
    } else if (n->type == ycf_node_type_if){
        uniqify_local_vars_in_paran_expression_rename_var_helper(&n->u.if_n.expression, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.if_n.if_statement, old_name, new_name);
    } else if (n->type == ycf_node_type_if_else){
        uniqify_local_vars_in_paran_expression_rename_var_helper(&n->u.if_else.if_part.expression, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.if_else.if_part.if_statement, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.if_else.else_statement, old_name, new_name);
    } else if (n->type == ycf_node_type_while){
        uniqify_local_vars_in_paran_expression_rename_var_helper(&n->u.while_n.expression, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.while_n.statement, old_name, new_name);
    } else if (n->type == ycf_node_type_do_while){
        uniqify_local_vars_in_paran_expression_rename_var_helper(&n->u.do_while.expression, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.do_while.statement, old_name, new_name);
    } else if (n->type == ycf_node_type_switch){
        uniqify_local_vars_in_paran_expression_rename_var_helper(&n->u.switch_n.expression, old_name, new_name);
        uniqify_local_vars_in_scope_rename_var_helper(&n->u.switch_n.scope, old_name, new_name);
    } else if (n->type == ycf_node_type_for){
        uniqify_local_vars_in_node_rename_var_helper(n->u.for_n.init, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.for_n.stop_cond, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.for_n.end_exp, old_name, new_name);
        uniqify_local_vars_in_node_rename_var_helper(n->u.for_n.statement, old_name, new_name);
    }
}

static void uniqify_local_vars_in_scope_rename_var_helper(ycf_node_code_scope* s,
                                                   char* old_name,
                                                   char* new_name){
    ycf_node* current = s->other_nodes.head;
    while(current != NULL){
        uniqify_local_vars_in_node_rename_var_helper(current, old_name, new_name);
        current = current->next;
    }
}



static void uniqify_definitions_in_scope(ycf_node_list* definition_nodes, ycf_node_code_scope* s){
    ycf_node* current = definition_nodes->head;
    while(current != NULL){
        ycf_symbol * old_symbol = NULL;
        if(current->type == ycf_node_type_variable_definition){
            old_symbol = current->u.definition.identifier;
        }else if(current->type == ycf_node_type_variable_definition_init){
            old_symbol = current->u.definition_init.definition.identifier;
        }
        ycf_uniq_variable_name_number_counter++;
        {
            char* old_name = ycf_symbol_get_text(old_symbol);
            char* new_name = ycf_string_new("%s%s%lu", old_name, YCF_UNIQ_VARIABLE_NAME_NUMBER_PREFIX,
                                            ycf_uniq_variable_name_number_counter);
            if(current->type == ycf_node_type_variable_definition){
                current->u.definition.identifier = ycf_symbol_copy_change_text(old_symbol, new_name);
            }else if(current->type == ycf_node_type_variable_definition_init){
                current->u.definition_init.definition.identifier = ycf_symbol_copy_change_text(old_symbol, new_name);
            }
            uniqify_local_vars_in_scope_rename_var_helper(s, old_name, new_name);
            current = current->next;
        }
    }
}

static void uniqify_local_vars_in_scope(ycf_node_code_scope* s);

static void uniqify_local_vars_in_node(ycf_node* n){
    if(n->type == ycf_node_type_code_scope){
        uniqify_local_vars_in_scope(&n->u.code_scope);
    } else if(n->type == ycf_node_type_on_restore_yield_state_code){
        uniqify_local_vars_in_scope(&n->u.special_code_block.code.if_statement->u.code_scope);
    } else if(n->type == ycf_node_type_on_save_yield_state_code ||
              n->type == ycf_node_type_on_save_yield_state_code ||
              n->type == ycf_node_type_on_destroy_state_code ||
              n->type == ycf_node_type_on_return_code ||
              n->type == ycf_node_type_on_destroy_state_or_return_code){
        uniqify_local_vars_in_scope(&n->u.special_code_block.code.if_statement->u.code_scope);
    } else if (n->type == ycf_node_type_if){
        uniqify_local_vars_in_node(n->u.if_n.if_statement);
    } else if (n->type == ycf_node_type_if_else){
        uniqify_local_vars_in_node(n->u.if_else.if_part.if_statement);
        uniqify_local_vars_in_node(n->u.if_else.else_statement);
    } else if (n->type == ycf_node_type_while){
        uniqify_local_vars_in_node(n->u.while_n.statement);
    } else if (n->type == ycf_node_type_do_while){
        uniqify_local_vars_in_node(n->u.do_while.statement);
    } else if (n->type == ycf_node_type_switch){
        uniqify_local_vars_in_scope(&n->u.switch_n.scope);
    } else if (n->type == ycf_node_type_for){
        uniqify_local_vars_in_node(n->u.for_n.statement);
    }
}

static void uniqify_local_vars_in_scope(ycf_node_code_scope* s){
    ycf_node* current = s->other_nodes.head;
    while(current != NULL){
        uniqify_local_vars_in_node(current);
        current = current->next;
    }
    uniqify_definitions_in_scope(&s->definition_nodes, s);
}

void ycf_uniqify_local_vars_in_function(ycf_node* function){
    uniqify_local_vars_in_scope(&function->u.function.body);
    uniqify_definitions_in_scope(&function->u.function.definition.parameters,
                                 &function->u.function.body);
}


ycf_node* ycf_node_new_assignment_from_definition_init(ycf_node_definition_init* n){
  ycf_node* res = ycf_malloc(sizeof(ycf_node));
  res->type = ycf_node_type_assignment;
  res->next = NULL;
  res->u.a.assignment_symbol = n->definition.end;
  {
    ycf_symbol* ident = ycf_symbol_copy(n->definition.identifier);
    ident->whitespace_or_comment_before =
      n->definition.type_specifiers.head->whitespace_or_comment_before;
    res->u.a.left_side = parse_expression(ident).result->u.expression;
  }
  res->u.a.right_side = parse_expression(n->initializer_expression.head).result->u.expression;
  res->u.a.end = ycf_symbol_new_semicolon();
  return res;
}

ycf_node* ycf_node_new_definition_from_definition_init(ycf_node_definition_init* n){
  ycf_node* res = ycf_malloc(sizeof(ycf_node));
  res->type = ycf_node_type_variable_definition;
  res->next = NULL;
  res->u.definition = n->definition;
  res->u.definition.end = ycf_symbol_new_semicolon();
  return res;
}

ycf_node* ycf_node_code_scope_shallow_copy(ycf_node* n){
    ycf_node* new_scope = ycf_node_shallow_copy(n);
    new_scope->u.code_scope.definition_nodes = ycf_node_list_shallow_copy(new_scope->u.code_scope.definition_nodes);
    new_scope->u.code_scope.other_nodes = ycf_node_list_shallow_copy(new_scope->u.code_scope.other_nodes);
    return new_scope;
}


void ycf_node_search_and_replace_statements_in_node(ycf_node* n, ycf_node* (*replacer) (ycf_node*,
                                                                                        ycf_node_code_scope*,
                                                                                        void*), void* context,
                                                                                        ycf_node_code_scope* s){
    if(n->type == ycf_node_type_code_scope){
        ycf_node_search_and_replace_statements_in_scope(&n->u.code_scope, replacer, context);
    } else if(n->type == ycf_node_type_on_destroy_state_code ||
              n->type == ycf_node_type_on_restore_yield_state_code ||
              n->type == ycf_node_type_on_save_yield_state_code ||
              n->type == ycf_node_type_on_destroy_state_or_return_code ||
              n->type == ycf_node_type_on_return_code) {
        ycf_node* replace_candidate = n->u.special_code_block.code.if_statement;
        ycf_node* possible_replacement = replacer(replace_candidate, s, context);
        if(replace_candidate != possible_replacement){
            n->u.special_code_block.code.if_statement = possible_replacement;
        } else {
            ycf_node_search_and_replace_statements_in_node(n->u.special_code_block.code.if_statement,
                                                           replacer,
                                                           context,
                                                           s);
        }
    } else if (n->type == ycf_node_type_if){
        ycf_node_search_and_replace_statements_in_node(n->u.if_n.if_statement, replacer, context, s);
    } else if (n->type == ycf_node_type_if_else){
        ycf_node_search_and_replace_statements_in_node(n->u.if_else.if_part.if_statement, replacer, context, s);
        ycf_node_search_and_replace_statements_in_node(n->u.if_else.else_statement, replacer, context, s);
    } else if (n->type == ycf_node_type_while){
        ycf_node_search_and_replace_statements_in_node(n->u.while_n.statement, replacer, context, s);
    } else if (n->type == ycf_node_type_do_while){
        ycf_node_search_and_replace_statements_in_node(n->u.do_while.statement, replacer, context, s);
    } else if (n->type == ycf_node_type_switch){
        ycf_node_search_and_replace_statements_in_scope(&n->u.switch_n.scope, replacer, context);
    } else if (n->type == ycf_node_type_for){
        ycf_node_search_and_replace_statements_in_node(n->u.for_n.statement, replacer, context, s);
    }
}

void ycf_node_search_and_replace_statements_in_scope(ycf_node_code_scope* s,
                                                     ycf_node* (*replacer) (ycf_node* canditate,
                                                                            ycf_node_code_scope* candidates_scope,
                                                                            void* context),
                                                     void* context){
    ycf_node* n = s->other_nodes.head;
    while(n != NULL){
        ycf_node* replace_candidate = n;
        ycf_node* possible_replacement = replacer(replace_candidate, s, context);
        if(replace_candidate != possible_replacement){
            ycf_node_list_replace(&s->other_nodes, replace_candidate, possible_replacement);
        } else {
            ycf_node_search_and_replace_statements_in_node(n, replacer, context, s);
        }
        n = n->next;
    }
    return;
}

ycf_node_code_scope ycf_node_copy_search_and_replace_statements_in_scope(ycf_node_code_scope s,
                                                                         ycf_node* (*replacer) (ycf_node*,
                                                                                                ycf_node_code_scope*,
                                                                                                void*),
                                                                         void* context){
    ycf_node_search_and_replace_statements_in_scope(&s, replacer, context);
    return s;
}

char* ycf_node_to_string(ycf_node* n){
    ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
    ycf_node_print(n, b);
    return b->buffer;
}

char* ycf_node_list_to_string(ycf_node_list* l){
    ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
    ycf_node* current = l->head;
    while(current != NULL){
      ycf_node_print(current, b);
      current = current->next;
    }
    return b->buffer;
}

ycf_node* ycf_node_new_text_node(char* text){
    ycf_node* n = ycf_node_new();
    n->type = ycf_node_type_other;
    n->u.other.what = ycf_symbol_new_something_else(text);
    return n;
}

ycf_node* ycf_node_new(){
    ycf_node* new = ycf_malloc(sizeof(ycf_node));
    new->next = NULL;
    return new;
}

ycf_node* ycf_node_find_function(ycf_node* c_file_node, char* fun_name){
    ycf_node* current = c_file_node->u.c_file.content.head;
    while(current != NULL){
        if(current->type == ycf_node_type_function_definition &&
                ycf_symbol_is_text_eq(current->u.function.definition.definition.identifier,
                                      fun_name)){
            return current;
        }
        current = current->next;
    }
    return NULL;
}

ycf_node* ycf_node_find_function_declaration(ycf_node* c_file_node, char* fun_name){
    ycf_node* current = c_file_node->u.c_file.content.head;
    while(current != NULL){
        if(current->type == ycf_node_type_function_declaration &&
           ycf_symbol_is_text_eq(current->u.function_definition.definition.identifier,
                                 fun_name)){
            return current;
        }
        current = current->next;
    }
    return NULL;
}

ycf_node* ycf_node_find_define_node(ycf_node* c_file_node, char* define_name){
    ycf_node* current = c_file_node->u.c_file.content.head;
    while(current != NULL){
        if(current->type == ycf_node_type_other &&
                ycf_symbol_is_text_eq(current->u.other.what,
                                      ycf_string_new("#define %s", define_name))){
            return current;
        }
        current = current->next;
    }
    return NULL;
}

bool ycf_node_is_void_ret_fun(ycf_node* f_node){
    ycf_symbol_list type = f_node->u.function.definition.definition.type_specifiers;
    return type.last->type == ycf_symbol_type_void;
}

ycf_node* ycf_node_get_from_code_scope_text(char* code){
    char* f_code = ycf_string_new("void f(){\n"
                                  "{\n"
                                  "%s\n"
                                  "}\n"
                                  "}\n",
                                  code);
    ycf_symbol_list symbols = ycf_symbol_list_from_text(f_code);
    ycf_node* tree = get_abstract_syntax_tree_root(&symbols);
    if(tree->u.c_file.content.head->type != ycf_node_type_function_definition){
        printf("NOT A FUNCTION\n");
        exit(1);
    }
    return tree->u.c_file.content.head->u.function.body.other_nodes.head;
}

ycf_node* ycf_node_get_function_from_text(char* code){
    char* f_code = ycf_string_new("%s",
                                  code);
    ycf_symbol_list symbols = ycf_symbol_list_from_text(f_code);
    ycf_node* tree = get_abstract_syntax_tree_root(&symbols);
    if(tree->u.c_file.content.head->type != ycf_node_type_function_definition){
        printf("ycf_get_function_from_text: NOT A FUNCTION\n");
        exit(1);
    }
    return tree->u.c_file.content.head;
}

ycf_node* ycf_node_from_string(char* src){
    ycf_symbol_list symbols = ycf_symbol_list_from_text(src);
    ycf_node* tree = get_abstract_syntax_tree_root(&symbols);
    return tree;
}

ycf_node* mk_scope_wrapper(ycf_node* statement){
    return ycf_node_get_from_code_scope_text(ycf_string_new("{\n"
                                                            "   %s\n"
                                                            "}\n",
                                                            ycf_node_to_string(statement)));
}

static ycf_node* scope_inserter(ycf_node* candidate, ycf_node_code_scope* s, void* context){
    (void)context;
    (void)s;
    if (candidate->type == ycf_node_type_while){
        if(candidate->u.while_n.statement->type == ycf_node_type_code_scope){
            ycf_node_insert_scopes_in_complex_statements(&candidate->u.while_n.statement->u.code_scope);
        }
        candidate->u.while_n.statement = mk_scope_wrapper(candidate->u.while_n.statement);
        return candidate;
    } else if (candidate->type == ycf_node_type_do_while){
        if(candidate->u.do_while.statement->type == ycf_node_type_code_scope){
            ycf_node_insert_scopes_in_complex_statements(&candidate->u.do_while.statement->u.code_scope);
        }
        candidate->u.do_while.statement = mk_scope_wrapper(candidate->u.do_while.statement);
        return candidate;
    } else if (candidate->type == ycf_node_type_for){
        if(candidate->u.for_n.statement->type == ycf_node_type_code_scope){
            ycf_node_insert_scopes_in_complex_statements(&candidate->u.for_n.statement->u.code_scope);
        }
        candidate->u.for_n.statement = mk_scope_wrapper(candidate->u.for_n.statement);
        return candidate;
    } else if (candidate->type == ycf_node_type_if){
        if(candidate->u.if_n.if_statement->type == ycf_node_type_code_scope){
            ycf_node_insert_scopes_in_complex_statements(&candidate->u.if_n.if_statement->u.code_scope);
        }
        candidate->u.if_n.if_statement = mk_scope_wrapper(candidate->u.if_n.if_statement);
        return candidate;
    } else if (candidate->type == ycf_node_type_if_else){
        if(candidate->u.if_else.if_part.if_statement->type == ycf_node_type_code_scope){
            ycf_node_insert_scopes_in_complex_statements(&candidate->u.if_else.if_part.if_statement->u.code_scope);
        }
        candidate->u.if_else.if_part.if_statement = mk_scope_wrapper(candidate->u.if_else.if_part.if_statement);
        if(candidate->u.if_else.else_statement->type == ycf_node_type_code_scope){
            ycf_node_insert_scopes_in_complex_statements(&candidate->u.if_else.else_statement->u.code_scope);
        }
        candidate->u.if_else.else_statement = mk_scope_wrapper(candidate->u.if_else.else_statement);
        return candidate;
    } else {
        return candidate;
    }
}

void ycf_node_insert_scopes_in_complex_statements(ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    scope_inserter,
                                                    NULL);
}


static ycf_node* scope_remover(ycf_node* candidate, ycf_node_code_scope* s, void* context){
  uintptr_t* removed_scopes = context;
  if(candidate->type == ycf_node_type_code_scope &&
     ycf_node_list_length(candidate->u.code_scope.definition_nodes) == 0 &&
     ycf_node_list_length(candidate->u.code_scope.other_nodes) == 1 &&
     candidate->u.code_scope.other_nodes.head->type == ycf_node_type_code_scope) {
    *removed_scopes = *removed_scopes + 1;
    return candidate->u.code_scope.other_nodes.head;
  }
  return candidate;
}

void ycf_node_remove_unecessary_scopes(ycf_node_code_scope* s){
  uintptr_t removed_scopes;
  do{
    removed_scopes = 0;
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    scope_remover,
                                                    &removed_scopes);
  }while(removed_scopes > 0);
}

static ycf_node* for_declaration_normalizer(ycf_node* candidate,
                                            ycf_node_code_scope* s,
                                            void* context){
    (void)context;
    (void)s;
    if (candidate->type == ycf_node_type_for){
        if(candidate->u.for_n.init != NULL &&
           (candidate->u.for_n.init->type == ycf_node_type_variable_definition ||
            candidate->u.for_n.init->type == ycf_node_type_variable_definition_init)){
            if(candidate->u.for_n.statement->type == ycf_node_type_code_scope){
                ycf_node_normalize_for_var_declarations(&candidate->u.for_n.statement->u.code_scope);
            }
            ycf_string_printable_buffer * b = ycf_string_printable_buffer_new();
            ycf_node_print(candidate->u.for_n.init, b);
            candidate->u.for_n.init = ycf_node_statement_new(ycf_node_expression_new(ycf_node_list_empty()),
                                                             ycf_symbol_new_semicolon());
            ycf_node_print(candidate, b);
            return ycf_node_get_from_code_scope_text(b->buffer);
        }else {
            return candidate;
        }

    } else {
        return candidate;
    }
}

void ycf_node_normalize_for_var_declarations(ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    for_declaration_normalizer,
                                                    NULL);
}



static ycf_node* ycf_node_move_in_code_var_declarations_to_top_mover(ycf_node* candidate,
                                                                     ycf_node_code_scope* candidates_scope,
                                                                     void* context){
    (void)context;
    if (candidate->type == ycf_node_type_variable_definition) {
      ycf_node_list_append(&candidates_scope->definition_nodes,
                           ycf_node_shallow_copy(candidate));
      return ycf_node_get_from_code_scope_text(ycf_string_new("\n/* moved declaration (%s)*/\n",
                                                              ycf_symbol_get_text(candidate->u.definition.identifier)));
    } else if (candidate->type == ycf_node_type_variable_definition_init) {
      ycf_node_list_append(&candidates_scope->definition_nodes,
                           ycf_node_new_definition_from_definition_init(&candidate->u.definition_init));
      return ycf_node_new_assignment_from_definition_init(&candidate->u.definition_init);
    } else {
        return candidate;
    }
}

void ycf_node_move_in_code_var_declarations_to_top(ycf_node_code_scope* s){
  ycf_node_search_and_replace_statements_in_scope(s,
                                                  ycf_node_move_in_code_var_declarations_to_top_mover,
                                                  NULL);
}

static ycf_node* ycf_node_remove_declarations_in_scope_helper(ycf_node* candidate,
                                                              ycf_node_code_scope* candidates_scope,
                                                              void* context){
    (void)context;
    candidates_scope->definition_nodes = ycf_node_list_empty();
    return candidate;
}

void ycf_node_remove_declarations_in_scope(ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    ycf_node_remove_declarations_in_scope_helper,
                                                    NULL);
}

static void normalize_init_definitions_in_scope_helper_insert_assignments(ycf_node* current_def,
                                                                          ycf_node_list* other_nodes){
    if(current_def == NULL){
        return;
    }
    normalize_init_definitions_in_scope_helper_insert_assignments(current_def->next,
                                                                  other_nodes);
    if(current_def->type == ycf_node_type_variable_definition_init){
        ycf_node* assignment = ycf_node_new_assignment_from_definition_init(&current_def->u.definition_init);
        ycf_node_list_prepend(other_nodes, assignment);
        return;
    }else{
        return;
    }
}

static ycf_node* ycf_node_normalize_init_definitions_in_scope_helper(ycf_node* candidate,
                                                                     ycf_node_code_scope* candidates_scope,
                                                                     void* context){
    (void)context;
    (void)candidates_scope;
    ycf_node_code_scope* s = NULL;
    if(candidate == NULL){
        s = context;
    } else if(candidate->type == ycf_node_type_code_scope){
        s = &candidate->u.code_scope;

    } else if(candidate->type == ycf_node_type_switch){
        s = &candidate->u.switch_n.scope;
    }
    if(s != NULL) {
        ycf_node * current = s->definition_nodes.head;
        normalize_init_definitions_in_scope_helper_insert_assignments(s->definition_nodes.head,
                                                                      &s->other_nodes);
        while (current != NULL) {
            if (current->type == ycf_node_type_variable_definition_init) {
                ycf_node *def_node = ycf_node_new_definition_from_definition_init(&current->u.definition_init);
                ycf_node_list_replace(&s->definition_nodes, current, def_node);
            }
            current = current->next;
        }
    }
    return candidate;
}

void ycf_node_normalize_init_definitions_in_scope(ycf_node_code_scope* s){
    ycf_node_normalize_init_definitions_in_scope_helper(NULL,
                                                        NULL,
                                                        s);
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    ycf_node_normalize_init_definitions_in_scope_helper,
                                                    NULL);
}


static ycf_node* ycf_node_get_declarations_in_scope_helper(ycf_node* candidate,
                                                           ycf_node_code_scope* candidates_scope,
                                                           void* context){
    ycf_node_list* list = (ycf_node_list*)context;
    if(candidate->type == ycf_node_type_code_scope){
        ycf_node_list to_append = ycf_node_list_shallow_copy(candidate->u.code_scope.definition_nodes);
        ycf_node_list_concat(list, &to_append);
    } else if(candidate->type == ycf_node_type_switch){
        ycf_node_list to_append = ycf_node_list_shallow_copy(candidate->u.switch_n.scope.definition_nodes);
        ycf_node_list_concat(list, &to_append);
    }
    return candidate;
}

ycf_node_list ycf_node_get_declarations_in_scope(ycf_node_code_scope* s){
    ycf_node_list res = s->definition_nodes;
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    ycf_node_get_declarations_in_scope_helper,
                                                    &res);
    return res;
}

ycf_node_list ycf_node_definition_list_from_string(char* str){
     return ycf_node_get_from_code_scope_text(str)->u.code_scope.definition_nodes;
}

void ycf_node_rename_function(ycf_node_function* f, char* new_name){
    f->definition.definition.identifier =
            ycf_symbol_copy_change_text(f->definition.definition.identifier,
                                        new_name);
}

static bool is_scope_ends_with_void_ret(ycf_node_code_scope* scope_node){
    if(scope_node->other_nodes.last->type == ycf_node_type_code_scope){
        return is_scope_ends_with_void_ret(&scope_node->other_nodes.last->u.code_scope);
    }else{
        ycf_node* prev = NULL;
        ycf_node* current = scope_node->other_nodes.head;
        while(current != NULL){
            prev = current;
            current = current->next;
        }
        return
                prev != NULL &&
                current != NULL &&
                prev->type == ycf_node_type_other &&
                prev->u.other.what->type == ycf_symbol_type_return &&
                current->type == ycf_node_type_other &&
                current->u.other.what->type == ycf_symbol_type_semicolon;
    }
}

bool ycf_node_is_void_ret_ending_fun(ycf_node* f_node){
    return is_scope_ends_with_void_ret(&f_node->u.function.body);
}

ycf_node_list ycf_node_get_all_definitions_in_function(ycf_node_function* f){
    ycf_node_list all = ycf_node_get_declarations_in_scope(&f->body);
    ycf_node_list all_copy = ycf_node_list_shallow_copy(all);
    ycf_node_list params = f->definition.parameters;
    ycf_node_list_concat(&all_copy, &params);
    return all_copy;
}

void ycf_node_remove_const_specifiers_from_declaration(ycf_node* declaration){
    ycf_symbol* current = declaration->u.definition.type_specifiers.head;
    while(current != NULL) {
        if(current->type == ycf_symbol_type_const){
            ycf_symbol_list_remove(&declaration->u.definition.type_specifiers,
                                   current);
        }
        current = current->next;
    }
}

void ycf_node_remove_static_specifiers_from_declaration(ycf_node* declaration){
    ycf_symbol* current = declaration->u.definition.type_specifiers.head;
    while(current != NULL) {
        if(current->type == ycf_symbol_type_static){
            ycf_symbol_list_remove(&declaration->u.definition.type_specifiers,
                                   current);
        }
        current = current->next;
    }
}

void ycf_node_remove_inline_specifiers_from_declaration(ycf_node* declaration){
    ycf_symbol* current = declaration->u.definition.type_specifiers.head;
    while(current != NULL) {
        if(current->type == ycf_symbol_type_inline){
            ycf_symbol_list_remove(&declaration->u.definition.type_specifiers,
                                   current);
        }
        current = current->next;
    }
}

void ycf_node_remove_array_size_info_from_declaration(ycf_node* declaration){
    ycf_node* current = declaration->u.definition.array_brackets.head;
    while(current != NULL) {
      current->u.array_bracket.empty = true;
      current = current->next;
    }
}

void ycf_node_modify_declarations(ycf_node_list declarations, void (*modifer)(ycf_node*)){
    ycf_node* current = declarations.head;
    while(current != NULL) {
        modifer(current);
        current = current->next;
    }
}

ycf_symbol_list ycf_node_get_return_type(ycf_node* f_node_or_f_dec_node){
  ycf_node_function_definition def;
  ycf_node * tmp;
  if (f_node_or_f_dec_node->type == ycf_node_type_function_declaration) {
    def = f_node_or_f_dec_node->u.function_definition;
  } else {
    def = f_node_or_f_dec_node->u.function.definition;
  }
  tmp =
    ycf_node_defenition_new(ycf_symbol_list_shallow_copy(def.definition.type_specifiers),
                            ycf_symbol_new_identifier(ycf_string_new("tmp")),
                            ycf_node_list_empty(),
                            ycf_symbol_new_semicolon());
  ycf_node_remove_inline_specifiers_from_declaration(tmp);
  ycf_node_remove_static_specifiers_from_declaration(tmp);
  return tmp->u.definition.type_specifiers;
}

ycf_symbol_list ycf_node_find_function_return_type(ycf_node* c_file_node, char* fun_name)
{
  ycf_node* f_node_or_f_dec_node = ycf_node_find_function(c_file_node, fun_name);
  if (f_node_or_f_dec_node == NULL) {
    f_node_or_f_dec_node = ycf_node_find_function_declaration(c_file_node, fun_name);
  }
  if (f_node_or_f_dec_node == NULL) {
    fprintf(stderr, "ycf_node_find_function_return_type: Could not find function declaration or definition: %s\n", fun_name);
    exit(1);
  }
  return ycf_node_get_return_type(f_node_or_f_dec_node);
}

char* ycf_node_get_node_type_string(ycf_node_type t){
  switch (t){
  case ycf_node_type_c_file:
    return "ycf_node_type_c_file";
  case ycf_node_type_variable_definition:
    return "ycf_node_type_variable_definition";
  case ycf_node_type_variable_definition_init:
    return "ycf_node_type_variable_definition_init";
  case ycf_node_type_function_definition:
    return "ycf_node_type_function_definition";
  case ycf_node_type_function_declaration:
    return "ycf_node_type_function_declaration";
  case ycf_node_type_code_scope:
    return "ycf_node_type_code_scope";
  case ycf_node_type_other:
    return "ycf_node_type_other";
  case ycf_node_type_gen_typedef_struct:
    return "ycf_node_type_gen_typedef_struct";
  case ycf_node_type_expression:
    return "ycf_node_type_expression";
  case ycf_node_type_statement:
    return "ycf_node_type_statement";
  case ycf_node_type_return_statement:
    return "ycf_node_type_return_statement";
  case ycf_node_type_assignment:
    return "ycf_node_type_assignment";
  case ycf_node_type_yield:
    return "ycf_node_type_yield";
  case ycf_node_type_consume_reds:
    return "ycf_node_type_consume_reds";
  case ycf_node_type_goto:
    return "ycf_node_type_goto";
  case ycf_node_type_parentheses_expression:
    return "ycf_node_type_parentheses_expression";
  case ycf_node_type_function_call:
    return "ycf_node_type_function_call";
  case ycf_node_type_assignment_function_call:
    return "ycf_node_type_assignment_function_call";
  case ycf_node_type_while:
    return "ycf_node_type_while";
  case ycf_node_type_do_while:
    return "ycf_node_type_do_while";
  case ycf_node_type_for:
    return "ycf_node_type_for";
  case ycf_node_type_switch:
    return "ycf_node_type_switch";
  case ycf_node_type_if:
    return "ycf_node_type_if";
  case ycf_node_type_if_else:
    return "ycf_node_type_if_else";
  case ycf_node_type_comma:
    return "ycf_node_type_comma";
  case ycf_node_type_array_bracket:
    return "ycf_node_type_array_bracket";
  case ycf_node_type_macro_cmd:
    return "ycf_node_type_macro_cmd";
  case ycf_node_type_period_field_access:
    return "ycf_node_type_period_field_access";
  case ycf_node_type_pointer_field_access:
    return "ycf_node_type_pointer_field_access";
  case ycf_node_type_on_save_yield_state_code:
    return "ycf_node_type_on_save_yield_state_code";
  case ycf_node_type_on_restore_yield_state_code:
    return "ycf_node_type_on_restore_yield_state_code";
  case ycf_node_type_on_destroy_state_code:
    return "ycf_node_type_on_destroy_state_code";
  case ycf_node_type_on_return_code:
    return "ycf_node_type_on_return_code";
  case ycf_node_type_on_destroy_state_or_return_code:
    return "ycf_node_type_on_destroy_state_or_return_code";
  }
  return "unrecognized type";
}

void ycf_node_print_node_type(ycf_node_type t){
  printf("%s\n",ycf_node_get_node_type_string(t));
}

ycf_node_assignment* ycf_node_get_assignment(ycf_node* n) {
  if(n->type == ycf_node_type_assignment){
      return &n->u.a;
  } else if (n->type == ycf_node_type_statement &&
             n->u.statement.expression->u.expression.content.head->type == ycf_node_type_assignment){
    return &n->u.statement.expression->u.expression.content.head->u.a;
  } else {
    fprintf(stderr,
            "Trying to get %s from a node of type %s\n",
            "ycf_node_type_assignment",
            ycf_node_get_node_type_string(n->type));
    exit(1);
  }
}

void ycf_node_normalize_function(ycf_node* fun){
      /* Remove array size info from parameters */
    ycf_node_modify_declarations(fun->u.function.definition.parameters,
                                 ycf_node_remove_array_size_info_from_declaration);
    /* Insert scope in nest statements (e.g., if(1) print(); -> if(1) {print();}) */
    ycf_node_insert_scopes_in_complex_statements(&fun->u.function.body);
    /* Move out declarations from for loops */
    ycf_node_normalize_for_var_declarations(&fun->u.function.body);
    /* Move in code declations to top of scope */
    ycf_node_move_in_code_var_declarations_to_top(&fun->u.function.body);
    /* Normalize declarations */
    ycf_node_normalize_init_definitions_in_scope(&fun->u.function.body);
    /* Uniqify local variables */
    ycf_uniqify_local_vars_in_function(fun);
    /* Save scope declarations and function declarations (includes
       parameters) */
    ycf_node_list scope_defs = ycf_node_get_declarations_in_scope(&fun->u.function.body);
    scope_defs = ycf_node_list_shallow_copy(scope_defs);
    ycf_node_list defs = ycf_node_get_all_definitions_in_function(&fun->u.function);
    defs = ycf_node_list_shallow_copy(defs);
    /* Remove const from const declarations */
    ycf_node_modify_declarations(defs, ycf_node_remove_const_specifiers_from_declaration);
    ycf_node_modify_declarations(scope_defs, ycf_node_remove_const_specifiers_from_declaration);
    ycf_node_modify_declarations(fun->u.function.definition.parameters,
                                 ycf_node_remove_const_specifiers_from_declaration);
    /* Move all declarations to top scope */
    ycf_node_remove_declarations_in_scope(&fun->u.function.body);
    fun->u.function.body.definition_nodes = scope_defs;
    /* Add return statement if function is missing return statement */
    if(ycf_node_is_void_ret_fun(fun) &&
       !ycf_node_is_void_ret_ending_fun(fun)){
      ycf_node_list_append(&fun->u.function.body.other_nodes,
                           ycf_node_get_from_code_scope_text("return;"));
    }
    ycf_node_remove_unecessary_scopes(&fun->u.function.body);
}
