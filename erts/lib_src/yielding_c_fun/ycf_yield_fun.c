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
#include <string.h>


#include "ycf_utils.h"
#include "ycf_yield_fun.h"
#include "ycf_node.h"
#include "ycf_symbol.h"
#include "ycf_string.h"
#include "ycf_printers.h"
#include "ycf_parser.h"

static int ycf_yield_location_id_counter = 0;

static
ycf_node* mk_typedef_struct_node(ycf_node_list definitions, char* name){
  ycf_node* res = ycf_malloc(sizeof(ycf_node));
  res->next = NULL;
  res->type = ycf_node_type_gen_typedef_struct;
  res->u.gen_typedef_struct.definition_nodes = definitions;
  res->u.gen_typedef_struct.name = name;
  return res;
}

static
ycf_node_list mk_ycf_trap_state_params(){
  return ycf_node_definition_list_from_string(ycf_string_new(
          "long* ycf_nr_of_reductions_param;\n"
          "void** ycf_trap_state;\n"
          "void* ycf_extra_context;\n"));
}

static
ycf_node_list mk_saved_ycf_trap_state_params(){
  return ycf_node_definition_list_from_string(
          "ycf_yield_alloc_type ycf_yield_alloc;\n"
          "ycf_yield_free_type ycf_yield_free;\n"
          "void* ycf_yield_alloc_free_context;\n"
          "size_t ycf_stack_alloc_size_or_max_size;\n"
          "void* ycf_stack_alloc_data;\n");
}

static
ycf_node_list mk_trap_extra_state(){
  return ycf_node_definition_list_from_string(
            "int ycf_trap_location;\n"
            "long ycf_nr_of_reductions;\n"
            "struct ycf_alloc_data ycf_frame_alloc_data;\n");;
}

static
char* get_ycf_trap_state_assignments(ycf_node_list ycf_trap_state_defines){
  ycf_node* current = ycf_trap_state_defines.head;
  char* str = "";
  while(current != NULL){
    char* ident = ycf_symbol_get_text(current->u.definition.identifier);
    if(current->u.definition.array_brackets.head != NULL && !current->u.definition.array_brackets.head->u.array_bracket.empty){
      ycf_string_printable_buffer* array_size_exp = ycf_string_printable_buffer_new();
      ycf_node* bracket = current->u.definition.array_brackets.head;
      while(bracket != NULL){
        print_node_code_expression(bracket->u.array_bracket.content,array_size_exp);
          ycf_string_printable_buffer_printf(array_size_exp, " * ");
        bracket = bracket->next;
      }
        ycf_string_printable_buffer_printf(array_size_exp, " sizeof(");
      print_symbol_list(&current->u.definition.type_specifiers, array_size_exp);
        ycf_string_printable_buffer_printf(array_size_exp, ")");
      str = ycf_string_new("%s"
                           "    memcpy(ycf_my_trap_state->%s, %s, %s);\n",
                           str,
                           ident,
                           ident,
                           array_size_exp->buffer);
    }else{
      str = ycf_string_new("%s"
                           "    ycf_my_trap_state->%s = %s;\n",
                           str,
                           ident,
                           ident);
    }
    current = current->next;
  }
  return str;
}

static
char* mk_code_from_special_code_list(ycf_node_list special_code_list){
  ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
  ycf_node* current = special_code_list.head;
  ycf_string_printable_buffer_printf(b, "\n/* YCF SPECIAL CUSTOM CODE START */\n");
  while(current != NULL) {
    ycf_node_print(current->u.special_code_block.code.if_statement, b);
    ycf_string_printable_buffer_printf(b, "\n");
    current = current->next;
  }
  ycf_string_printable_buffer_printf(b, "\n/* YCF SPECIAL CUSTOM CODE END */\n");
  return b->buffer;
}

static
ycf_node* mk_yield_code(ycf_node* f_node,
                        char* ycf_trap_state_name,
                        ycf_node_list ycf_trap_state_defines,
                        ycf_node_list on_save_yield_state_code_list,
                        bool debug){
  char* ret_code;
  if(ycf_node_is_void_ret_fun(f_node)){
    ret_code = "return;\n";
  } else{
    ycf_symbol_list ret_type = ycf_node_get_return_type(f_node);
    ret_code =
      ycf_string_new("  {\n"
                     "    %s ycf_ret_value;\n"
                     "    return ycf_ret_value;\n"
                     "  }\n",
                     ycf_symbol_list_to_str(&ret_type));
  }
  char *debug_check_for_stack_ptrs = "";
  if(debug){
    debug_check_for_stack_ptrs =
      ycf_string_new("ycf_debug_check_block(\"%s\",\n"
                     "                      ycf_find_stack_bottom_conservative(),\n"
                     "                      ycf_trap_state,\n"
                     "                      ycf_my_trap_state,\n"
                     "                      sizeof(struct %s));\n",
                     ycf_trap_state_name,
                     ycf_trap_state_name);
  }
  char* code = ycf_string_new("\n"
                              "{\n"
                              "  struct %s* ycf_my_trap_state;\n"
                              "  ycf_do_yield_label_%s:;\n"
                              "  %s"
                              "  if (*ycf_trap_state == NULL) {\n"
                              "    ycf_my_trap_state = ycf_yield_alloc(sizeof(struct %s), ycf_yield_alloc_free_context);\n"
                              "  } else {\n"
                              "    ycf_my_trap_state = *ycf_trap_state;\n"
                              "  }\n"
                              "  %s\n"
                              "  *ycf_nr_of_reductions_param = ycf_nr_of_reductions;\n"
                              "  *ycf_trap_state = ycf_my_trap_state;\n"
                              "  %s"
                              "  %s\n"
                              "}\n"
                              "\n",
                              ycf_trap_state_name,
                              ycf_symbol_get_text(f_node->u.function.definition.definition.identifier),
                              mk_code_from_special_code_list(on_save_yield_state_code_list),
                              ycf_trap_state_name,
                              get_ycf_trap_state_assignments(ycf_trap_state_defines),
                              debug_check_for_stack_ptrs,
                              ret_code
  );
  return ycf_node_get_from_code_scope_text(code);
}

ycf_node* mk_goto_yield_code(ycf_node* f_node,
                             char* ycf_trap_state_name,
                             ycf_node_list ycf_trap_state_defines){
  ycf_yield_location_id_counter++;
  char* code = ycf_string_new("\n"
                              "{\n"
                              "  ycf_nr_of_reductions = 0;\n"
                              "  ycf_trap_location = %d;\n"
                              "  goto ycf_do_yield_label_%s;\n"
                              "  ycf_yield_location_label_%d:;\n"
                              "}\n",
                              ycf_yield_location_id_counter,
                              ycf_symbol_get_text(f_node->u.function.definition.definition.identifier),
                              ycf_yield_location_id_counter
  );
  return ycf_node_get_from_code_scope_text(code);
}

static
ycf_node* mk_goto_yield_no_reds_code(ycf_node* f_node,
                                     char* ycf_trap_state_name,
                                     ycf_node_list ycf_trap_state_defines){
  ycf_yield_location_id_counter++;
  char* code = ycf_string_new("\n"
                              "{\n"
                              "  ycf_trap_location = %d;\n"
                              "  goto ycf_do_yield_label_%s;\n"
                              "  ycf_yield_location_label_%d:;\n"
                              "}\n",
                              ycf_yield_location_id_counter,
                              ycf_symbol_get_text(f_node->u.function.definition.definition.identifier),
                              ycf_yield_location_id_counter
  );
  return ycf_node_get_from_code_scope_text(code);
}


static
ycf_node* mk_yield_fun_call_state_var(ycf_node* f){
  ycf_node_list code = ycf_node_list_empty();
  char* fun_name = NULL;
  if(f->u.statement.expression->type == ycf_node_type_function_call){
    fun_name = ycf_symbol_get_text(f->u.statement.expression->u.function_call.identifier);
  }else{
    fun_name = ycf_symbol_get_text(f->u.statement.expression->u.function_call_assignment.fun_call.identifier);
  }
  ycf_node * scope_with_dec =
          ycf_node_get_from_code_scope_text(ycf_string_new("void* ycf_sub_fun_trap_state_wb = NULL;\n"
                                                           "/*special_code_start:ON_DESTROY_STATE*/\n"
                                                           "if(0){\n"
                                                           "  if(ycf_sub_fun_trap_state_wb != NULL)\n{"
                                                           "     %s_ycf_gen_destroy(ycf_sub_fun_trap_state_wb);\n"
                                                           "  }\n"
                                                           "}\n"
                                                           "/*special_code_end*/\n", fun_name));
  ycf_node_list_append(&code, ycf_node_shallow_copy(f));
  ycf_node_list_append(&code, scope_with_dec->u.code_scope.other_nodes.head);
  ycf_node* ret = ycf_node_scope_new(ycf_symbol_new_open_curly_brace(),
                                     scope_with_dec->u.code_scope.definition_nodes,
                                     code,
                                     ycf_symbol_new_end_curly_brace());
  return ret;
}

static
ycf_node* mk_yield_fun_call(ycf_node* c_file_node,
                            ycf_node* f,
                            char* ycf_trap_state_var_name,
                            char* calling_fun_name,
                            bool auto_yield){
  ycf_yield_location_id_counter++;
  ycf_node_function_call f_node;
  char* assignment_code = "";
  char* tmp_assignment_var_name = "ycf_tmp_fun_call_tmp_var";
  char* tmp_assignment_var_declaration = "";
  char* parameters = "";
  char* finalize_call_code = "";
  if(f->u.statement.expression->type == ycf_node_type_function_call){
    f_node = f->u.statement.expression->u.function_call;
  }else{
    ycf_symbol_list called_fun_ret_type;
    ycf_string_printable_buffer* assign_to_b = ycf_string_printable_buffer_new();
    f_node = f->u.statement.expression->u.function_call_assignment.fun_call;
    called_fun_ret_type =
      ycf_node_find_function_return_type(c_file_node,
                                         ycf_symbol_get_text(f_node.identifier));
    tmp_assignment_var_declaration =
      ycf_string_new("%s %s;\n",
                     ycf_symbol_list_to_str(&called_fun_ret_type),
                     tmp_assignment_var_name);
    print_node_code_expression(f->u.statement.expression->u.function_call_assignment.left_side, assign_to_b);
    assignment_code = ycf_string_new("%s = ", tmp_assignment_var_name);
    finalize_call_code = ycf_string_new("%s = %s;\n",
                                        assign_to_b->buffer,
                                        tmp_assignment_var_name);
  }
  if (f_node.parameter_expressions.head != NULL){
    ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
    print_node_list_code(f_node.parameter_expressions.head, b);
    parameters = ycf_string_new(",%s", b->buffer);
  }

  char* code = ycf_string_new("%s"
                              "%s"
                              "%s %s%s_ycf_gen_yielding(&ycf_nr_of_reductions,\n"
                              "                         &%s,ycf_extra_context,\n"
                              "                         ycf_yield_alloc,ycf_yield_free,\n"
                              "                         ycf_yield_alloc_free_context,\n"
                              "                         YCF_ALLOC_NEXT_MAX_SIZE(),\n"
                              "                         YCF_ALLOC_NEXT_BLOCK()\n"
                              "                         %s);\n"
                              "while(YCF_IS_YIELDED(%s)){\n"
                              "   ycf_trap_location = %d;\n"
                              "   goto ycf_do_yield_label_%s;\n"
                              "  ycf_yield_location_label_%d:;\n"
                              "   %s %s%s_ycf_gen_continue(&ycf_nr_of_reductions,\n"
                              "                            &%s,\n"
                              "                            ycf_extra_context);\n"
                              "}\n"
                              "%s\n",
                              tmp_assignment_var_declaration,
                              auto_yield ? "YCF_CONSUME_REDS(1);" : "",
                              assignment_code,
                              ycf_symbol_list_to_str(&f_node.neg_symbols),
                              ycf_symbol_get_text(f_node.identifier),
                              ycf_trap_state_var_name,
                              parameters,
                              ycf_trap_state_var_name,
                              ycf_yield_location_id_counter,
                              calling_fun_name,
                              ycf_yield_location_id_counter,
                              assignment_code,
                              ycf_symbol_list_to_str(&f_node.neg_symbols),
                              ycf_symbol_get_text(f_node.identifier),
                              ycf_trap_state_var_name,
                              finalize_call_code
  );

  return ycf_node_get_from_code_scope_text(code);
}


static
char* get_trap_restore_assignments(ycf_node_list ycf_trap_state_defines){
  ycf_node* current = ycf_trap_state_defines.head;
  char* str = "\n";
  while(current != NULL){
    char* ident = ycf_symbol_get_text(current->u.definition.identifier);
    if(current->u.definition.array_brackets.head != NULL && !current->u.definition.array_brackets.head->u.array_bracket.empty){
      ycf_string_printable_buffer* array_size_exp = ycf_string_printable_buffer_new();
      ycf_node* bracket = current->u.definition.array_brackets.head;
      while(bracket != NULL){
        print_node_code_expression(bracket->u.array_bracket.content,array_size_exp);
          ycf_string_printable_buffer_printf(array_size_exp, " * ");
        bracket = bracket->next;
      }
        ycf_string_printable_buffer_printf(array_size_exp, " sizeof(");
      print_symbol_list(&current->u.definition.type_specifiers, array_size_exp);
        ycf_string_printable_buffer_printf(array_size_exp, ")");
      str = ycf_string_new("%s"
                           "    memcpy(%s, ycf_my_trap_state->%s, %s);\n",
                           str,
                           ident,
                           ident,
                           array_size_exp->buffer);
    }else{
      str = ycf_string_new("%s"
                           "    %s = ycf_my_trap_state->%s;\n",
                           str,
                           ident,
                           ident);
    }
    current = current->next;
  }
  return str;
}

static
char* get_goto_ycf_trap_location_switch(int nr_of_ycf_trap_locations){
  int i;
  char* str = "switch(ycf_trap_location){\n";
  for(i = 1; i <= nr_of_ycf_trap_locations; i++){
    str = ycf_string_new("%s"
                         "case %d: goto ycf_yield_location_label_%d;\n", str, i, i);
  }
  str = ycf_string_new("%s\n}", str, i, i);
  return str;
}

typedef struct {
  ycf_node* f_node;
  char* ycf_trap_state_name;
  ycf_node_list ycf_trap_state_defines;
} yield_code_replacer_context;

ycf_node* yield_code_replacer(ycf_node* candidate, ycf_node_code_scope* s,void* context){
  yield_code_replacer_context* c = context;
  (void)s;
  if(candidate->type == ycf_node_type_statement &&
     candidate->u.statement.expression != NULL &&
     candidate->u.statement.expression->type == ycf_node_type_function_call &&
     ycf_symbol_is_text_eq(candidate->u.statement.expression->u.function_call.identifier,
                           "YCF_YIELD") &&
     candidate->u.statement.expression->u.function_call.parameter_expressions.head == NULL){
    ycf_node* yield_code = mk_goto_yield_code(c->f_node, c->ycf_trap_state_name, c->ycf_trap_state_defines);
    return yield_code;
  } else {
    return candidate;
  }
}

static
void insert_yield_code(ycf_node* f_node, char* ycf_trap_state_name, ycf_node_list ycf_trap_state_defines, ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    yield_code_replacer,
                                                    &(yield_code_replacer_context) {f_node, ycf_trap_state_name,
                                                                                    ycf_trap_state_defines});
}

static
ycf_node* special_code_section_replacer(ycf_node* candidate, ycf_node_code_scope* s,void* context){
  (void)context;
  (void)s;
  if(candidate->type == ycf_node_type_statement &&
     candidate->u.statement.expression != NULL &&
     candidate->u.statement.expression->type == ycf_node_type_function_call) {
    ycf_node_function_call fun_call = candidate->u.statement.expression->u.function_call;
    ycf_symbol* fun_name = fun_call.identifier;
    if(ycf_symbol_is_text_eq(fun_name, "YCF_SPECIAL_CODE_START") &&
       ycf_node_list_length(fun_call.parameter_expressions) == 1) {
      char* parameter = ycf_node_to_string(fun_call.parameter_expressions.head);
      ycf_node* special_code_start =
        ycf_node_new_text_node(ycf_string_new("/*special_code_start:%s*/\n"
                                              "if(0){\n",
                                              parameter));
      return special_code_start;
    } else if (ycf_symbol_is_text_eq(candidate->u.statement.expression->u.function_call.identifier,
                                     "YCF_SPECIAL_CODE_END") &&
               ycf_node_list_length(fun_call.parameter_expressions) == 0) {
      return ycf_node_new_text_node("}/*special_code_end*/\n");
    }
  }
  return candidate;
}

static
ycf_node* replace_alt_syntax_special_code_section_code(ycf_node* f_node){
    ycf_node_search_and_replace_statements_in_scope(&f_node->u.function.body,
                                                    special_code_section_replacer,
                                                    NULL);
    return ycf_node_get_function_from_text(ycf_node_to_string(f_node));
}


static
ycf_node* yield_no_reds_code_replacer(ycf_node* candidate, ycf_node_code_scope* s,void* context){
  yield_code_replacer_context* c = context;
  (void)s;
  if(candidate->type == ycf_node_type_statement &&
     candidate->u.statement.expression != NULL &&
     candidate->u.statement.expression->type == ycf_node_type_function_call &&
     ycf_symbol_is_text_eq(candidate->u.statement.expression->u.function_call.identifier,
                           "YCF_YIELD_NO_REDS") &&
     candidate->u.statement.expression->u.function_call.parameter_expressions.head == NULL){
    ycf_node* yield_code = mk_goto_yield_no_reds_code(c->f_node, c->ycf_trap_state_name, c->ycf_trap_state_defines);
    return yield_code;
  } else {
    return candidate;
  }
}

static
void insert_yield_no_reds_code(ycf_node* f_node, char* ycf_trap_state_name, ycf_node_list ycf_trap_state_defines, ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    yield_no_reds_code_replacer,
                                                    &(yield_code_replacer_context){f_node, ycf_trap_state_name,
                                                                                  ycf_trap_state_defines});
}

typedef struct {
  ycf_node_list on_return_code_list;
  ycf_node_list on_return_or_destroy_code_list;
  bool auto_yield;
} replace_return_ctx;

static
ycf_node* save_nr_of_reductions_before_return_replacer(ycf_node* candidate,
                                                       ycf_node_code_scope* s,
                                                       void* context){
  replace_return_ctx* ctx = context;
  (void)s;
  char* consume_reds_code = ctx->auto_yield ? "YCF_CONSUME_REDS(1);\n" : "";
  if(candidate->type == ycf_node_type_return_statement){
    char* code =
      ycf_string_new("\n"
                     "%s"
                     "if (*ycf_trap_state != NULL) {\n"
                     "   ycf_yield_free(*ycf_trap_state, ycf_yield_alloc_free_context);\n"
                     "   *ycf_trap_state = NULL;\n"
                     "}\n"
                     "ycf_destroy_stack_allocator(&ycf_frame_alloc_data,\n"
                     "                            ycf_yield_free,\n"
                     "                            ycf_yield_alloc_free_context);\n"
                     "*ycf_nr_of_reductions_param = ycf_nr_of_reductions;"
                     "%s"
                     "%s"
                     "%s",
                     consume_reds_code,
                     mk_code_from_special_code_list(
                                                    ctx->on_return_or_destroy_code_list),
                     mk_code_from_special_code_list(ctx->on_return_code_list),
                     ycf_node_to_string(candidate));
    return ycf_node_get_from_code_scope_text(code);
  } else {
    return candidate;
  }
}

static
void save_nr_of_reductions_before_return(ycf_node_code_scope* s,
                                         ycf_node_list on_return_code_list,
                                         ycf_node_list on_return_or_destroy_code_list,
                                         bool auto_yield){
  replace_return_ctx ctx;
  ctx.on_return_code_list = on_return_code_list;
  ctx.on_return_or_destroy_code_list = on_return_or_destroy_code_list;
  ctx.auto_yield = auto_yield;
  ycf_node_search_and_replace_statements_in_scope(s,
                                                  save_nr_of_reductions_before_return_replacer,
                                                  &ctx);
}

ycf_node* mk_consume_reds_code(char* function_name, ycf_node* consume_reds_node){
    ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
    ycf_yield_location_id_counter++;
    print_node_code_paran_expression(consume_reds_node->u.consume_reds.nr_of_reds_expression, b);
    return ycf_node_get_from_code_scope_text(ycf_string_new("ycf_nr_of_reductions = ycf_nr_of_reductions - %s;\n"
                                                            "if (ycf_nr_of_reductions <= 0) {\n"
                                                            "  ycf_trap_location = %d;\n"
                                                            "  goto ycf_do_yield_label_%s;\n"
                                                            "  ycf_yield_location_label_%d:;\n"
                                                            "}\n",
                                                            b->buffer,
                                                            ycf_yield_location_id_counter,
                                                            function_name,
                                                            ycf_yield_location_id_counter));
}

static
ycf_node* consume_reds_replacer(ycf_node* candidate, ycf_node_code_scope* s, void* context){
  char* function_name = context;
  (void)s;
  if(candidate->type == ycf_node_type_consume_reds){
    return mk_consume_reds_code(function_name, candidate);
  } else {
    return candidate;
  }
}

static
void insert_consume_reds_code(char* function_name, ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    consume_reds_replacer,
                                                    function_name);
}

static
ycf_node* mk_consume_reds_wrapped_statement(ycf_node* statement){
  return ycf_node_get_from_code_scope_text(ycf_string_new("YCF_CONSUME_REDS(1);\n"
                                                          "%s\n", ycf_node_to_string(statement)));
}

static
void insert_consume_reds_calls(ycf_node_code_scope* s);

static
ycf_node* consume_reds_calls_inserter(ycf_node* candidate, ycf_node_code_scope* s, void* context){
  if(candidate->type == ycf_node_type_goto){
    return mk_consume_reds_wrapped_statement(candidate);
  } else if (candidate->type == ycf_node_type_while){
    if(candidate->u.while_n.statement->type == ycf_node_type_code_scope){
      insert_consume_reds_calls(&candidate->u.while_n.statement->u.code_scope);
    }
    candidate->u.while_n.statement = mk_consume_reds_wrapped_statement(candidate->u.while_n.statement);
    return candidate;
  } else if (candidate->type == ycf_node_type_do_while){
    if(candidate->u.do_while.statement->type == ycf_node_type_code_scope){
      insert_consume_reds_calls(&candidate->u.do_while.statement->u.code_scope);
    }
    candidate->u.do_while.statement = mk_consume_reds_wrapped_statement(candidate->u.do_while.statement);
    return candidate;
  } else if (candidate->type == ycf_node_type_for){
    if(candidate->u.for_n.statement->type == ycf_node_type_code_scope){
      insert_consume_reds_calls(&candidate->u.for_n.statement->u.code_scope);
    }
    candidate->u.for_n.statement = mk_consume_reds_wrapped_statement(candidate->u.for_n.statement);
    return candidate;
  } else {
    return candidate;
  }
}

static void insert_consume_reds_calls(ycf_node_code_scope* s){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    consume_reds_calls_inserter,
                                                    NULL);
}

typedef struct {
  ycf_node_list code;
  ycf_node_type code_type;
} special_code_context;

ycf_node* do_special_code_replace(ycf_node* candidate, ycf_node_code_scope* s, void* context){
  special_code_context* c = context;
  (void)s;
  if(candidate->type == c->code_type){
    ycf_node_list_append(&c->code, ycf_node_shallow_copy(candidate));
    return ycf_node_new_text_node("\n/* YCF Replaced special code */\n");
  } else {
    return candidate;
  }
}

static
ycf_node_list save_and_replace_special_code(ycf_node_code_scope* s, ycf_node_type code_type){
  special_code_context context;
  context.code = ycf_node_list_empty();
  context.code_type = code_type;
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    do_special_code_replace,
                                                    &context);
  return context.code;
}

static
ycf_node* insert_fun_call_state_var_replacer(ycf_node* candidate, ycf_node_code_scope* s, void* context){
  ycf_string_item_list* yielding_funs = context;
  (void)s;
  if(candidate->type == ycf_node_type_statement &&
     (candidate->u.statement.expression->type == ycf_node_type_function_call ||
      candidate->u.statement.expression->type == ycf_node_type_assignment_function_call)){
    ycf_string_item* current = yielding_funs->head;
    while(current != NULL){
      char* fun_name = NULL;
      if(candidate->u.statement.expression->type == ycf_node_type_function_call){
        fun_name = ycf_symbol_get_text(candidate->u.statement.expression->u.function_call.identifier);
      } else if(candidate->u.statement.expression->type == ycf_node_type_assignment_function_call){
        fun_name = ycf_symbol_get_text(
                candidate->u.statement.expression->u.function_call_assignment.fun_call.identifier);
      } else {
        current = current->next;
        continue;
      }
      if(ycf_string_is_equal(current->str, fun_name)){
        return mk_yield_fun_call_state_var(candidate);
      }
      current = current->next;
    }
  }
  return candidate;
}

static
void insert_fun_call_state_var(ycf_node_code_scope* s, ycf_string_item_list* yielding_funs){
    ycf_node_search_and_replace_statements_in_scope(s,
                                                    insert_fun_call_state_var_replacer,
                                                    yielding_funs);
}

typedef struct {
  ycf_node* c_file_node;
  char* calling_fun_name;
  ycf_string_item_list* yielding_funs;
  ycf_string_item_list* starte_vars_wb;
  bool auto_yield;
} yielding_fun_call_code_context;

static
ycf_node* insert_yielding_fun_call_code_replacer(ycf_node* candidate,
                                                 ycf_node_code_scope* s,
                                                 void* context){
  yielding_fun_call_code_context* c = context;
  ycf_node* c_file_node = c->c_file_node;
  ycf_string_item_list* yielding_funs = c->yielding_funs;
  (void)s;
  if (candidate->type == ycf_node_type_code_scope &&
      candidate->u.code_scope.other_nodes.head != candidate->u.code_scope.other_nodes.last){
    ycf_node* call_candidate = candidate->u.code_scope.other_nodes.head->next;
    if (call_candidate->type == ycf_node_type_statement &&
        (call_candidate->u.statement.expression->type == ycf_node_type_function_call ||
         call_candidate->u.statement.expression->type == ycf_node_type_assignment_function_call)) {
      ycf_string_item* current = yielding_funs->head;
      while(current != NULL){
        char* fun_name = NULL;
        if (call_candidate->u.statement.expression->type == ycf_node_type_function_call) {
          fun_name = ycf_symbol_get_text(call_candidate->u.statement.expression->u.function_call.identifier);
        } else if (call_candidate->u.statement.expression->type == ycf_node_type_assignment_function_call) {
          fun_name = ycf_symbol_get_text(
                  call_candidate->u.statement.expression->u.function_call_assignment.fun_call.identifier);
        } else {
          current = current->next;
          continue;
        }
        if(ycf_string_is_equal(current->str, fun_name)){
          char* ycf_trap_state_var_name =
            ycf_symbol_get_text(ycf_node_get_assignment(candidate->u.code_scope.other_nodes.head)->left_side.content.head->u.other.what);
            ycf_string_item_list_append(c->starte_vars_wb, ycf_string_item_new(ycf_trap_state_var_name));
            ycf_node* new_call_code = mk_yield_fun_call(c_file_node,
                                                        call_candidate,
                                                        ycf_trap_state_var_name,
                                                        c->calling_fun_name,
                                                        c->auto_yield);
          ycf_node_list_replace(&candidate->u.code_scope.other_nodes, call_candidate, new_call_code);
          return ycf_node_scope_new(ycf_symbol_new_open_curly_brace(),
                                    candidate->u.code_scope.definition_nodes,
                                    candidate->u.code_scope.other_nodes,
                                    ycf_symbol_new_end_curly_brace());;
        }
        current = current->next;
      }
    }
  }
  return candidate;
}

ycf_string_item_list insert_yielding_fun_call_code(ycf_node* c_file_node,
                                                   ycf_node_code_scope* s,
                                                   ycf_string_item_list* yielding_funs,
                                                   char* calling_fun_name,
                                                   bool auto_yield){
  yielding_fun_call_code_context context;
  ycf_string_item_list state_vars = ycf_string_item_list_empty();
  context.c_file_node = c_file_node;
  context.calling_fun_name = calling_fun_name;
  context.yielding_funs = yielding_funs;
  context.starte_vars_wb = &state_vars;
  context.auto_yield = auto_yield;
  ycf_node_search_and_replace_statements_in_scope(s,
                                                  insert_yielding_fun_call_code_replacer,
                                                  &context);
  return state_vars;
}

static
ycf_node* mk_yield_init_code(char* ycf_trap_state_name, ycf_node_list ycf_trap_state_defines, ycf_node_list on_restore_yield_state_code_list, bool auto_yield, char *function_name){
  char* code = ycf_string_new("\n"
                              "{\n"
                              "  ycf_nr_of_reductions = *ycf_nr_of_reductions_param;\n"
                              "  ycf_frame_alloc_data.size = 0;\n"
                              "  ycf_frame_alloc_data.max_size = ycf_stack_alloc_size_or_max_size;\n"
                              "  ycf_frame_alloc_data.data = ycf_stack_alloc_data;\n"
                              "  ycf_frame_alloc_data.needs_freeing = 0;\n"
                              "  if(*ycf_trap_state != NULL){\n"
                              "     struct %s* ycf_my_trap_state = *ycf_trap_state;\n"
                              "     %s\n"
                              "     %s\n"
                              "     ycf_nr_of_reductions = *ycf_nr_of_reductions_param;\n"
                              "     %s\n"
                              "  }\n"
                              "}\n",
                              ycf_trap_state_name,
                              get_trap_restore_assignments(ycf_trap_state_defines),
                              mk_code_from_special_code_list(on_restore_yield_state_code_list),
                              get_goto_ycf_trap_location_switch(ycf_yield_location_id_counter));
  return ycf_node_get_from_code_scope_text(code);
}

static
ycf_node* mk_destroy_state_function_node(char* yielding_function_name,
                                         ycf_node_list defs,
                                         char* ycf_trap_state_struct_name,
                                         ycf_node_list destroy_code,
                                         ycf_node_list on_destroy_state_or_return_code_list,
                                         bool static_aux_funs){
  ycf_node_list my_defs = ycf_node_list_shallow_copy(defs);
  ycf_node* current = my_defs.head;
  while(current != NULL){
    current->u.definition.end = ycf_symbol_new_semicolon();
    int empty_array_brackets = 0;
    {
      current->u.definition.array_brackets = ycf_node_list_shallow_copy(current->u.definition.array_brackets);
      ycf_node* b = current->u.definition.array_brackets.head;
      while(b != NULL && b->u.array_bracket.empty){
        empty_array_brackets++;
        ycf_node_list_remove(&current->u.definition.array_brackets, b);
        b = b->next;
      }
    }
    current->u.definition.type_specifiers = ycf_symbol_list_shallow_copy(current->u.definition.type_specifiers);
    if(empty_array_brackets > 0){
      for(int i = 0; i < empty_array_brackets; i++){
        ycf_symbol_list_append(&current->u.definition.type_specifiers, ycf_symbol_new_star());
      }
    }
    current = current->next;
  }
  char* code =
    ycf_string_new("\n"
                   "%s void %s_ycf_gen_destroy(struct %s* ycf_my_trap_state){\n"
                   "     {\n"
                   "     %s\n"
                   "     %s\n"
                   "     %s\n"
                   "     %s\n"
                   "     ycf_destroy_stack_allocator(&ycf_frame_alloc_data, ycf_yield_free, ycf_yield_alloc_free_context);\n"
                   "     ycf_yield_free(ycf_my_trap_state, ycf_yield_alloc_free_context);\n"
                   "     }\n"
                   "}\n",
                   static_aux_funs ? "static" : "",
                   yielding_function_name,
                   ycf_trap_state_struct_name,
                   ycf_node_list_to_string(&my_defs),
                   get_trap_restore_assignments(my_defs),
                   mk_code_from_special_code_list(on_destroy_state_or_return_code_list),
                   mk_code_from_special_code_list(destroy_code));
  return ycf_node_get_function_from_text(code);
}

void ast_add_yield_code_generated_define(ycf_node* source_out_tree/*Will be changed*/, bool debug_mode)
{
  char *debug_mode_code =
    (debug_mode ?
     "\n"
     "#include <setjmp.h>\n"
     "#include <stdint.h>\n"
     "#include <string.h>\n"
     "static void* ycf_find_stack_bottom_conservative_helper() {\n"
     "  void* p = NULL;\n"
     "  volatile intptr_t tmp = (intptr_t)&p;\n"
     "  return (void*)tmp;\n"
     "}\n"
     "static void* ycf_find_stack_bottom_conservative() {\n"
     "  jmp_buf env;\n"
     "  setjmp(env);\n"
     "\n"
     "  {\n"
     "  volatile int noinline = 1;\n"
     "  void* (*bottom)(void) = noinline\n"
     "    ? ycf_find_stack_bottom_conservative_helper\n"
     "    : (void*(*)(void))(NULL);\n"
     "\n"
     "  return bottom();\n"
     "  }\n"
     "}\n"
     "static void ycf_debug_check_block(char* struct_name, void* stack_start, void* stack_end, void* block, size_t block_size) {\n"
     "  char* p;\n"
     "  for (p = block; p < (((char*)block) + block_size); p += sizeof(void*)) {\n"
     "    if(*((char**)p) > (char*)stack_start && *((char**)p) <= (char*)stack_end){\n"
     "      fprintf(stderr, \"Pointer to stack in yielded functions state!!!!! (pointer_address=%p, struct %s,offset=%lu)\\n\", (void*)p, struct_name, (unsigned long)(p-(size_t)block));\n"
     "      exit(1);\n"
     "    }\n"
     "  }\n"
     "}\n"
     "\n"
     :
     "");
  char* ycf_yielding_c_fun_helpers_code =
    ycf_string_new("#ifndef YCF_YIELDING_C_FUN_HELPERS\n"
                   "#define YCF_YIELDING_C_FUN_HELPERS 1\n"
                   "#include <string.h>\n"
                   "#include <stdio.h>\n"
                   "#include <stdlib.h>\n"
                   "\n"
                   "/*\n"
                   " * YCF_GCC_DIAG_ON and YCF_GCC_DIAG_OFF can be used to temporarly\n"
                   " * disable a gcc or clang warning in a file.\n"
                   " *\n"
                   " * Example:\n"
                   " * YCF_GCC_DIAG_OFF(unused-function)\n"
                   " * static int test(){ return 0;}\n"
                   " * YCF_GCC_DIAG_ON(unused-function)\n"
                   " *\n"
                   " * These macros were orginally authored by Jonathan Wakely and has\n"
                   " * been modified by Patrick Horgan.\n"
                   " *\n"
                   " * Source: http://dbp-consulting.com/tutorials/SuppressingGCCWarnings.html\n"
                   " *\n"
                   " */\n"
                   "#if defined(_MSC_VER)\n"
                   "#  define YCF_GCC_DIAG_OFF(x) __pragma(warning(push, 0))\n"
                   "#  define YCF_GCC_DIAG_ON(x)  __pragma(warning(pop))\n"
                   "#elif ((__GNUC__ * 100) + __GNUC_MINOR__) >= 402\n"
                   "#define YCF_GCC_DIAG_STR(s) #s\n"
                   "#define YCF_GCC_DIAG_JOINSTR(x,y) YCF_GCC_DIAG_STR(x ## y)\n"
                   "# define YCF_GCC_DIAG_DO_PRAGMA(x) _Pragma (#x)\n"
                   "# define YCF_GCC_DIAG_PRAGMA(x) YCF_GCC_DIAG_DO_PRAGMA(GCC diagnostic x)\n"
                   "# if ((__GNUC__ * 100) + __GNUC_MINOR__) >= 406\n"
                   "#  define YCF_GCC_DIAG_OFF(x) YCF_GCC_DIAG_PRAGMA(push) \\\n"
                   "      YCF_GCC_DIAG_PRAGMA(ignored YCF_GCC_DIAG_JOINSTR(-W,x))\n"
                   "#  define YCF_GCC_DIAG_ON(x) YCF_GCC_DIAG_PRAGMA(pop)\n"
                   "# else\n"
                   "#  define YCF_GCC_DIAG_OFF(x) YCF_GCC_DIAG_PRAGMA(ignored YCF_GCC_DIAG_JOINSTR(-W,x))\n"
                   "#  define YCF_GCC_DIAG_ON(x)  YCF_GCC_DIAG_PRAGMA(warning YCF_GCC_DIAG_JOINSTR(-W,x))\n"
                   "# endif\n"
                   "#else\n"
                   "# define YCF_GCC_DIAG_OFF(x)\n"
                   "# define YCF_GCC_DIAG_ON(x)\n"
                   "#endif\n"
                   "#ifdef __GNUC__\n"
                   "#  if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 5) || defined(__clang__)\n"
                   "#    define YCF_GCC_ATTRIBUTE_UNUSED __attribute__ ((unused))\n"
                   "#  else\n"
                   "#    define YCF_GCC_ATTRIBUTE_UNUSED\n"
                   "#  endif\n"
                   "#else\n"
                   "#  define YCF_GCC_ATTRIBUTE_UNUSED\n"
                   "#endif\n"
                   "\n"
                   "typedef void* (*ycf_yield_alloc_type) (size_t,void*);\n"
                   "typedef void (*ycf_yield_free_type) (void*,void*);\n"
                   "\n"
                   "struct ycf_alloc_data {\n"
                   "  size_t size;\n"
                   "  size_t max_size;\n"
                   "  int needs_freeing;\n"
                   "  char* data;\n"
                   "};\n"
                   "\n"
                   "#define YCF_ALLOC_NEXT_BLOCK() (\\\n"
                   " ycf_frame_alloc_data.data == NULL \\\n"
                   "   ? NULL \\\n"
                   "   : ((void*)(&ycf_frame_alloc_data.data[ycf_frame_alloc_data.size]))\\\n"
                   ")\n"
                   "#define YCF_ALLOC_NEXT_MAX_SIZE() (\\\n"
                   " ycf_frame_alloc_data.data == NULL \\\n"
                   "   ? 0 \\\n"
                   "   : (ycf_frame_alloc_data.max_size - ycf_frame_alloc_data.size)\\\n"
                   ")\n"
                   "\n"
                   "/* Macros for special code sections */\n"
                   "#define ON_SAVE_YIELD_STATE ON_SAVE_YIELD_STATE\n"
                   "#define ON_RESTORE_YIELD_STATE ON_RESTORE_YIELD_STATE\n"
                   "#define ON_DESTROY_STATE ON_DESTROY_STATE\n"
                   "#define ON_RETURN ON_RETURN\n"
                   "#define ON_DESTROY_STATE_OR_RETURN ON_DESTROY_STATE_OR_RETURN\n"
                   "#define YCF_SPECIAL_CODE_START(PARAM) \\\n"
                   "   /*special_code_start:PARAM*/       \\\n"
                   "   if(0){\n"
                   "#define YCF_SPECIAL_CODE_END() \\\n"
                   "   }\\\n"
                   "   /*special_code_end*/\n"
                   "\n"
                   "YCF_GCC_ATTRIBUTE_UNUSED\n"
                   "static void* ycf_stack_alloc(size_t size,\n"
                   "                             struct ycf_alloc_data* data,\n"
                   "                              ycf_yield_alloc_type allocator,\n"
                   "                              void* ycf_yield_alloc_free_context,\n"
                   "                              size_t default_stack_size){\n"
                   "  void * ret = NULL;"
                   "  if (data->data == NULL) {\n"
                   "    if (default_stack_size == 0) {\n"
                   "      fprintf(stderr, \"ycf_alloc: not enough stack!!\\n\");\n"
                   "      exit(1);\n"
                   "    }\n"
                   "    data->data = allocator(default_stack_size, ycf_yield_alloc_free_context);\n"
                   "    data->needs_freeing = 1;"
                   "    data->max_size = default_stack_size;"
                   "    data->size = 0;"
                   "  }\n"
                   "  if (data->size + size > data->max_size) {\n"
                   "    fprintf(stderr, \"ycf_alloc: not enough stack!\\n\");\n"
                   "    exit(1);\n"
                   "  }\n"
                   "  ret = &data->data[data->size];\n"
                   "  data->size = data->size + size;\n"
                   "  return ret;\n"
                   "}\n"
                   "static void ycf_destroy_stack_allocator(struct ycf_alloc_data* data,\n"
                   "                                        ycf_yield_free_type freer,\n"
                   "                                        void* ycf_yield_alloc_free_context){\n"
                   "  if(data->needs_freeing){\n"
                   "    freer(data->data, ycf_yield_alloc_free_context);\n"
                   "  }\n"
                   "}\n"
                   "\n"
                   "#include <limits.h>\n"
                   "#define YCF_MAX_NR_OF_REDS LONG_MAX\n"
                   "#define YCF_NR_OF_REDS_LEFT() ycf_nr_of_reductions\n"
                   "#define YCF_SET_NR_OF_REDS_LEFT(NEW_NR_OF_REDS_LEFT) \\\n"
                   "  do{ycf_nr_of_reductions = (NEW_NR_OF_REDS_LEFT);}while(0)\n"
                   "\n"
                   "#define YCF_GET_EXTRA_CONTEXT() ycf_extra_context\n"
                   "\n"
                   "#define YCF_IS_YIELDED(CTX) (CTX != NULL)\n"
                   "\n"
                   "#define YCF_YIELD_CODE_GENERATED 1\n"
                   "\n"
                   "%s"
                   "\n"
                   "/* end of YCF_YIELDING_C_FUN_HELPERS guard */\n"
                   "#endif\n", debug_mode_code);
  ycf_node_list_prepend(&source_out_tree->u.c_file.content,
                        ycf_node_new_text_node(ycf_yielding_c_fun_helpers_code));
}

static
ycf_node* mk_fun_def(ycf_node* fun_node){
  ycf_node* fun_def = ycf_malloc(sizeof(ycf_node));
  fun_def->next = NULL;
  fun_def->type = ycf_node_type_function_declaration;
  fun_def->u.function_definition = fun_node->u.function.definition;
  ycf_symbol_list fun_node_def_end = ycf_symbol_list_empty();
  ycf_symbol_list_append(&fun_node_def_end, ycf_symbol_new_semicolon());
  fun_def->u.function_definition.end = fun_node_def_end;
  return fun_def;
}


static
ycf_node* mk_null_vars_code(ycf_string_item_list fun_call_state_vars){
  ycf_string_item* current = fun_call_state_vars.head;
  char* assignments = "";
  while(current != NULL){
    assignments = ycf_string_new("%s\n"
                                 "%s = NULL;\n",
                                 assignments,
                                 current->str);
    current = current->next;
  }
  ycf_node* code = ycf_node_get_from_code_scope_text(assignments);
  return code;
}

static
ycf_node* mk_continue_function_node(char* yielding_function_name,
                                    char* ycf_trap_state_struct_name,
                                    ycf_node* yielding_fun,
                                    ycf_node_list uniqified_parameters){
  char* parameters = "";
  ycf_node* current = uniqified_parameters.head;
  while(current != NULL){
    parameters = ycf_string_new("%s,ycf_my_trap_state->%s", parameters, ycf_symbol_get_text(current->u.definition.identifier));
    current = current->next;
  }
  char* fun_call_str =
    ycf_string_new("%s_ycf_gen_yielding(ycf_number_of_reduction_param,\n"
                   "                    ycf_trap_state,\n"
                   "                    ycf_extra_context,\n"
                   "                    ycf_my_trap_state->ycf_yield_alloc,\n"
                   "                    ycf_my_trap_state->ycf_yield_free,\n"
                   "                    ycf_my_trap_state->ycf_yield_alloc_free_context,\n"
                   "                    ycf_my_trap_state->ycf_stack_alloc_size_or_max_size,\n"
                   "                    ycf_my_trap_state->ycf_stack_alloc_data\n"
                   "                    %s)\n",
                   yielding_function_name,
                   parameters);
  char* code = ycf_string_new("\n"
                              "%s %s_ycf_gen_continue(long* ycf_number_of_reduction_param,\n"
                              "                       void** ycf_trap_state,\n"
                              "                       void* ycf_extra_context){\n"
                              "     struct %s* ycf_my_trap_state = *ycf_trap_state;\n"
                              "%s"
                              "}\n",
                              ycf_symbol_list_to_str(&yielding_fun->u.function.definition.definition.type_specifiers),
                              yielding_function_name,
                              ycf_trap_state_struct_name,
                              (ycf_node_is_void_ret_fun(yielding_fun) ?
                               ycf_string_new("%s;\n"
                                             "return;\n",
                                             fun_call_str):
                               ycf_string_new("return %s;\n",
                                              fun_call_str)));
  ycf_symbol_list symbols = ycf_symbol_list_from_text(code);
  ycf_node* tree = get_abstract_syntax_tree_root(&symbols);
  if(tree->u.c_file.content.head->type != ycf_node_type_function_definition){
    printf("NOT A FUNCTION\n");
    exit(1);
  }
  return tree->u.c_file.content.head;
}

static
ycf_node* mk_debug_yielding_fun_call_wrapper(char* yielding_function_name,
                                             char* ycf_trap_state_struct_name,
                                             ycf_node* yielding_fun){
  char* parameters_2 = "";
  {
    ycf_node* current = yielding_fun->u.function.definition.parameters.head;
    bool first = true;
    char* identifier;
    while(current != NULL){
      identifier = ycf_symbol_get_text(current->u.definition.identifier);
      if(strcmp(identifier, "ycf_trap_state") == 0){
        identifier = ycf_string_new("(void**)(&ycf_my_trap_state)");
      }
      parameters_2 = ycf_string_new(first ? "%s%s" :"%s,%s",
                                  parameters_2,
                                  identifier);
      current = current->next;
      first = false;
    }
  }
  char* parameters = "";
  {
    ycf_node* current = yielding_fun->u.function.definition.parameters.head;
    bool first = true;
    while(current != NULL){
      parameters = ycf_string_new(first ? "%s%s" :"%s,%s",
                                  parameters,
                                  ycf_symbol_get_text(current->u.definition.identifier));
      current = current->next;
      first = false;
    }
  }
  ycf_string_printable_buffer* header_2 = ycf_string_printable_buffer_new();
  ycf_node_function_definition def_2 = yielding_fun->u.function.definition;
  def_2.definition.identifier =
    ycf_symbol_copy_change_text(def_2.definition.identifier,
                                ycf_string_new("%s_2",
                                               ycf_symbol_get_text(def_2.definition.identifier)));
  print_node_code_function_def(def_2, header_2);
  ycf_string_printable_buffer* header = ycf_string_printable_buffer_new();
  print_node_code_function_def(yielding_fun->u.function.definition, header);
  ycf_string_printable_buffer* parameter_types = ycf_string_printable_buffer_new();
  print_node_code_function_def_parameters(yielding_fun->u.function.definition, parameter_types);
  ycf_symbol_list ret_type = ycf_node_get_return_type(yielding_fun);
  char* wrapper_assignment = NULL;
  char* wrapper_return = NULL;
  if(ycf_node_is_void_ret_fun(yielding_fun)){
    wrapper_assignment = ycf_string_new("");
    wrapper_return = ycf_string_new("return;");
  }else{
    wrapper_assignment = ycf_string_new("%s ycf_to_return = ", ycf_symbol_list_to_str(&ret_type));
    wrapper_return = ycf_string_new("return ycf_to_return;");
  }
  char* code = ycf_string_new("\n"
                              "%s{\n"
                              "  volatile int noinline = 1;\n"
                              "  volatile void* ycf_my_trap_state = *ycf_trap_state;\n"
                              "  %s (*next)(%s = noinline\n"
                              "    ? %s_ycf_gen_yielding_3\n"
                              "    : (%s(*)(%s)(NULL);\n"
                              "  {\n"
                              "    %s next(%s);\n"
                              "    *ycf_trap_state = (struct %s*)ycf_my_trap_state;\n"
                              "    %s\n"
                              "  }\n"
                              "}\n"
                              "\n"
                              "\n"
                              "%s{\n"
                              "  volatile int noinline = 1;\n"
                              "  %s (*next)(%s = noinline\n"
                              "    ? %s_ycf_gen_yielding_2\n"
                              "    : (%s(*)(%s)(NULL);\n"
                              "\n"
                              "  {\n"
                              "    %s next(%s);\n"
                              "    %s;\n"
                              "  }\n"
                              "}\n",
                              header_2->buffer,
                              ycf_symbol_list_to_str(&ret_type),
                              parameter_types->buffer,
                              yielding_function_name,
                              ycf_symbol_list_to_str(&ret_type),
                              parameter_types->buffer,
                              wrapper_assignment,
                              parameters_2,
                              ycf_trap_state_struct_name,
                              wrapper_return,
                              /* Second function*/
                              header->buffer,
                              ycf_symbol_list_to_str(&ret_type),
                              parameter_types->buffer,
                              yielding_function_name,
                              ycf_symbol_list_to_str(&ret_type),
                              parameter_types->buffer,
                              wrapper_assignment,
                              parameters,
                              wrapper_return);

  return ycf_node_new_text_node(code);
}


static
void break_up_control_expressions(ycf_node_code_scope* s,
                                  ycf_string_item_list* yielding_function_names);


static ycf_node* break_up_control_expressions_helper(ycf_node* candidate,
                                                     ycf_node_code_scope* candidates_scope,
                                                     void* context){
    ycf_string_item_list* list = (ycf_string_item_list*)context;
    if(candidate->type == ycf_node_type_if &&
       ycf_node_list_length(candidate->u.if_n.expression.content.content) == 1 &&
       candidate->u.if_n.expression.content.content.head->type == ycf_node_type_function_call &&
       ycf_string_item_list_contains(list,
                                     ycf_symbol_get_text(candidate->u.if_n.expression.content.content.head->u.function_call.identifier))){
      break_up_control_expressions(&candidate->u.if_n.if_statement->u.code_scope, list);
      ycf_node * wrapper =
        ycf_node_get_from_code_scope_text(ycf_string_new("int ycf_gen_control_tmp;\n"
                                                         "ycf_gen_control_tmp = !!%s;\n"
                                                         "if(ycf_gen_control_tmp)\n"
                                                         "%s",
                                                         ycf_node_to_string(candidate->u.if_n.expression.content.content.head),
                                                         ycf_node_to_string(candidate->u.if_n.if_statement)));
      return wrapper;
    } else if(candidate->type == ycf_node_type_if_else &&
       ycf_node_list_length(candidate->u.if_else.if_part.expression.content.content) == 1 &&
       candidate->u.if_else.if_part.expression.content.content.head->type == ycf_node_type_function_call &&
       ycf_string_item_list_contains(list,
                                     ycf_symbol_get_text(candidate->u.if_else.if_part.expression.content.content.head->u.function_call.identifier))){
      break_up_control_expressions(&candidate->u.if_else.if_part.if_statement->u.code_scope, list);
      ycf_node * wrapper =
        ycf_node_get_from_code_scope_text(ycf_string_new("int ycf_gen_control_tmp;\n"
                                                         "ycf_gen_control_tmp = !!%s;\n"
                                                         "if(ycf_gen_control_tmp)\n"
                                                         "%s\n"
                                                         "%s\n"
                                                         "%s\n",
                                                         ycf_node_to_string(candidate->u.if_else.if_part.expression.content.content.head),
                                                         ycf_node_to_string(candidate->u.if_else.if_part.if_statement),
                                                         ycf_symbol_get_text(candidate->u.if_else.else_word),
                                                         ycf_node_to_string(candidate->u.if_else.else_statement)));
      return wrapper;
    } else if(candidate->type == ycf_node_type_do_while &&
       ycf_node_list_length(candidate->u.do_while.expression.content.content) == 1 &&
       candidate->u.do_while.expression.content.content.head->type == ycf_node_type_function_call &&
       ycf_string_item_list_contains(list,
                                     ycf_symbol_get_text(candidate->u.do_while.expression.content.content.head->u.function_call.identifier))){
      break_up_control_expressions(&candidate->u.do_while.statement->u.code_scope, list);
      ycf_node * wrapper =
        ycf_node_get_from_code_scope_text(ycf_string_new("int ycf_gen_control_tmp;\n"
                                                         "do {\n"
                                                         "  %s\n"
                                                         "  ycf_gen_control_tmp = !!%s;\n"
                                                         "}while(ycf_gen_control_tmp);\n",
                                                         ycf_node_to_string(candidate->u.do_while.statement),
                                                         ycf_node_to_string(candidate->u.do_while.expression.content.content.head)));
      return wrapper;
    } else if(candidate->type == ycf_node_type_while &&
       ycf_node_list_length(candidate->u.while_n.expression.content.content) == 1 &&
       candidate->u.while_n.expression.content.content.head->type == ycf_node_type_function_call &&
       ycf_string_item_list_contains(list,
                                     ycf_symbol_get_text(candidate->u.while_n.expression.content.content.head->u.function_call.identifier))){
      break_up_control_expressions(&candidate->u.while_n.statement->u.code_scope, list);
      ycf_node * wrapper =
        ycf_node_get_from_code_scope_text(ycf_string_new("int ycf_gen_control_tmp;\n"
                                                         "ycf_gen_control_tmp = !!%s;\n"
                                                         "while(ycf_gen_control_tmp){\n"
                                                         "  %s\n"
                                                         "  ycf_gen_control_tmp = !!%s;\n"
                                                         "}\n",
                                                         ycf_node_to_string(candidate->u.while_n.expression.content.content.head),
                                                         ycf_node_to_string(candidate->u.while_n.statement),
                                                         ycf_node_to_string(candidate->u.while_n.expression.content.content.head)));
      return wrapper;
    }
    return candidate;
}

static
void break_up_control_expressions(ycf_node_code_scope* s,
                                  ycf_string_item_list* yielding_function_names){
  ycf_node_insert_scopes_in_complex_statements(s);
  ycf_node_search_and_replace_statements_in_scope(s,
                                                  break_up_control_expressions_helper,
                                                  yielding_function_names);
}

static
ycf_node* mk_wrap_in_surpress_warn(char* warning, ycf_node* to_wrap) {
  ycf_string_printable_buffer* buf = ycf_string_printable_buffer_new();
  ycf_string_printable_buffer_printf(buf,
                                     "\n"
                                     "/* clang-format off */\n"
                                     "YCF_GCC_DIAG_OFF(%s)\n"
                                     "/* clang-format on */\n", warning);
  ycf_node_print(to_wrap, buf);
  ycf_string_printable_buffer_printf(buf,
                                     "\n"
                                     "/* clang-format off */\n"
                                     "YCF_GCC_DIAG_ON(%s)\n"
                                     "/* clang-format on */\n", warning);
  return ycf_node_new_text_node(buf->buffer);

}

static
ycf_node* supress_warnings_wrap_yielding_fun(ycf_node* yielding_fun){
  ycf_node* ret = yielding_fun;
  ret = mk_wrap_in_surpress_warn("uninitialized", ret);
  ret = mk_wrap_in_surpress_warn("maybe-uninitialized", ret);
  ret = mk_wrap_in_surpress_warn("sometimes-uninitialized", ret);
  ret = mk_wrap_in_surpress_warn("unknown-warning-option", ret);
  ret = mk_wrap_in_surpress_warn("pragmas", ret);
  return ret;
}

static
ycf_node* supress_warnings_wrap_destroy_fun(ycf_node* fun){
  ycf_node* ret = fun;
  ret = mk_wrap_in_surpress_warn("unused-function", ret);
  ret = mk_wrap_in_surpress_warn("unused-but-set-variable", ret);
  ret = mk_wrap_in_surpress_warn("unknown-warning-option", ret);
  ret = mk_wrap_in_surpress_warn("pragmas", ret);
  return ret;
}

ycf_node* insert_yielding_function_with_prefix_suffix(ycf_node* tree_to_insert_to,
                                                      ycf_node* insert_before,
                                                      ycf_node* yielding_fun,
                                                      bool debug_mode,
                                                      char* yielding_function_name,
                                                      char* ycf_trap_state_struct_name){
  ycf_node* prefix = ycf_node_new_text_node("\n"
                                            "#define YCF_IN_YIELDING_FUN 1\n"
                                            "#undef YCF_STACK_ALLOC\n"
                                            "#define YCF_STACK_ALLOC(SIZE) \\\n"
                                            "    ycf_stack_alloc(SIZE,\\\n"
                                            "                    &ycf_frame_alloc_data,\\\n"
                                            "                    ycf_yield_alloc, ycf_yield_alloc_free_context,\\\n"
                                            "                     ycf_stack_alloc_size_or_max_size)\n");
  if(insert_before == NULL){
    ycf_node_list_append(&tree_to_insert_to->u.c_file.content, prefix);
  }else{
    ycf_node_list_insert_before(&tree_to_insert_to->u.c_file.content,
                                insert_before,
                                prefix);
  }
  if(debug_mode){
    ycf_node* wrapper_funs =
      mk_debug_yielding_fun_call_wrapper(yielding_function_name,
                                         ycf_trap_state_struct_name,
                                         yielding_fun);
    ycf_node_rename_function(&yielding_fun->u.function,
                             ycf_string_new("%s_ycf_gen_yielding_3",
                                            yielding_function_name));

    if(insert_before == NULL){
      yielding_fun = supress_warnings_wrap_yielding_fun(yielding_fun);
      ycf_node_list_append(&tree_to_insert_to->u.c_file.content,
                           yielding_fun);
      ycf_node_list_append(&tree_to_insert_to->u.c_file.content, wrapper_funs);
    } else {
      yielding_fun = supress_warnings_wrap_yielding_fun(yielding_fun);
      ycf_node_list_insert_before(&tree_to_insert_to->u.c_file.content,
                                  insert_before,
                                  yielding_fun);
      ycf_node_list_insert_before(&tree_to_insert_to->u.c_file.content,
                                  insert_before,
                                  wrapper_funs);
    }
  }else {
    if(insert_before == NULL){
      yielding_fun = supress_warnings_wrap_yielding_fun(yielding_fun);
      ycf_node_list_append(&tree_to_insert_to->u.c_file.content,
                           yielding_fun);
    }else{
      yielding_fun = supress_warnings_wrap_yielding_fun(yielding_fun);
      ycf_node_list_insert_before(&tree_to_insert_to->u.c_file.content,
                                  insert_before,
                                  yielding_fun);
    }
  }
  if(insert_before == NULL){
    ycf_node_list_append(&tree_to_insert_to->u.c_file.content,
                         ycf_node_new_text_node("\n"
                                                "#undef YCF_STACK_ALLOC\n"
                                                "#undef YCF_IN_YIELDING_FUN\n"));
  }else{
    ycf_node_list_insert_after(&tree_to_insert_to->u.c_file.content,
                               yielding_fun,
                               ycf_node_new_text_node("\n"
                                                      "#undef YCF_STACK_ALLOC\n"
                                                      "#undef YCF_IN_YIELDING_FUN\n"
                                                      "#define YCF_STACK_ALLOC(SIZE) malloc(SIZE)\n"));
    ycf_node_list_insert_after(&tree_to_insert_to->u.c_file.content,
                               insert_before,
                               ycf_node_new_text_node("\n#undef YCF_STACK_ALLOC\n"));
  }
  return yielding_fun;
}

ycf_node* ast_get_ast_with_yieldified_function(ycf_node* source_tree,
                                               ycf_node* header_tree,
                                               char* yielding_function_name,
                                               ycf_string_item_list* all_yielding_function_names,
                                               bool auto_yield,
                                               bool recusive_auto_yield,
                                               bool debug_mode,
                                               bool only_yielding_funs,
                                               ycf_node** only_yielding_funs_tree,
                                               bool static_aux_funs)
{
  ycf_yield_location_id_counter = 0;
  ycf_node* tree_ret = ycf_node_deep_copy(source_tree);
  if (*only_yielding_funs_tree == NULL){
    *only_yielding_funs_tree = ycf_node_c_file_new(ycf_node_list_empty());
  }
  /* Find function */
  ycf_node* fun = ycf_node_find_function(tree_ret, yielding_function_name);
  if(fun == NULL){
    fprintf(stderr, "Could not find function %s\n", yielding_function_name);
    exit(1);
  }
  ycf_node* fun_change = ycf_node_deep_copy(fun);
  /* Replace alternative syntax for special code sections */
  fun_change = replace_alt_syntax_special_code_section_code(fun_change);
  ycf_node_normalize_function(fun_change);
  /* Brake up control expressions with simple calls to yielding functions */
  break_up_control_expressions(&fun_change->u.function.body, all_yielding_function_names);
  /* Insert trap state var for calls to yielding functions */
  insert_fun_call_state_var(&fun_change->u.function.body, all_yielding_function_names);
  /* Normalize the function to make transformation easier (move all declarations to the top etc) */
  ycf_node_normalize_function(fun_change);
  /* Insert YCF_CONSUME_REDS(1) code if auto yielding is on */
  if(auto_yield){
    insert_consume_reds_calls(&fun_change->u.function.body);
  }
  /* Save variable declaraions */
  ycf_node_list uniqified_parameters =
    ycf_node_list_shallow_copy(fun_change->u.function.definition.parameters);
  ycf_node_list scope_defs =
    ycf_node_list_shallow_copy(ycf_node_get_declarations_in_scope(&fun_change->u.function.body));
  ycf_node_list defs =
    ycf_node_list_shallow_copy(ycf_node_get_all_definitions_in_function(&fun_change->u.function));
  /* Add extra vaiables that are needed for yielding */
  ycf_node_list extra_ycf_trap_state = mk_trap_extra_state();
  ycf_node_list_concat(&scope_defs, &extra_ycf_trap_state);
  fun_change->u.function.body.definition_nodes = scope_defs;
  /* Generate trap state struct */
  ycf_node_list trap_state_struct_var_declarations =
    ycf_node_list_shallow_copy(extra_ycf_trap_state);
  ycf_node_list_concat(&trap_state_struct_var_declarations, &defs);
  char* ycf_trap_state_struct_name =
          ycf_string_new("gen_ycf_trap_state_for_%s",
                         yielding_function_name);
  trap_state_struct_var_declarations =
    ycf_node_list_copy_concat(mk_saved_ycf_trap_state_params(),
                              trap_state_struct_var_declarations);
  ycf_node* ycf_trap_state_struct =
    mk_typedef_struct_node(trap_state_struct_var_declarations,
                           ycf_trap_state_struct_name);
  /* Add extra parameters for trapping */
  {
    ycf_node_list trap_params = mk_ycf_trap_state_params();
    ycf_node_list saved_params = mk_saved_ycf_trap_state_params();
    ycf_node_list_concat(&trap_params, &saved_params);
    ycf_node_list_concat(&trap_params, &fun_change->u.function.definition.parameters);
    fun_change->u.function.definition.parameters = trap_params;
    fun_change->u.function.definition.ignore_param_ending = 1;
  }
  /* Collect and replace special code */
  ycf_node_list on_save_yield_state_code_list =
    save_and_replace_special_code(&fun_change->u.function.body,
                                  ycf_node_type_on_save_yield_state_code);
  ycf_node_list on_restore_yield_state_code_list =
    save_and_replace_special_code(&fun_change->u.function.body,
                                  ycf_node_type_on_restore_yield_state_code);
  ycf_node_list on_destroy_state_code_list =
    save_and_replace_special_code(&fun_change->u.function.body,
                                  ycf_node_type_on_destroy_state_code);
  ycf_node_list on_destroy_state_or_return_code_list =
    save_and_replace_special_code(&fun_change->u.function.body,
                                  ycf_node_type_on_destroy_state_or_return_code);
  ycf_node_list on_return_code_list =
    save_and_replace_special_code(&fun_change->u.function.body, ycf_node_type_on_return_code);
  save_nr_of_reductions_before_return(&fun_change->u.function.body,
                                          on_destroy_state_or_return_code_list,
                                          on_return_code_list,
                                          recusive_auto_yield);
  /* Insert goto yield code in function */
  insert_yield_code(fun_change,
                    ycf_trap_state_struct_name,
                    trap_state_struct_var_declarations,
                    &fun_change->u.function.body);
  insert_yield_no_reds_code(fun_change,
                            ycf_trap_state_struct_name,
                            trap_state_struct_var_declarations,
                            &fun_change->u.function.body);
  {
    ycf_string_item_list fun_call_state_vars =
      insert_yielding_fun_call_code(source_tree,
                                    &fun_change->u.function.body,
                                    all_yielding_function_names,
                                    yielding_function_name,
                                    recusive_auto_yield);
    /* Null all fun_call_state_vars */
    ycf_node_list_prepend(&fun_change->u.function.body.other_nodes, mk_null_vars_code(fun_call_state_vars));
  }
  /* Replace YCF_CONSUME_REDS calls */
  insert_consume_reds_code(yielding_function_name, &fun_change->u.function.body);
  /* Insert yield initialization in the beginning of the function */
  {
    ycf_node* trap_init = mk_yield_init_code(ycf_trap_state_struct_name,
                                             trap_state_struct_var_declarations,
                                             on_restore_yield_state_code_list,
                                             auto_yield,
                                             yielding_function_name);
    ycf_node_list_prepend(&fun_change->u.function.body.other_nodes, trap_init);
  }
  /* Add code that saves the state and yields to the end of the function */
  ycf_node_list_append(&fun_change->u.function.body.other_nodes,
                         mk_yield_code(fun_change,
                                       ycf_trap_state_struct_name,
                                       trap_state_struct_var_declarations,
                                       on_save_yield_state_code_list,
                                       debug_mode));
  /* Change name of function */
  ycf_node_rename_function(&fun_change->u.function,
                           ycf_string_new("%s_ycf_gen_yielding",
                                          yielding_function_name));
  /* Remove unecessary scopes */
  ycf_node_remove_unecessary_scopes(&fun_change->u.function.body);
  /* Make continue function */
  ycf_node* continue_function =
    mk_continue_function_node(yielding_function_name,
                              ycf_trap_state_struct_name,
                              fun,
                              uniqified_parameters);
  /* Make destroy state function */
  ycf_node* destroy_state_function =
    mk_destroy_state_function_node(yielding_function_name,
                                   trap_state_struct_var_declarations,
                                   ycf_trap_state_struct_name,
                                   on_destroy_state_code_list,
                                   on_destroy_state_or_return_code_list,
                                   static_aux_funs);
  ycf_node* fun_change_dec = mk_fun_def(fun_change);
  /****************************************************************
   *
   * The following code inserts the changed function and its helper
   * function into the output AST(s)
   *
   ****************************************************************/
  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
  /* Insert changed function into tree that will be printed */
  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
  ycf_node* fun_ret = ycf_node_find_function(tree_ret, yielding_function_name);
  ycf_node* fun_change_wrapper =
    insert_yielding_function_with_prefix_suffix(tree_ret,
                                                fun_ret,
                                                fun_change,
                                                debug_mode,
                                                yielding_function_name,
                                                ycf_trap_state_struct_name);
  /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
  /* END: Insert changed function into tree that will be printed */
  /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
  /* Insert declarations in the top */
  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
  ycf_node* fun_dec_node = ycf_node_find_function_declaration(tree_ret, yielding_function_name);
  ycf_node_list_prepend(&tree_ret->u.c_file.content, ycf_node_new_text_node("\n"));
  if(fun_dec_node == NULL){
    ycf_node_list_prepend(&tree_ret->u.c_file.content,
                          mk_wrap_in_surpress_warn("unused-function",
                                                   mk_fun_def(destroy_state_function)));
    ycf_node_list_prepend(&tree_ret->u.c_file.content,
                          mk_wrap_in_surpress_warn("unused-function",
                                                   mk_fun_def(continue_function)));
  } else {
    ycf_node_list_insert_before(&tree_ret->u.c_file.content,
                                fun_dec_node,
                                mk_wrap_in_surpress_warn("unused-function",
                                                         mk_fun_def(destroy_state_function)));
    ycf_node_list_insert_before(&tree_ret->u.c_file.content,
                                fun_dec_node,
                                mk_wrap_in_surpress_warn("unused-function",
                                                         mk_fun_def(continue_function)));

  }
  ycf_node_list_prepend(&header_tree->u.c_file.content, ycf_node_shallow_copy(fun_change_dec));
  ycf_node_list_prepend(&header_tree->u.c_file.content, ycf_node_new_text_node("\n"));
  ycf_node_list_prepend(&header_tree->u.c_file.content,
                        mk_wrap_in_surpress_warn("unused-function",
                                                 mk_fun_def(destroy_state_function)));
  ycf_node_list_prepend(&header_tree->u.c_file.content,
                        mk_wrap_in_surpress_warn("unused-function",
                                                 mk_fun_def(continue_function)));
  ycf_node* ycf_trap_state_struct_dec =
          ycf_node_new_text_node(ycf_string_new("\n\nstruct %s;",
                                                ycf_trap_state_struct->u.gen_typedef_struct.name));
  ycf_node_list_prepend(&tree_ret->u.c_file.content,
                        ycf_trap_state_struct_dec);
  ycf_node_list_prepend(&header_tree->u.c_file.content,
                        ycf_node_shallow_copy(ycf_trap_state_struct_dec));
  ycf_node_list_append(&header_tree->u.c_file.content,
                       ycf_node_new_text_node("\n"));
  /* Insert definition of changed function */
  if(fun_dec_node != NULL){
    ycf_node_list_insert_before(&tree_ret->u.c_file.content,
                                fun_dec_node,
                                ycf_node_shallow_copy(ycf_node_shallow_copy(fun_change_dec)));
  }else{
    ycf_node_list_insert_before(&tree_ret->u.c_file.content,
                                fun_change_wrapper,
                                ycf_node_shallow_copy(ycf_node_shallow_copy(fun_change_dec)));
  }
  ycf_node_list_insert_before(&tree_ret->u.c_file.content, fun_change_wrapper, ycf_trap_state_struct);
  ycf_node_list_insert_after(&tree_ret->u.c_file.content,
                             ycf_trap_state_struct,
                             supress_warnings_wrap_destroy_fun(destroy_state_function));
  ycf_node_list_insert_after(&tree_ret->u.c_file.content,
                             ycf_trap_state_struct,
                             mk_wrap_in_surpress_warn("unused-function",
                                                      continue_function));
  /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
  /* END: Insert declarations in the top */
  /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
  if(only_yielding_funs){
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_shallow_copy(fun_change_dec));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          mk_wrap_in_surpress_warn("unused-function",mk_fun_def(destroy_state_function)));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          mk_wrap_in_surpress_warn("unused-function",
                                                   mk_fun_def(continue_function)));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_shallow_copy(ycf_trap_state_struct_dec));
    ycf_node_list_prepend(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));

    ycf_node_list_append(&(*only_yielding_funs_tree)->u.c_file.content,
                         ycf_node_shallow_copy(ycf_trap_state_struct));
    ycf_node_list_append(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));
    ycf_node_list_append(&(*only_yielding_funs_tree)->u.c_file.content,
                         supress_warnings_wrap_destroy_fun(destroy_state_function));
    ycf_node_list_append(&(*only_yielding_funs_tree)->u.c_file.content,
                          ycf_node_new_text_node("\n"));
    ycf_node_list_append(&(*only_yielding_funs_tree)->u.c_file.content,
                         mk_wrap_in_surpress_warn("unused-function",continue_function));
    insert_yielding_function_with_prefix_suffix((*only_yielding_funs_tree),
                                                NULL,
                                                fun_change,
                                                debug_mode,
                                                yielding_function_name,
                                                ycf_trap_state_struct_name);
  }
  return tree_ret;
}
