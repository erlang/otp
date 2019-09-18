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

#include "ycf_yield_fun.h"
#include "ycf_utils.h"
#include "ycf_node.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void print_symbol_code(ycf_symbol* s);
ycf_parse_result parse_exp_statement(ycf_symbol* symbols);

ycf_parse_result fail_parse_result(){
  ycf_parse_result r;
  r.success = false;
  r.next_symbol = NULL;
  r.result = NULL;
  return r;
}

ycf_parse_result success_parse_result(ycf_symbol* next,
                                      ycf_node* result){
  ycf_parse_result r;
  r.success = true;
  r.next_symbol = next;
  r.result = result;
  return r;
}

typedef struct {
  ycf_node_list list;
  ycf_symbol* next_symbol;
} parse_list_res;

typedef struct {
  ycf_symbol_list list;
  ycf_symbol* next_symbol;
} parse_symbol_list_res;


parse_list_res parse_list_generic(int number_of_parsers,
                                  ycf_parse_result (*parsers[])(ycf_symbol*),
                                  ycf_symbol* symbols,
                                  bool use_break_symbol,
                                  int nr_of_break_symbols,
                                  ycf_symbol_type break_symbols[]){
  parse_list_res r;
  ycf_symbol* current = symbols;
  int i;
  r.list = ycf_node_list_empty();
  while(current != NULL){
    if(use_break_symbol){
      bool is_break = false;
      for(int i = 0; i < nr_of_break_symbols; i++){
        if(break_symbols[i] == current->type){
          is_break = true;
        }
      }
      if(is_break){
        break;
      }
    }
    ycf_symbol* prev_current_symbol = current;
    for(i = 0; i < number_of_parsers; i++){
      ycf_parse_result res = parsers[i](current);
      if(res.success){
        ycf_node_list_append(&r.list, res.result);
        current = res.next_symbol;
        break;
      }
    }
    if(prev_current_symbol == current){
      break;
    }
  }
  r.next_symbol = current;
  return r;
}

parse_list_res parse_list(int number_of_parsers,
                          ycf_parse_result (*parsers[])(ycf_symbol*),
                          ycf_symbol* symbols){
  return parse_list_generic(number_of_parsers,
                            parsers,
                            symbols,
                            false,
                            0,
                            NULL);

}

parse_list_res parse_list_break_symbol(int number_of_parsers,
                                       ycf_parse_result (*parsers[])(ycf_symbol*),
                                       ycf_symbol* symbols,
                                       ycf_symbol_type break_symbol){
  ycf_symbol_type break_s[1] = {break_symbol};
  return parse_list_generic(number_of_parsers,
                            parsers,
                            symbols,
                            true,
                            1,
                            break_s);

}

parse_list_res parse_list_break_symbols(int number_of_parsers,
                                        ycf_parse_result (*parsers[])(ycf_symbol*),
                                        ycf_symbol* symbols,
                                        int nr_of_break_symbols,
                                        ycf_symbol_type break_symbols[]){
  return parse_list_generic(number_of_parsers,
                            parsers,
                            symbols,
                            true,
                            nr_of_break_symbols,
                            break_symbols);

}


ycf_node* ycf_node_defenition_new(ycf_symbol_list type_spec,
                                  ycf_symbol* id,
                                  ycf_node_list array_brackets,
                                  ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_variable_definition;
  n->next = NULL;
  n->u.definition.identifier = ycf_symbol_copy(id);
  n->u.definition.type_specifiers = type_spec;
  n->u.definition.array_brackets = array_brackets;
  n->u.definition.end = ycf_symbol_copy(end);
  return n;
}

ycf_node* ycf_node_defenition_with_init_new(ycf_node_definition def,
                                            ycf_symbol_list expression,
                                            ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_variable_definition_init;
  n->next = NULL;
  n->u.definition_init.definition = def;
  n->u.definition_init.initializer_expression = expression;
  n->u.definition_init.end = ycf_symbol_copy(end);
  return n;
}

ycf_node* ycf_node_c_file_new(ycf_node_list content){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_c_file;
  n->next = NULL;
  n->u.c_file.content = content;
  return n;
}


ycf_node* ycf_node_function_def_new(ycf_node_definition def_node,
                                    ycf_node_list parameters,
                                    bool ignore_param_ending,
                                    ycf_symbol_list end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_function_declaration;
  n->next = NULL;
  n->u.function_definition.definition = def_node;
  n->u.function_definition.parameters = parameters;
  n->u.function_definition.ignore_param_ending = ignore_param_ending;
  n->u.function_definition.end = end;
  return n;
}

ycf_node* ycf_node_yield_new(ycf_symbol* yield_symbol,
                             ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_yield;
  n->next = NULL;
  n->u.yield.yield_symbol = ycf_symbol_copy(yield_symbol);
  n->u.yield.end_symbol = ycf_symbol_copy(end);
  return n;
}



ycf_node* ycf_node_consume_reds_new(ycf_symbol* consume_reds_symbol,
                                    ycf_node_parentheses_expression nr_of_reds_expression,
                                    ycf_symbol* end_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_consume_reds;
  n->next = NULL;
  n->u.consume_reds.consume_reds_symbol = ycf_symbol_copy(consume_reds_symbol);
  n->u.consume_reds.nr_of_reds_expression = nr_of_reds_expression;
  n->u.consume_reds.end_symbol = ycf_symbol_copy(end_symbol);
  return n;
}


ycf_node* ycf_node_other_new(ycf_symbol* other_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_other;
  n->next = NULL;
  n->u.other.what = ycf_symbol_copy(other_symbol);
  return n;
}

ycf_node* ycf_node_scope_new(ycf_symbol* start,
                             ycf_node_list declaration_nodes,
                             ycf_node_list other_nodes,
                             ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_code_scope;
  n->next = NULL;
  n->u.code_scope.start = ycf_symbol_copy(start);
  n->u.code_scope.definition_nodes = declaration_nodes;
  n->u.code_scope.other_nodes = other_nodes;
  n->u.code_scope.end = ycf_symbol_copy(end);
  return n;
}

ycf_node* ycf_node_function_new(ycf_node_function_definition fun_def,
                                ycf_node_code_scope body){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_function_definition;
  n->next = NULL;
  n->u.function.definition = fun_def;
  n->u.function.body = body;
  return n;
}

ycf_node* ycf_node_function_call_new(ycf_symbol_list neg_symbols,
                                     ycf_symbol* ident,
                                     ycf_symbol* paran_start,
                                     ycf_node_list parameters,
                                     ycf_symbol* paran_end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_function_call;
  n->next = NULL;
  n->u.function_call.neg_symbols = neg_symbols;
  n->u.function_call.identifier = ident;
  n->u.function_call.start_symbol = paran_start;
  n->u.function_call.parameter_expressions = parameters;
  n->u.function_call.end_symbol = paran_end;
  return n;
}

ycf_node* ycf_node_paran_expression_new(ycf_symbol* start, ycf_node_expression expr, ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_parentheses_expression;
  n->next = NULL;
  n->u.parentheses_expression.start_symbol = start;
  n->u.parentheses_expression.content = expr;
  n->u.parentheses_expression.end_symbol = end;
  return n;
}

ycf_node* ycf_node_expression_new(ycf_node_list expr){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_expression;
  n->next = NULL;
  n->u.expression.content  = expr;
  n->u.expression.end_symbol = NULL;
  return n;
}

ycf_node* ycf_node_statement_new(ycf_node* expression,
                                 ycf_symbol* end_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->type = ycf_node_type_statement;
  n->next = NULL;
  n->u.statement.expression = expression;
  n->u.statement.end_symbol = end_symbol;
  return n;
}

ycf_node* ycf_node_while_new(ycf_symbol* while_word,
                             ycf_node_parentheses_expression expression,
                             ycf_node* statement){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_while;
  n->u.while_n.while_word = while_word;
  n->u.while_n.expression = expression;
  n->u.while_n.statement = statement;
  return n;
}



ycf_node* ycf_node_do_while_new(ycf_symbol* do_word,
                                ycf_node* statm,
                                ycf_symbol* while_word,
                                ycf_node_parentheses_expression expression,
                                ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_do_while;
  n->u.do_while.do_word = do_word;
  n->u.do_while.statement = statm;
  n->u.do_while.while_word = while_word;
  n->u.do_while.expression = expression;
  n->u.do_while.end = end;
  return n;
}

ycf_node* ycf_node_for_new(ycf_symbol* for_word,
                           ycf_symbol* start_paran,
                           struct ycf_node* init,
                           ycf_node* stop_cond,
                           ycf_symbol* stop_cond_end,
                           ycf_node* end_exp,
                           ycf_symbol* end_paran,
                           ycf_node* statem){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_for;
  n->u.for_n.for_word = for_word;
  n->u.for_n.start_parentheses = start_paran;
  n->u.for_n.init = init;
  n->u.for_n.stop_cond = stop_cond;
  n->u.for_n.stop_cond_end = stop_cond_end;
  n->u.for_n.end_exp = end_exp;
  n->u.for_n.end_parentheses = end_paran;
  n->u.for_n.statement = statem;
  return n;
}

ycf_node* ycf_node_switch_new(ycf_symbol* switch_word,
                              ycf_node_parentheses_expression expression,
                              ycf_node_code_scope scope){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_switch;
  n->u.switch_n.switch_word = switch_word;
  n->u.switch_n.expression = expression;
  n->u.switch_n.scope = scope;
  return n;
}

ycf_node* ycf_node_if_new(ycf_symbol* if_word,
                          ycf_node_parentheses_expression expression,
                          struct ycf_node* if_statem){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_if;
  n->u.if_n.if_word = if_word;
  n->u.if_n.expression = expression;
  n->u.if_n.if_statement = if_statem;
  return n;
}

ycf_node* ycf_node_if_else_new(ycf_node_if if_n,
                               ycf_symbol* else_word,
                               struct ycf_node* else_statement){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_if_else;
  n->u.if_else.if_part = if_n;
  n->u.if_else.else_word = else_word;
  n->u.if_else.else_statement = else_statement;
  return n;
}


ycf_node* ycf_node_fun_call_assignment_new(ycf_node_expression left_side,
                                           ycf_symbol* assignment_symbol,
                                           ycf_node_function_call fun_call){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_assignment_function_call;
  n->u.function_call_assignment.left_side = left_side;
  n->u.function_call_assignment.assignment_symbol = assignment_symbol;
  n->u.function_call_assignment.fun_call = fun_call;
  return n;
}

ycf_node* ycf_node_assignment_new(ycf_node_expression left_side,
                                  ycf_symbol* assignment_symbol,
                                  ycf_node_expression right_side,
                                  ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_assignment;
  n->u.a.left_side = left_side;
  n->u.a.right_side = right_side;
  n->u.a.assignment_symbol = assignment_symbol;
  n->u.a.end = end;
  return n;
}

ycf_node* ycf_node_comma_new(ycf_symbol* comma_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_comma;
  n->u.comma.comma_symbol = comma_symbol;
  return n;
}

ycf_node* ycf_node_array_bracket_new(ycf_symbol* start,
                                     bool empty,
                                     ycf_node_expression content,
                                     ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_array_bracket;
  n->u.array_bracket.start = start;
  n->u.array_bracket.empty = empty;
  n->u.array_bracket.content = content;
  n->u.array_bracket.end = end;
  return n;
}

ycf_node* ycf_node_macro_cmd_new(ycf_symbol* macro_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_macro_cmd;
  n->u.macro_cmd.symbol = macro_symbol;
  return n;
}

ycf_node* ycf_node_special_code_block_new(ycf_node_type type,
                                          ycf_symbol* start,
                                          ycf_node_if code,
                                          ycf_symbol* end){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = type;
  n->u.special_code_block.start = start;
  n->u.special_code_block.code = code;
  n->u.special_code_block.end = end;
  return n;
}

ycf_node* ycf_node_goto_new(ycf_symbol* goto_symbol,
                            ycf_symbol* label_symbol,
                            ycf_symbol* end_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_goto;
  n->u.goto_n.goto_symbol = goto_symbol;
  n->u.goto_n.label_symbol = label_symbol;
  n->u.goto_n.end_symbol = end_symbol;
  return n;
}

ycf_node* ycf_node_return_new(ycf_symbol* return_symbol,
                              ycf_node* return_expression,
                              ycf_symbol* end_symbol){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_return_statement;
  n->u.return_n.return_symbol = return_symbol;
  n->u.return_n.return_expression = return_expression;
  n->u.return_n.end_symbol = end_symbol;
  return n;
}


ycf_node* ycf_node_period_field_access_new(ycf_symbol* period,
                                           ycf_symbol* field_name){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_period_field_access;
  n->u.period_field_access.period = period;
  n->u.period_field_access.field_name = field_name;
  return n;
}

ycf_node* ycf_pointer_field_access_new(ycf_symbol* pointer,
                                       ycf_symbol* field_name){
  ycf_node* n = ycf_malloc(sizeof(ycf_node));
  n->next = NULL;
  n->type = ycf_node_type_period_field_access;
  n->u.pointer_field_access.pointer = pointer;
  n->u.pointer_field_access.field_name = field_name;
  return n;
}

parse_symbol_list_res parse_symbol_list(int nr_of_aloved_symbols,
                                        ycf_symbol_type accept_symbols[],
                                        ycf_symbol* symbols){
  int i;
  parse_symbol_list_res r;
  ycf_symbol* current = symbols;
  r.list = ycf_symbol_list_empty();
  while (current != NULL){
    ycf_symbol* prev_current = current;
    for(i = 0; i < nr_of_aloved_symbols; i++){
      if(current->type == accept_symbols[i]){
        ycf_symbol_list_append(&r.list, ycf_symbol_copy(current));
        current = current->next;
      }
    }
    if(prev_current == current){
      break;
    }
  }
  r.next_symbol = current;
  return r;
}

parse_symbol_list_res
parse_symbol_list_until(ycf_symbol* symbols, ycf_symbol_type end_symbol){
  parse_symbol_list_res r;
  ycf_symbol* current = symbols;
  r.list = ycf_symbol_list_empty();
  while (current->type != end_symbol){
    ycf_symbol_list_append(&r.list, ycf_symbol_copy(current));
    current = current->next;
  }
  r.next_symbol = current;
  return r;
}

ycf_parse_result parse_expression_end_end_squarebracket(ycf_symbol* symbols);

ycf_parse_result parse_array_bracket(ycf_symbol* symbols){
  if(symbols->type != ycf_symbol_type_open_square_bracket){
    return fail_parse_result();
  }
  if(symbols->next->type == ycf_symbol_type_end_square_bracket){
    ycf_node_expression empty = {.content = ycf_node_list_empty(), .end_symbol = NULL};
    return success_parse_result(symbols->next->next,
                                ycf_node_array_bracket_new(symbols, true, empty, symbols->next));
  }
  ycf_parse_result square_bracket_content =
    parse_expression_end_end_squarebracket(symbols->next);
  if(!square_bracket_content.success){
    return fail_parse_result();
  }
  return success_parse_result(square_bracket_content.next_symbol->next,
                              ycf_node_array_bracket_new(symbols,
                                                         false,
                                                         square_bracket_content.result->u.expression,
                                                         square_bracket_content.next_symbol));
}

ycf_parse_result parse_defenition_until_identifier(ycf_symbol* symbols,
                                                   ycf_symbol_type end_symbol_type){
  ycf_symbol* current = symbols;
  ycf_symbol_list type_specifier = ycf_symbol_list_empty();
  ycf_symbol* ident = NULL;
  ycf_node_list array_brackets;
  /* Parse modifiers */
  {
    int nr_of_aloved_symbols = 4;
    ycf_symbol_type accept_symbols[4];
    accept_symbols[0] = ycf_symbol_type_static;
    accept_symbols[1] = ycf_symbol_type_const;
    accept_symbols[2] = ycf_symbol_type_inline;
    accept_symbols[3] = ycf_symbol_type_volatile;
    parse_symbol_list_res more =
      parse_symbol_list(nr_of_aloved_symbols,
                        accept_symbols,
                        current);
    ycf_symbol_list_concat(&type_specifier, &more.list);
    current = more.next_symbol;
  }
  /* Parse first symbol */
  if(current->type == ycf_symbol_type_identifier ||
     current->type == ycf_symbol_type_void){
    ycf_symbol_list_append(&type_specifier, ycf_symbol_copy(current));
    current = current->next;
  } else {
    return fail_parse_result();
  }
  if(type_specifier.last->type == ycf_symbol_type_identifier){
    /* Parse remaining identifiers if first symbol is ycf_symbol_type_identifier */
    int nr_of_aloved_symbols = 2;
    ycf_symbol_type accept_symbols[2];
    accept_symbols[0] = ycf_symbol_type_identifier;
    accept_symbols[1] = ycf_symbol_type_const;
    parse_symbol_list_res more =
      parse_symbol_list(nr_of_aloved_symbols,
                        accept_symbols,
                        current);
    ycf_symbol_list_concat(&type_specifier, &more.list);
    current = more.next_symbol;
  }
  {
    /* Parse stars */
    int nr_of_aloved_symbols = 1;
    ycf_symbol_type accept_symbols[1];
    accept_symbols[0] = ycf_symbol_type_star;
    parse_symbol_list_res more =
      parse_symbol_list(nr_of_aloved_symbols,
                        accept_symbols,
                        current);
    ycf_symbol_list_concat(&type_specifier, &more.list);
    current = more.next_symbol;
  }
  /* Handle ycf_symbol_type_identifier */
  if(type_specifier.last->type == ycf_symbol_type_identifier){
    ident = type_specifier.last;
    ycf_symbol_list_remove(&type_specifier, type_specifier.last);
    if (type_specifier.head == NULL) {
      return fail_parse_result();
    }
  } else if (current->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  } else {
    ident = current;
    current = current->next;
  }
  /* Handle array brackets */
  {
    ycf_parse_result (*parsers[])(ycf_symbol *) = {
      parse_array_bracket
    };
    parse_list_res res = parse_list(1, parsers, current);
    array_brackets = res.list;
    current = res.next_symbol;
  }
  /* Parse end symbol */
  if (current == NULL ||
      current->type != end_symbol_type){
    return fail_parse_result();
  } else {
    return success_parse_result(current->next,
                                ycf_node_defenition_new(type_specifier,
                                                        ident,
                                                        array_brackets,
                                                        current));
  }
}

ycf_parse_result parse_defenition_no_init(ycf_symbol* symbols){
  ycf_parse_result res = parse_defenition_until_identifier(symbols, ycf_symbol_type_semicolon);
  return res;
}

ycf_parse_result parse_defenition_with_init(ycf_symbol* symbols){
  ycf_parse_result dec = parse_defenition_until_identifier(symbols, ycf_symbol_type_equal_sign);
  parse_symbol_list_res expression_list_res;
  if(!dec.success){
    return fail_parse_result();
  }
  expression_list_res =
    parse_symbol_list_until(dec.next_symbol, ycf_symbol_type_semicolon);
  return success_parse_result(expression_list_res.next_symbol->next,
                              ycf_node_defenition_with_init_new(dec.result->u.definition,
                                                                expression_list_res.list,
                                                                expression_list_res.next_symbol));
}

ycf_parse_result parse_defenition_comma(ycf_symbol* symbols){
  ycf_parse_result res = parse_defenition_until_identifier(symbols, ycf_symbol_type_comma);
  return res;
}

ycf_parse_result parse_defenition_end_paran(ycf_symbol* symbols){
  ycf_parse_result res = parse_defenition_until_identifier(symbols, ycf_symbol_type_end_parenthesis);
  return res;
}

ycf_parse_result parse_function_head(ycf_symbol* symbols,
                                     ycf_symbol_type end_symbol,
                                     bool exclude_end_symbol){
  ycf_symbol* current = symbols;
  ycf_parse_result def_res =
    parse_defenition_until_identifier(current,
                                      ycf_symbol_type_open_parenthesis);
  if(!def_res.success){
    return fail_parse_result();
  }
  current = def_res.next_symbol;

  /* Parse function without parameters */

  if(current->type == ycf_symbol_type_end_parenthesis &&
     current->next->type == end_symbol){
    ycf_symbol_list end = ycf_symbol_list_empty();
    ycf_symbol* next_symbol;
    ycf_node_list parameter_list = ycf_node_list_empty();
    ycf_symbol_list_append(&end, ycf_symbol_copy(current));
    if(!exclude_end_symbol){
      next_symbol = current->next->next;
      ycf_symbol_list_append(&end, ycf_symbol_copy(current->next));
    } else {
      next_symbol = current->next;
    }
    return success_parse_result(next_symbol,
                                ycf_node_function_def_new(def_res.result->u.definition,
                                                          parameter_list,
                                                          false,
                                                          end));
  } else if(current->type == ycf_symbol_type_void &&
            current->next->type == ycf_symbol_type_end_parenthesis &&
     current->next->next->type == end_symbol){
    ycf_symbol_list end = ycf_symbol_list_empty();
    ycf_symbol* next_symbol;
    ycf_node_list parameter_list = ycf_node_list_empty();
    ycf_symbol_list_append(&end, ycf_symbol_copy(current));
    ycf_symbol_list_append(&end, ycf_symbol_copy(current->next));
    if(!exclude_end_symbol){
      next_symbol = current->next->next->next;
      ycf_symbol_list_append(&end, ycf_symbol_copy(current->next->next));
    }else{
      next_symbol = current->next->next;
    }
    return success_parse_result(next_symbol,
                                ycf_node_function_def_new(def_res.result->u.definition,
                                                          parameter_list,
                                                          false,
                                                          end));
  }

  /* Parse parameters */

  int number_of_parsers = 2;
    ycf_parse_result (*parsers[])(ycf_symbol *) = {
    parse_defenition_comma,
    parse_defenition_end_paran
  };
  parse_list_res param_list =
    parse_list(number_of_parsers, parsers, current);
  if(param_list.next_symbol == NULL ||
     param_list.next_symbol->type != end_symbol){
    return fail_parse_result();
  }

  /* Parse ending */

  {
    ycf_symbol_list end = ycf_symbol_list_empty();
    ycf_symbol* next_symbol;
    if(!exclude_end_symbol){
      next_symbol = param_list.next_symbol->next;
      ycf_symbol_list_append(&end, ycf_symbol_copy(param_list.next_symbol));
    }else{
      next_symbol = param_list.next_symbol;
    }
    return success_parse_result(next_symbol,
                                ycf_node_function_def_new(def_res.result->u.definition,
                                                          param_list.list,
                                                          false,
                                                          end));
  }
}

ycf_parse_result parse_function_def(ycf_symbol* symbols){
  return parse_function_head(symbols, ycf_symbol_type_semicolon, false);
}

ycf_parse_result parse_other(ycf_symbol* symbols){
  ycf_symbol* what = symbols;
  return success_parse_result(symbols->next,
                              ycf_node_other_new(what));
}


ycf_parse_result parse_goto(ycf_symbol* symbols){
  ycf_symbol* goto_symbol = symbols;
  if(goto_symbol->type != ycf_symbol_type_goto){
    return fail_parse_result();
  }
  ycf_symbol* label_symbol = goto_symbol->next;
  if(label_symbol->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  }
  ycf_symbol* end_symbol = label_symbol->next;
  if(end_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(end_symbol->next,
                              ycf_node_goto_new(goto_symbol, label_symbol, end_symbol));
}


ycf_parse_result parse_comma(ycf_symbol* symbols){
  if(symbols->type == ycf_symbol_type_comma){
    return success_parse_result(symbols->next, ycf_node_comma_new(symbols));
  } else {
    return fail_parse_result();
  }
}


ycf_parse_result parse_pointer_field_access_node(ycf_symbol* symbols){
  ycf_symbol* pointer = symbols;
  if(pointer->type != ycf_symbol_type_pointer_field_access){
    return fail_parse_result();
  }
  ycf_symbol* field_name = pointer->next;
  if(field_name->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  }
  return success_parse_result(field_name->next,
                              ycf_pointer_field_access_new(pointer, field_name));
}

ycf_parse_result parse_period_field_access_node(ycf_symbol* symbols){
  ycf_symbol* p = symbols;
  if(p->type != ycf_symbol_type_period){
    return fail_parse_result();
  }
  ycf_symbol* field_name = p->next;
  if(field_name->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  }
  return success_parse_result(field_name->next,
                              ycf_node_period_field_access_new(p, field_name));
}

ycf_parse_result parse_function_call(ycf_symbol* symbols);
ycf_parse_result parse_expression_generic(ycf_symbol* symbols,
                                          bool use_end_symbol,
                                          bool no_function_call_assignment,
                                          int nr_of_end_symbols,
                                          ycf_symbol_type end_symbols[]);

ycf_parse_result parse_paran_expression(ycf_symbol* symbols){
  if(symbols->type != ycf_symbol_type_open_parenthesis){
    return fail_parse_result();
  }
  ycf_symbol* start = symbols;
  ycf_parse_result exp = parse_expression_generic(start->next,
                                                  true,
                                                  false,
                                                  1,
                                                  (ycf_symbol_type[]){ycf_symbol_type_end_parenthesis});
  if(!exp.success || exp.next_symbol->type != ycf_symbol_type_end_parenthesis ){
    return fail_parse_result();
  }
  return success_parse_result(exp.next_symbol->next,
                              ycf_node_paran_expression_new(start,
                                                            exp.result->u.expression,
                                                            exp.next_symbol));
}

ycf_parse_result parse_function_call_assignment(ycf_symbol* symbols);
ycf_parse_result parse_assignment(ycf_symbol* symbols);

ycf_parse_result parse_expression_generic(ycf_symbol* symbols,
                                          bool use_end_symbol,
                                          bool no_function_call_assignment,
                                          int nr_of_end_symbols,
                                          ycf_symbol_type end_symbols[]){
  int number_of_parsers = (!no_function_call_assignment ? 1 : 0) + 5;
  ycf_parse_result (*parsers[7])(ycf_symbol *);
  if(!no_function_call_assignment){
    parsers[0] = parse_function_call;
    parsers[1] = parse_function_call_assignment;
    parsers[2] = parse_paran_expression;
    parsers[3] = parse_period_field_access_node;
    parsers[4] = parse_pointer_field_access_node;
    parsers[5] = parse_other;
  }else{
    parsers[0] = parse_function_call;
    parsers[1] = parse_paran_expression;
    parsers[2] = parse_period_field_access_node;
    parsers[3] = parse_pointer_field_access_node;
    parsers[4] = parse_other;
  }
  parse_list_res res = parse_list_break_symbols(number_of_parsers, parsers, symbols, nr_of_end_symbols,end_symbols);
  if(res.list.head == NULL || (use_end_symbol &&
                               (res.next_symbol == NULL ||res.next_symbol->type != end_symbols[0]))){
    return fail_parse_result();
  }
  return success_parse_result(res.next_symbol,
                              ycf_node_expression_new(res.list));
}

ycf_parse_result parse_expression(ycf_symbol* symbols){
  return parse_expression_generic(symbols,
                                  false,
                                  false,
                                  0 /* ignored */,
                                  NULL);
}

ycf_parse_result parse_expression_end_comma(ycf_symbol* symbols){
  return parse_expression_generic(symbols,
                                  true,
                                  false,
                                  2,
                                  (ycf_symbol_type[]){ycf_symbol_type_comma, ycf_symbol_type_end_parenthesis});
}

ycf_parse_result parse_expression_end_end_paren(ycf_symbol* symbols){
  return parse_expression_generic(symbols,
                                  true,
                                  false,
                                  1,
                                  (ycf_symbol_type[]){ycf_symbol_type_end_parenthesis});
}

ycf_parse_result parse_expression_end_end_semicolon(ycf_symbol* symbols){
  return parse_expression_generic(symbols,
                                  true,
                                  false,
                                  1,
                                  (ycf_symbol_type[]){ycf_symbol_type_semicolon});
}

ycf_parse_result parse_expression_end_end_squarebracket(ycf_symbol* symbols){
  return parse_expression_generic(symbols,
                                  true,
                                  false,
                                  1,
                                  (ycf_symbol_type[]){ycf_symbol_type_end_square_bracket});
}


ycf_parse_result parse_function_call(ycf_symbol* symbols){
  parse_symbol_list_res negs;
  {
    /* Parse neg symbols */
    int nr_of_aloved_symbols = 1;
    ycf_symbol_type accept_symbols[1];
    accept_symbols[0] = ycf_symbol_type_neg;
    negs =
      parse_symbol_list(nr_of_aloved_symbols,
                        accept_symbols,
                        symbols);
  }
  ycf_symbol* ident = negs.next_symbol;
  if(ident == NULL ||
     ident->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  }
  ycf_symbol* paran_start = ident->next;
  if(paran_start == NULL || paran_start->type != ycf_symbol_type_open_parenthesis){
    return fail_parse_result();
  }
  int number_of_parsers = 3;
    ycf_parse_result (*parsers[])(ycf_symbol *) = {
    parse_comma,
    parse_expression_end_comma,
    parse_expression_end_end_paren
  };
  parse_list_res param_list_res =
    parse_list_break_symbol(number_of_parsers,
                            parsers,
                            paran_start->next,
                            ycf_symbol_type_end_parenthesis);
  if(param_list_res.next_symbol == NULL ||
     param_list_res.next_symbol->type != ycf_symbol_type_end_parenthesis){
    return fail_parse_result();
  }
  return success_parse_result(param_list_res.next_symbol->next,
                              ycf_node_function_call_new(negs.list,
                                                         ident,
                                                         paran_start,
                                                         param_list_res.list,
                                                         param_list_res.next_symbol));
}

ycf_parse_result parse_assignment(ycf_symbol* symbols) {
  if(symbols->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  }
  if(symbols->next == NULL || symbols->next->type != ycf_symbol_type_equal_sign){
    return fail_parse_result();
  }
  ycf_parse_result left_expression = parse_expression_generic(symbols,
                                                              false,
                                                              true,
                                                              3,
                                                              (ycf_symbol_type[]){
                                                                ycf_symbol_type_equal_sign,
                                                                  ycf_symbol_type_semicolon,
                                                                  ycf_symbol_type_end_parenthesis});
  if(!left_expression.success){
    return fail_parse_result();
  }
  ycf_symbol* equal_sign = left_expression.next_symbol;
  if(equal_sign == NULL || equal_sign->type != ycf_symbol_type_equal_sign) {
    return fail_parse_result();
  }
  if(equal_sign->next == NULL || equal_sign->next->type != ycf_symbol_type_identifier){
    return fail_parse_result();
  }
  if(equal_sign->next->next == NULL || equal_sign->next->next->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  ycf_parse_result right_expression = parse_expression_generic(equal_sign->next,
                                                               false,
                                                               true,
                                                               3,
                                                               (ycf_symbol_type[]){
                                                                 ycf_symbol_type_semicolon,
                                                                   ycf_symbol_type_equal_sign,
                                                                   ycf_symbol_type_end_parenthesis});
  if(!right_expression.success ||
     right_expression.next_symbol == NULL ||
     right_expression.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  ycf_node* node = ycf_node_assignment_new(left_expression.result->u.expression,
                                           ycf_symbol_copy(equal_sign),
                                           right_expression.result->u.expression,
                                           ycf_symbol_copy(right_expression.next_symbol));
  return success_parse_result(right_expression.next_symbol->next,
                              node);
}

ycf_parse_result parse_function_call_assignment(ycf_symbol* symbols){
  ycf_parse_result expression = parse_expression_generic(symbols,
                                                         true,
                                                         true,
                                                         3,
                                                         (ycf_symbol_type[]){ycf_symbol_type_equal_sign,
                                                                             ycf_symbol_type_semicolon,
                                                                             ycf_symbol_type_end_parenthesis});
  if(!expression.success){
    return fail_parse_result();
  }
  ycf_symbol* equal_sign = expression.next_symbol;
  ycf_parse_result fun_call = parse_function_call(equal_sign->next);
  if(!fun_call.success){
    return fail_parse_result();
  }
  return success_parse_result(fun_call.next_symbol,
                              ycf_node_fun_call_assignment_new(expression.result->u.expression,
                                                               equal_sign,
                                                               fun_call.result->u.function_call));
}


ycf_parse_result parse_function_call_assignment_statement(ycf_symbol* symbols){
  ycf_parse_result fun_call_ass = parse_function_call_assignment(symbols);
  if(!fun_call_ass.success || fun_call_ass.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(fun_call_ass.next_symbol->next,
                              ycf_node_statement_new(fun_call_ass.result,
                                                     fun_call_ass.next_symbol));
}


ycf_parse_result parse_function_call_statement(ycf_symbol* symbols){
  ycf_parse_result fun_call = parse_function_call(symbols);
  if(!fun_call.success || fun_call.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(fun_call.next_symbol->next,
                              ycf_node_statement_new(fun_call.result,
                                                     fun_call.next_symbol));
}

ycf_parse_result parse_statement(ycf_symbol* symbols);


ycf_parse_result parse_cond_statement(ycf_symbol* symbols, ycf_symbol_type cond_keyword){
  if(symbols->type != cond_keyword){
    return fail_parse_result();
  }
  ycf_symbol* cond_word = symbols;
  ycf_parse_result cond_exp = parse_paran_expression(cond_word->next);
  if(!cond_exp.success){
    return fail_parse_result();
  }
  ycf_parse_result cond_statement = parse_statement(cond_exp.next_symbol);
  if(!cond_statement.success){
    return fail_parse_result();
  }
  /* Return if node even though it might be something else */
  return success_parse_result(cond_statement.next_symbol,
                              ycf_node_if_new(cond_word,
                                              cond_exp.result->u.parentheses_expression,
                                              cond_statement.result));
}

ycf_parse_result parse_if_statement(ycf_symbol* symbols){
  return parse_cond_statement(symbols, ycf_symbol_type_if);
}

ycf_parse_result parse_if_else_statement(ycf_symbol* symbols){
  ycf_parse_result if_statem = parse_if_statement(symbols);
  if(!if_statem.success){
    return fail_parse_result();
  }
  if(if_statem.next_symbol->type != ycf_symbol_type_else){
    return fail_parse_result();
  }
  ycf_symbol* else_w = if_statem.next_symbol;
  ycf_parse_result else_statement = parse_statement(else_w->next);
  if(!else_statement.success){
    return fail_parse_result();
  }
  return success_parse_result(else_statement.next_symbol,
                              ycf_node_if_else_new(if_statem.result->u.if_n,
                                                   else_w,
                                                   else_statement.result));
}

ycf_parse_result parse_while_statement(ycf_symbol* symbols){
  ycf_parse_result cond_statem = parse_cond_statement(symbols, ycf_symbol_type_while);
  if(!cond_statem.success){
    return fail_parse_result();
  }
  return success_parse_result(cond_statem.next_symbol,
                              ycf_node_while_new(cond_statem.result->u.if_n.if_word,
                                                 cond_statem.result->u.if_n.expression,
                                                 cond_statem.result->u.if_n.if_statement));
}

ycf_parse_result parse_do_while_statement(ycf_symbol* symbols){
  if(symbols->type != ycf_symbol_type_do){
    return fail_parse_result();
  }
  ycf_symbol* do_word = symbols;
  ycf_parse_result statement = parse_statement(do_word->next);
  if(!statement.success || statement.next_symbol->type != ycf_symbol_type_while){
    return fail_parse_result();
  }
  ycf_symbol* while_word = statement.next_symbol;
  ycf_parse_result cond = parse_paran_expression(while_word->next);
  if(!cond.success){
    return fail_parse_result();
  }
  if(cond.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(cond.next_symbol->next,
                              ycf_node_do_while_new(do_word, statement.result, while_word,
                                                    cond.result->u.parentheses_expression, cond.next_symbol));
}

ycf_parse_result parse_for_statement(ycf_symbol* symbols){
  if(symbols->type != ycf_symbol_type_for){
    return fail_parse_result();
  }
  ycf_symbol* for_word = symbols;
  if(for_word->next->type != ycf_symbol_type_open_parenthesis){
    return fail_parse_result();
  }
  ycf_symbol* start_paran = for_word->next;
    ycf_node* init = NULL;
     ycf_parse_result init_res = parse_defenition_with_init(start_paran->next);
    if(!init_res.success){
        init_res = parse_defenition_no_init(start_paran->next);
    }
    if(!init_res.success){
        init_res = parse_exp_statement(start_paran->next);
    }
    if(!init_res.success){
        return fail_parse_result();
    }
    init = init_res.result;
  ycf_node* stop_cond = NULL;
  ycf_symbol* stop_cond_end = NULL;
  if(init_res.next_symbol->type == ycf_symbol_type_semicolon){
    stop_cond_end = init_res.next_symbol;
  }else{
    ycf_parse_result stop_cond_res = parse_expression_end_end_semicolon(init_res.next_symbol);
    if(!stop_cond_res.success || stop_cond_res.next_symbol->type != ycf_symbol_type_semicolon){
      return fail_parse_result();
    }
    stop_cond = stop_cond_res.result;
    stop_cond_end = stop_cond_res.next_symbol;
  }
  ycf_node* end_exp = NULL;
  ycf_symbol* end_exp_end = NULL;
  if(stop_cond_end->next->type == ycf_symbol_type_end_parenthesis){
    end_exp_end = stop_cond_end->next;
  }else{
    ycf_parse_result end_exp_res = parse_expression_end_end_paren(stop_cond_end->next);
    if(!end_exp_res.success || end_exp_res.next_symbol->type != ycf_symbol_type_end_parenthesis){
      return fail_parse_result();
    }
    end_exp = end_exp_res.result;
    end_exp_end = end_exp_res.next_symbol;
  }
  ycf_parse_result for_statement = parse_statement(end_exp_end->next);
  if(!for_statement.success){
    return fail_parse_result();
  }
  return success_parse_result(for_statement.next_symbol,
                              ycf_node_for_new(for_word,
                                               start_paran,
                                               init,
                                               stop_cond,
                                               stop_cond_end,
                                               end_exp,
                                               end_exp_end,
                                               for_statement.result));
}


ycf_parse_result parse_switch_statement(ycf_symbol* symbols){
  ycf_parse_result cond_statem = parse_cond_statement(symbols, ycf_symbol_type_switch);
  if(!cond_statem.success){
    return fail_parse_result();
  }
  if(cond_statem.result->u.if_n.if_statement->type != ycf_node_type_code_scope){
    return fail_parse_result();
  }
  return success_parse_result(cond_statem.next_symbol,
                              ycf_node_switch_new(cond_statem.result->u.if_n.if_word,
                                                  cond_statem.result->u.if_n.expression,
                                                  cond_statem.result->u.if_n.if_statement->u.code_scope));
}

ycf_parse_result parse_exp_statement(ycf_symbol* symbols){
  if(symbols->type == ycf_symbol_type_semicolon){
      /*Empty statement*/
      return success_parse_result(symbols->next,
                                  ycf_node_statement_new(ycf_node_expression_new(ycf_node_list_empty()),
                                                         symbols));
  }
  ycf_parse_result expression = parse_expression_end_end_semicolon(symbols);
  if(!expression.success || expression.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(expression.next_symbol->next,
                              ycf_node_statement_new(expression.result,
                                                     expression.next_symbol));
}

ycf_parse_result parse_return_statement(ycf_symbol* symbols){
  ycf_symbol* return_symbol = symbols;
  if(return_symbol->type != ycf_symbol_type_return){
    return fail_parse_result();
  }
  if(return_symbol->next->type == ycf_symbol_type_semicolon){
    return success_parse_result(return_symbol->next->next,
                                ycf_node_return_new(return_symbol,
                                                    NULL,
                                                    return_symbol->next));
  }
  ycf_parse_result expression = parse_expression_end_end_semicolon(return_symbol->next);
  if(!expression.success || expression.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(expression.next_symbol->next,
                              ycf_node_return_new(return_symbol,
                                                  expression.result,
                                                  expression.next_symbol));
}

ycf_parse_result parse_macro_command(ycf_symbol* symbols){
  if(symbols->type == ycf_symbol_type_macro_command || symbols->type == ycf_symbol_type_macro_define){
    return success_parse_result(symbols->next, ycf_node_macro_cmd_new(symbols));
  } else {
    return fail_parse_result();
  }
}

ycf_parse_result parse_scope(ycf_symbol* symbols);

ycf_parse_result parse_special_code_block(ycf_symbol* symbols, char* code_block_name, ycf_node_type type){
  ycf_symbol* start = symbols;
  if(symbols->type != ycf_symbol_type_special_code_start ||
     !ycf_symbol_is_text_eq(start,
                            ycf_string_new("/*special_code_start:%s*/", code_block_name))){
    return fail_parse_result();
  }
  ycf_parse_result code = parse_if_statement(start->next);
  if(!code.success){
    return fail_parse_result();
  }
  ycf_symbol* end = code.next_symbol;
  if(end->type != ycf_symbol_type_special_code_end){
    return fail_parse_result();
  }
  return success_parse_result(end->next,
                              ycf_node_special_code_block_new(type,
                                                              start,
                                                              code.result->u.if_n,
                                                              end));
}

ycf_parse_result parse_on_save_yield_state(ycf_symbol* symbols){
  return parse_special_code_block(symbols, "ON_SAVE_YIELD_STATE", ycf_node_type_on_save_yield_state_code);
}

ycf_parse_result parse_on_restore_yield_state(ycf_symbol* symbols){
  return parse_special_code_block(symbols, "ON_RESTORE_YIELD_STATE", ycf_node_type_on_restore_yield_state_code);
}

ycf_parse_result parse_destroy_state_code(ycf_symbol* symbols){
  ycf_parse_result res = parse_special_code_block(symbols, "ON_DESTROY_STATE", ycf_node_type_on_destroy_state_code);
  return res;
}

ycf_parse_result parse_on_return_code(ycf_symbol* symbols){
  ycf_parse_result res = parse_special_code_block(symbols, "ON_RETURN", ycf_node_type_on_return_code);
  return res;
}

ycf_parse_result parse_on_destroy_state_or_return_code(ycf_symbol* symbols){
  ycf_parse_result res = parse_special_code_block(symbols, "ON_DESTROY_STATE_OR_RETURN", ycf_node_type_on_destroy_state_or_return_code);
  return res;
}

ycf_parse_result parse_consume_reds(ycf_symbol* symbols){
  ycf_symbol* consume_reds_symbol = symbols;
  if(consume_reds_symbol->type != ycf_symbol_type_consume_reds){
    return fail_parse_result();
  }
  ycf_parse_result paran_expression =
    parse_paran_expression(consume_reds_symbol->next);
  if(!paran_expression.success){
    return fail_parse_result();
  }
  if(paran_expression.next_symbol->type != ycf_symbol_type_semicolon){
    return fail_parse_result();
  }
  return success_parse_result(paran_expression.next_symbol->next,
                              ycf_node_consume_reds_new(consume_reds_symbol,
                                                        paran_expression.result->u.parentheses_expression,
                                                        paran_expression.next_symbol));
}

ycf_parse_result parse_statement(ycf_symbol* symbols){
  int number_of_parsers = 22;
    ycf_parse_result (*parsers[])(ycf_symbol *) = {
    parse_defenition_no_init,
    parse_defenition_with_init,
    parse_if_else_statement,
    parse_if_statement,
    parse_while_statement,
    parse_do_while_statement,
    parse_for_statement,
    parse_switch_statement,
    parse_scope,
    parse_goto,
    parse_return_statement,
    parse_consume_reds,
    parse_function_call_statement,
    parse_function_call_assignment_statement,
    parse_assignment,
    parse_on_save_yield_state,
    parse_on_restore_yield_state,
    parse_on_return_code,
    parse_on_destroy_state_or_return_code,
    parse_destroy_state_code,
    parse_macro_command,
    parse_exp_statement
  };
  for(int i = 0; i < number_of_parsers; i++){
    ycf_parse_result res = parsers[i](symbols);
    if(res.success){
      return res;
    }
  }
  return fail_parse_result();
}

ycf_parse_result parse_scope(ycf_symbol* symbols){
  ycf_symbol* current = symbols;
  ycf_symbol* start;
  if(current->type != ycf_symbol_type_open_curly_brace){
    return fail_parse_result();
  }
  start = current;
  current = current->next;
  int number_of_parsers = 2;
    ycf_parse_result (*parsers[])(ycf_symbol *) = {
    parse_defenition_no_init,
    parse_defenition_with_init
  };
  parse_list_res decs = parse_list(number_of_parsers, parsers, current);
  current = decs.next_symbol;
  /* parse rest */
  number_of_parsers = 1;
  ycf_parse_result (*rest_parsers[])(ycf_symbol *) = {
    parse_statement
  };
  parse_list_res others =
    parse_list_break_symbol(number_of_parsers,
                            rest_parsers,
                            current,
                            ycf_symbol_type_end_curly_brace);
  current = others.next_symbol;
  if (current == NULL) {
    return fail_parse_result();
  }
  return success_parse_result(current->next, ycf_node_scope_new(start,
                                                                decs.list,
                                                                others.list,
                                                                current));
}

ycf_parse_result parse_function(ycf_symbol* symbols){
  ycf_parse_result fun_head = parse_function_head(symbols, ycf_symbol_type_open_curly_brace, true);
  if(!fun_head.success){
    return fail_parse_result();
  }
  ycf_parse_result scope = parse_scope(fun_head.next_symbol);
  if(!scope.success){
    return fail_parse_result();
  }
  return success_parse_result(scope.next_symbol,
                              ycf_node_function_new(fun_head.result->u.function_definition,
                                                    scope.result->u.code_scope));
}

ycf_parse_result parse_c_file(ycf_symbol* symbols){
  int number_of_parsers = 5;
    ycf_parse_result (*parsers[])(ycf_symbol *) = {
    parse_defenition_no_init,
    parse_defenition_with_init,
    parse_function_def,
    parse_function,
    parse_other
  };
  parse_list_res program_list =
    parse_list(number_of_parsers, parsers, symbols);
  if(program_list.next_symbol != NULL){
      return fail_parse_result();
  } else {
    return success_parse_result(NULL,
                                ycf_node_c_file_new(program_list.list));
  }
}


ycf_node* get_abstract_syntax_tree_root(ycf_symbol_list* symbols){
  ycf_parse_result res = parse_c_file(symbols->head);
  if(!res.success){
    printf("ERROR: Could not parse file\n");
    exit(1);
  }
  return res.result;
}

ycf_node* ycf_node_deep_copy(ycf_node *n) {
    ycf_string_printable_buffer *b = ycf_string_printable_buffer_new();
    ycf_node_print(n, b);
    ycf_symbol_list symbols = ycf_symbol_list_from_text(b->buffer);
    ycf_parse_result res;
    if (n->type == ycf_node_type_c_file) {
        res = parse_c_file(symbols.head);
    } else if (n->type == ycf_node_type_variable_definition ||
               n->type == ycf_node_type_variable_definition_init ||
               n->type == ycf_node_type_other ||
               n->type == ycf_node_type_code_scope ||
               n->type == ycf_node_type_assignment ||
               n->type == ycf_node_type_yield ||
               n->type == ycf_node_type_statement ||
               n->type == ycf_node_type_if ||
               n->type == ycf_node_type_if_else ||
               n->type == ycf_node_type_while ||
               n->type == ycf_node_type_do_while ||
               n->type == ycf_node_type_switch ||
               n->type == ycf_node_type_for ||
               n->type == ycf_node_type_assignment_function_call ||
               n->type == ycf_node_type_on_save_yield_state_code ||
               n->type == ycf_node_type_on_restore_yield_state_code ||
               n->type == ycf_node_type_on_destroy_state_code ||
               n->type == ycf_node_type_on_return_code ||
               n->type == ycf_node_type_on_destroy_state_or_return_code ||
               n->type == ycf_node_type_goto ||
               n->type == ycf_node_type_return_statement ||
               n->type == ycf_node_type_consume_reds) {
        res = parse_statement(symbols.head);
    } else if (n->type == ycf_node_type_function_declaration) {
        res = parse_function_def(symbols.head);
    } else if (n->type == ycf_node_type_function_definition) {
        res = parse_function(symbols.head);
    } else if (n->type == ycf_node_type_function_call) {
        res = parse_function_call(symbols.head);
    } else if (n->type == ycf_node_type_expression) {
        res = parse_expression(symbols.head);
    } else if (n->type == ycf_node_type_parentheses_expression) {
        res = parse_paran_expression(symbols.head);
    } else if (n->type == ycf_node_type_comma) {
        res = parse_comma(symbols.head);
    } else if (n->type == ycf_node_type_array_bracket) {
        res = parse_array_bracket(symbols.head);
    } else if (n->type == ycf_node_type_macro_cmd) {
        res = parse_macro_command(symbols.head);
    } else if (n->type == ycf_node_type_period_field_access) {
        res = parse_period_field_access_node(symbols.head);
    } else if (n->type == ycf_node_type_pointer_field_access) {
        res = parse_pointer_field_access_node(symbols.head);
    } else {
        fprintf(stderr, "Unknown node type %d\n", n->type);
        exit(1);
    }
    if(!res.success) {
        fprintf(stderr, "An error has been detected in the function ycf_node_deep_copy\n");
        exit(1);
    }
    return res.result;
}

