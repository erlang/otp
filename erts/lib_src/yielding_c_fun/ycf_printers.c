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


#include <stdio.h>
#include <stdlib.h>

#include "ycf_yield_fun.h"
#include "ycf_node.h"

void print_node_code_expression(ycf_node_expression e, ycf_string_printable_buffer* b);

void print_symbol_code(ycf_symbol* s, ycf_string_printable_buffer* b){
  if(s == NULL){
    return;
  }
  print_symbol_code(s->whitespace_or_comment_before, b);
    ycf_string_printable_buffer_printf(b, "%s", ycf_symbol_get_text(s));
}

void print_symbol_list(ycf_symbol_list* l, ycf_string_printable_buffer* b){
  ycf_symbol* s = l->head;
  while(s != NULL){
    print_symbol_code(s, b);
    s = s->next;
  }
}

void print_node_code_array_bracket(ycf_node_array_bracket n, ycf_string_printable_buffer* b){
  print_symbol_code(n.start, b);
  if(!n.empty){
    print_node_code_expression(n.content, b);
  }
  print_symbol_code(n.end, b);
}

void print_node_code_definition_custom_end(ycf_node_definition d, bool dyn_array_to_ptr, ycf_symbol* end, ycf_string_printable_buffer* b){
  int nr_of_empty_brackets = 0;
  {
    ycf_node* current = d.array_brackets.head;
    while(current != NULL){
      if(current->u.array_bracket.empty){
        nr_of_empty_brackets++;
      }
      current = current->next;
    }
  }
  print_symbol_list(&d.type_specifiers, b);
  if(dyn_array_to_ptr){
    for(int i = 0; i < nr_of_empty_brackets; i++){
        ycf_string_printable_buffer_printf(b, "*");
    }
  }
  print_symbol_code(d.identifier, b);
  if (!dyn_array_to_ptr || nr_of_empty_brackets == 0){
    print_node_list_code(d.array_brackets.head, b);
  }
  print_symbol_code(end, b);
}

void print_node_code_definition(ycf_node_definition d, ycf_string_printable_buffer* b){
  print_node_code_definition_custom_end(d, false, d.end, b);
}

void print_node_code_definition_init(ycf_node_definition_init d, ycf_string_printable_buffer* b){
  print_node_code_definition(d.definition, b);
  print_symbol_list(&d.initializer_expression, b);
  print_symbol_code(d.end, b);
}

void print_node_list_code(ycf_node* n, ycf_string_printable_buffer* b){
   while(n != NULL){
       ycf_node_print(n, b);
     n = n->next;
   }
}

void print_node_code_function_def_parameters(ycf_node_function_definition f_def, ycf_string_printable_buffer* b){
  if(f_def.ignore_param_ending){
    ycf_node* n = f_def.parameters.head;
    while(n != NULL){
      if(n->next == NULL){
        print_node_code_definition_custom_end(n->u.definition, false, ycf_symbol_new_parenthesis(), b);
      }else{
        print_node_code_definition_custom_end(n->u.definition, false, ycf_symbol_new_comma(), b);
      }
      n = n->next;
    }
    if(f_def.end.head != NULL && f_def.end.head->type == ycf_symbol_type_end_parenthesis){
      ycf_symbol_list end = f_def.end;
      end.head = end.head->next;
      print_symbol_list(&end, b);
    }else if(f_def.end.head != NULL && f_def.end.head->type != ycf_symbol_type_void){
      print_symbol_list(&f_def.end, b);
    }
  }else {
    print_node_list_code(f_def.parameters.head, b);
    print_symbol_list(&f_def.end, b);
  }
}
void print_node_code_function_def(ycf_node_function_definition f_def, ycf_string_printable_buffer* b){
  print_node_code_definition(f_def.definition, b);
  print_node_code_function_def_parameters(f_def, b);
}

void print_node_code_scope(ycf_node_code_scope s, ycf_string_printable_buffer* b){
  print_symbol_code(s.start, b);
  print_node_list_code(s.definition_nodes.head, b);
  print_node_list_code(s.other_nodes.head, b);
  print_symbol_code(s.end, b);
}

void print_node_code_function(ycf_node_function f, ycf_string_printable_buffer* b){
  print_node_code_function_def(f.definition, b);
  print_node_code_scope(f.body, b);
}

void print_node_code_assignment(ycf_node_assignment a, ycf_string_printable_buffer* b){
  print_node_code_expression(a.left_side, b);
  print_symbol_code(a.assignment_symbol, b);
  print_node_code_expression(a.right_side, b);
  print_symbol_code(a.end, b);
}

void print_node_code_gen_struct(ycf_node_gen_typedef_struct n, ycf_string_printable_buffer* b){
    ycf_string_printable_buffer_printf(b, "\n\n\nstruct %s {", n.name);
  ycf_node* current = n.definition_nodes.head;
  while(current != NULL){
    print_node_code_definition_custom_end(current->u.definition, true, ycf_symbol_new_semicolon(), b);
    current = current->next;
  }
    ycf_string_printable_buffer_printf(b, "\n};\n");
}

void print_node_code_yield(ycf_node_yield n, ycf_string_printable_buffer* b){
  print_symbol_code(n.yield_symbol,b);
  print_symbol_code(n.end_symbol,b);

}

void print_node_code_function_call(ycf_node_function_call n, ycf_string_printable_buffer* b){
  print_symbol_list(&n.neg_symbols, b);
  print_symbol_code(n.identifier, b);
  print_symbol_code(n.start_symbol, b);
  print_node_list_code(n.parameter_expressions.head, b);
  print_symbol_code(n.end_symbol, b);
}

void print_node_code_expression(ycf_node_expression e, ycf_string_printable_buffer* b){
  print_node_list_code(e.content.head, b);
}


void print_node_code_paran_expression(ycf_node_parentheses_expression e, ycf_string_printable_buffer* b){
  print_symbol_code(e.start_symbol, b);
  print_node_code_expression(e.content, b);
  print_symbol_code(e.end_symbol, b);
}

void print_node_code_consume_reds(ycf_node_consume_reds e, ycf_string_printable_buffer* b){
  print_symbol_code(e.consume_reds_symbol, b);
  print_node_code_paran_expression(e.nr_of_reds_expression, b);
  print_symbol_code(e.end_symbol, b);
}

void print_node_code_if_statement(ycf_node_if e, ycf_string_printable_buffer* b){
  print_symbol_code(e.if_word, b);
  print_node_code_paran_expression(e.expression, b);
    ycf_node_print(e.if_statement, b);
}

void print_node_code_while_statement(ycf_node_while e, ycf_string_printable_buffer* b){
  print_symbol_code(e.while_word, b);
  print_node_code_paran_expression(e.expression, b);
    ycf_node_print(e.statement, b);
}

void print_node_code_do_while_statement(ycf_node_do_while e, ycf_string_printable_buffer* b){
  print_symbol_code(e.do_word, b);
    ycf_node_print(e.statement, b);
  print_symbol_code(e.while_word, b);
  print_node_code_paran_expression(e.expression, b);
  print_symbol_code(e.end, b);
}

void print_node_code_switch_statement(ycf_node_switch e, ycf_string_printable_buffer* b){
  print_symbol_code(e.switch_word, b);
  print_node_code_paran_expression(e.expression, b);
  print_node_code_scope(e.scope, b);
}

void print_node_code_for_statement(ycf_node_for e, ycf_string_printable_buffer* b){
  print_symbol_code(e.for_word, b);
  print_symbol_code(e.start_parentheses, b);
    ycf_node_print(e.init, b);
    ycf_node_print(e.stop_cond, b);
  print_symbol_code(e.stop_cond_end, b);
    ycf_node_print(e.end_exp, b);
  print_symbol_code(e.end_parentheses, b);
    ycf_node_print(e.statement, b);
}

void print_node_code_if_else_statement(ycf_node_if_else e, ycf_string_printable_buffer* b){
  print_node_code_if_statement(e.if_part, b);
  print_symbol_code(e.else_word, b);
    ycf_node_print(e.else_statement, b);
}

void print_node_code_for_assignment_fun_call(ycf_node_function_call_assignment e, ycf_string_printable_buffer* b){
  print_node_code_expression(e.left_side, b);
  print_symbol_code(e.assignment_symbol, b);
  print_node_code_function_call(e.fun_call, b);
}

void print_node_code_special_code_block(ycf_node_special_code_block block, ycf_string_printable_buffer* b){
  print_symbol_code(block.start, b);
  print_node_code_if_statement(block.code, b);
  print_symbol_code(block.end, b);

}

void print_node_code_goto(ycf_node_goto n, ycf_string_printable_buffer* b){
  print_symbol_code(n.goto_symbol, b);
  print_symbol_code(n.label_symbol, b);
  print_symbol_code(n.end_symbol, b);
}

void print_node_code_period_field_access(ycf_node_period_field_access n, ycf_string_printable_buffer* b){
  print_symbol_code(n.period, b);
  print_symbol_code(n.field_name, b);
}

void print_node_code_pointer_field_access(ycf_pointer_field_access n, ycf_string_printable_buffer* b){
  print_symbol_code(n.pointer, b);
  print_symbol_code(n.field_name, b);
}

void print_node_code_return(ycf_node_return n, ycf_string_printable_buffer* b){
  print_symbol_code(n.return_symbol, b);
  if (n.return_expression != NULL) {
      ycf_node_print(n.return_expression, b);
  }
  print_symbol_code(n.end_symbol, b);
}

void ycf_node_print(ycf_node* node, ycf_string_printable_buffer* b){
  if(node == NULL){
    return;
  } else if(node->type == ycf_node_type_c_file){
    print_node_list_code(node->u.c_file.content.head, b);
  } else if(node->type == ycf_node_type_variable_definition){
    print_node_code_definition(node->u.definition, b);
  } else if(node->type == ycf_node_type_variable_definition_init){
    print_node_code_definition_init(node->u.definition_init, b);
  } else if(node->type == ycf_node_type_function_declaration){
    print_node_code_function_def(node->u.function_definition, b);
  } else if(node->type == ycf_node_type_other){
    print_symbol_code(node->u.other.what, b);
  } else if(node->type == ycf_node_type_code_scope){
    print_node_code_scope(node->u.code_scope, b);
  }else if(node->type == ycf_node_type_function_definition){
    print_node_code_function(node->u.function, b);
  }else if(node->type == ycf_node_type_assignment){
    print_node_code_assignment(node->u.a, b);
  }else if(node->type == ycf_node_type_gen_typedef_struct){
    print_node_code_gen_struct(node->u.gen_typedef_struct, b);
  }else if(node->type == ycf_node_type_yield){
    print_node_code_yield(node->u.yield, b);
  } else if(node->type == ycf_node_type_statement){
      ycf_node_print(node->u.statement.expression, b);
    print_symbol_code(node->u.statement.end_symbol, b);
  } else if(node->type == ycf_node_type_function_call){
    print_node_code_function_call(node->u.function_call, b);
  } else if(node->type == ycf_node_type_expression){
    print_node_code_expression(node->u.expression, b);
  } else if(node->type == ycf_node_type_parentheses_expression){
    print_node_code_paran_expression(node->u.parentheses_expression, b);
  } else if(node->type == ycf_node_type_if){
    print_node_code_if_statement(node->u.if_n, b);
  }else if(node->type == ycf_node_type_if_else){
    print_node_code_if_else_statement(node->u.if_else, b);
  }else if(node->type == ycf_node_type_while){
    print_node_code_while_statement(node->u.while_n, b);
  }else if(node->type == ycf_node_type_do_while){
    print_node_code_do_while_statement(node->u.do_while, b);
  }else if(node->type == ycf_node_type_switch){
    print_node_code_switch_statement(node->u.switch_n, b);
  }else if(node->type == ycf_node_type_for){
    print_node_code_for_statement(node->u.for_n, b);
  }else if(node->type == ycf_node_type_assignment_function_call){
    print_node_code_for_assignment_fun_call(node->u.function_call_assignment, b);
  } else if(node->type == ycf_node_type_comma){
    print_symbol_code(node->u.comma.comma_symbol, b);
  } else if(node->type == ycf_node_type_array_bracket){
    print_node_code_array_bracket(node->u.array_bracket, b);
  } else if(node->type == ycf_node_type_macro_cmd){
    print_symbol_code(node->u.macro_cmd.symbol, b);
  } else if(node->type == ycf_node_type_on_save_yield_state_code ||
            node->type == ycf_node_type_on_restore_yield_state_code ||
            node->type == ycf_node_type_on_destroy_state_code ||
            node->type == ycf_node_type_on_return_code ||
            node->type == ycf_node_type_on_destroy_state_or_return_code){
    print_node_code_special_code_block(node->u.special_code_block, b);
  } else if(node->type == ycf_node_type_consume_reds){
    print_node_code_consume_reds(node->u.consume_reds, b);
  } else if(node->type == ycf_node_type_goto){
    print_node_code_goto(node->u.goto_n, b);
  } else if(node->type == ycf_node_type_return_statement){
    print_node_code_return(node->u.return_n, b);
  } else if(node->type == ycf_node_type_period_field_access){
    print_node_code_period_field_access(node->u.period_field_access, b);
  } else if(node->type == ycf_node_type_pointer_field_access){
    print_node_code_pointer_field_access(node->u.pointer_field_access, b);
  } else {
    fprintf(stderr, "Unknown node type %d\n", node->type);
    exit(1);
  }
}


void print_definition(ycf_node_definition d){
    printf("NODE: definition (type=%s,%s=%s)\n",
           ycf_symbol_text_between(d.type_specifiers.head,
                                   d.type_specifiers.last),
           get_symbol_type_text(d.identifier->type),
           ycf_symbol_get_text(d.identifier) );
}

void print_scope(ycf_node_code_scope node){
  printf("NODE: scope\n");
  printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
  printf("Defenition Nodes:\n");
  struct ycf_node* n = node.definition_nodes.head;
  while(n != NULL){
    print_abstract_syntax_tree(n);
    n = n->next;
  }
  printf("Other Nodes:\n");
  n = node.other_nodes.head;
  while(n != NULL){
    print_abstract_syntax_tree(n);
    n = n->next;
  }
  printf("END SCOPE\n");
  printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
}

void print_function_def(ycf_node_function_definition node){
    printf("NODE: function def:\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    printf("definition:\n");
    print_definition(node.definition);
    printf("parameters:\n");
    struct ycf_node* n = node.parameters.head;
    while(n != NULL){
      print_abstract_syntax_tree(n);
      n = n->next;
    }
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
}

void print_fun_call(ycf_node_function_call f_call){
    printf("NODE: fun_call %s parameters:\n", ycf_symbol_get_text(f_call.identifier));
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    struct ycf_node* current = f_call.parameter_expressions.head;
    int i = 1;
    while(current != NULL){
      printf("param %d: \n", i);
        ycf_node_print(current, NULL);
      current = current->next;
      i++;
    }
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
}

void print_abstract_syntax_tree(ycf_node* node){
  printf("%p\n",(void*)node);
  if(node == NULL){
    return;
  } else if(node->type == ycf_node_type_c_file){
    printf("NODE: c_file\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    struct ycf_node* n = node->u.c_file.content.head;
    while(n != NULL){
      print_abstract_syntax_tree(n);
      n = n->next;
    }
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  } else if(node->type == ycf_node_type_variable_definition){
    print_definition(node->u.definition);
  } else if(node->type == ycf_node_type_variable_definition_init){
    printf("NODE: definition_init (type=%s,%s=%s,init=%s)\n",
           ycf_symbol_text_between(node->u.definition_init.definition.type_specifiers.head,
                                   node->u.definition_init.definition.type_specifiers.last),
           get_symbol_type_text(node->u.definition_init.definition.identifier->type),
           ycf_symbol_get_text(node->u.definition_init.definition.identifier),
           ycf_symbol_text_between(node->u.definition_init.initializer_expression.head,
                                   node->u.definition_init.initializer_expression.last));
  } else if(node->type == ycf_node_type_function_declaration){
    print_function_def(node->u.function_definition);
  } else if(node->type == ycf_node_type_other){
    printf("NODE: other (%s)\n", get_symbol_type_text(node->u.other.what->type));
  } else if(node->type == ycf_node_type_code_scope){
    print_scope(node->u.code_scope);
  }else if(node->type == ycf_node_type_function_definition){
    printf("NODE: function\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    print_function_def(node->u.function.definition);
    print_scope(node->u.function.body);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_assignment){
    printf("NODE: assignment\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    print_node_code_assignment(node->u.a, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_yield){
    printf("NODE: yield\n");
  }else if(node->type == ycf_node_type_statement){
    printf("NODE: statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    print_abstract_syntax_tree(node->u.statement.expression);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_if){
    printf("NODE: if_statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_if_else){
    printf("NODE: if_else_statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_while){
    printf("NODE: while_statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_do_while){
    printf("NODE: do_while_statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_for){
    printf("NODE: for_statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }else if(node->type == ycf_node_type_switch){
    printf("NODE: switch_statement\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  } else if(node->type == ycf_node_type_function_call) {
    print_fun_call(node->u.function_call);
  } else if(node->type == ycf_node_type_assignment_function_call) {
    printf("NODE: assignment fun call\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
    printf("Assignment expression:\n");
    print_node_code_expression(node->u.function_call_assignment.left_side, NULL);
    print_fun_call(node->u.function_call_assignment.fun_call);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");


  } else {
    printf("NODE: OTHER???\n");
    printf(">>>>>>>>>>>>>>>>>>>>>>>\n");
      ycf_node_print(node, NULL);
    printf("<<<<<<<<<<<<<<<<<<<<<<<\n");
  }
  
}

