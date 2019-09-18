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


#include "lib/tiny_regex_c/re.h"
#include "ycf_yield_fun.h"
#include "ycf_utils.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>



int ycf_symbol_is_text_eq(ycf_symbol* symbol, char* str){
  unsigned long symbol_length = symbol->stop - symbol->start;
  return
    symbol_length == strlen(str) &&
    strncmp(str, &symbol->source[symbol->start], symbol_length) == 0;
}

char* ycf_symbol_text_between(ycf_symbol* s1, ycf_symbol* s2){
  int size = s2->stop - s1->start;
  char* str = ycf_malloc(size+1);
  strncpy(str, &s1->source[s1->start], size);
  str[size] = 0;
  return str;
}

char* get_symbol_type_text(ycf_symbol_type type){
  switch(type) {
  case ycf_symbol_type_comment: return "ycf_symbol_type_comment";
  case ycf_symbol_type_string_literal: return "ycf_symbol_type_string_literal";
  case ycf_symbol_type_macro_define: return "ycf_symbol_type_macro_define";
  case ycf_symbol_type_macro_command: return "ycf_symbol_type_macro_command";
  case ycf_symbol_type_whitespace: return "ycf_symbol_type_whitespace";
  case ycf_symbol_type_identifier: return "ycf_symbol_type_identifier";
  case ycf_symbol_type_number: return "ycf_symbol_type_number";
  case ycf_symbol_type_star: return "ycf_symbol_type_star";
  case ycf_symbol_type_neg: return "ycf_symbol_type_neg";
  case ycf_symbol_type_equal_equal_sign: return "ycf_symbol_type_equal_equal_sign";
  case ycf_symbol_type_not_equal_sign: return "ycf_symbol_type_not_equal_sign";
  case ycf_symbol_type_open_parenthesis: return "ycf_symbol_type_open_parenthesis";
  case ycf_symbol_type_end_parenthesis: return "ycf_symbol_type_end_parenthesis";
  case ycf_symbol_type_open_curly_brace: return "ycf_symbol_type_open_curly_brace";
  case ycf_symbol_type_end_curly_brace: return "ycf_symbol_type_end_curly_brace";
  case ycf_symbol_type_open_square_bracket: return "ycf_symbol_type_open_square_bracket";
  case ycf_symbol_type_end_square_bracket: return "ycf_symbol_type_end_square_bracket";
  case ycf_symbol_type_equal_sign: return "ycf_symbol_type_equal_sign";
  case ycf_symbol_type_semicolon: return "ycf_symbol_type_semicolon";
  case ycf_symbol_type_comma: return "ycf_symbol_type_comma";
  case ycf_symbol_type_consume_reds: return "ycf_symbol_type_consume_reds";
  case ycf_symbol_type_pointer_field_access: return "ycf_symbol_type_pointer_field_access";
  case ycf_symbol_type_period: return "ycf_symbol_type_period";
  case ycf_symbol_type_const: return "ycf_symbol_type_const";
  case ycf_symbol_type_void: return "ycf_symbol_type_void";
  case ycf_symbol_type_volatile: return "ycf_symbol_type_volatile";
  case ycf_symbol_type_static: return "ycf_symbol_type_static";
  case ycf_symbol_type_inline: return "ycf_symbol_type_inline";
  case ycf_symbol_type_return: return "ycf_symbol_type_return";
  case ycf_symbol_type_if: return "ycf_symbol_type_if";
  case ycf_symbol_type_else: return "ycf_symbol_type_else";
  case ycf_symbol_type_goto: return "ycf_symbol_type_goto";
  case ycf_symbol_type_break: return "ycf_symbol_type_break";
  case ycf_symbol_type_while: return "ycf_symbol_type_while";
  case ycf_symbol_type_do: return "ycf_symbol_type_do";
  case ycf_symbol_type_for: return "ycf_symbol_type_for";
  case ycf_symbol_type_switch: return "ycf_symbol_type_switch";
  case ycf_symbol_type_continue: return "ycf_symbol_type_continue";
  case ycf_symbol_type_something_else: return "ycf_symbol_type_something_else";
  case ycf_symbol_type_special_code_start: return "ycf_symbol_type_special_code_start";
  case ycf_symbol_type_special_code_end: return "ycf_symbol_type_special_code_end";
  }
  return "non_existing_symbol?";
}

typedef struct symbol_finder {
  int (*finder)(struct symbol_finder*,char*);
  ycf_symbol_type type;
  int length;
  char *str_1;
  char *str_2;
} symbol_finder;

int starts_with(char *str, char *prefix)
{
  return strncmp(str, prefix, strlen(prefix)) == 0;
}

int until_no_match(symbol_finder* f, char* text){
  int pos = 0;
  while(re_match(f->str_1, &(text[pos])) == 0){
    pos++;
  }
  return pos;
}

int string_litteral_finder(symbol_finder* f, char* text){
  int pos = 0;
  if (starts_with(text, "\"")){
    pos++;
    //\"(\\.|[^"\\])*\"
    while(re_match("\\.", &(text[pos])) == 0 ||
          re_match("[^\"]", &(text[pos])) == 0){
      pos++;
    }
    if(starts_with(&(text[pos]), "\"")){
      return pos + 1;
    }else {
      printf("Broken string litteral\n");
      exit(1);
    }
  }
  return pos;
}

int macro_define_finder(symbol_finder* f, char* text){
  int pos = 0;
  if (starts_with(text, "#define")){
    pos = pos + strlen("#define");
    while(1){
      if(starts_with(&(text[pos]), "\\\n")){
        pos = pos + 2;
      } else if (starts_with(&(text[pos]), "\n")){
        break;
      } else {
        pos++;
      }
    }
  }
  return pos;
}


int starts_with_until_no_match(symbol_finder* f, char* text){
  int pos = 0;
  if(re_match(f->str_1, text) == 0){
    while(re_match(f->str_2, &(text[pos])) == 0){
      pos++;
    }
  }
  return pos;
}

int starts_with_ends_with(symbol_finder* f, char* text){
  if(starts_with(text, f->str_1)){
    int pos = 1;
    while(!starts_with(&(text[pos]), f->str_2)){
      pos++;
    }
    return pos+strlen(f->str_2);
  }
  return 0;
}

int fixed_string(symbol_finder* f, char* text){
  if(starts_with(text, f->str_1)){
    return strlen(f->str_1);
  }
  return 0;
}

int fixed_alpha_string(symbol_finder* f, char* text){
  if(starts_with(text, f->str_1) &&
     re_match("[^\\W]", &text[strlen(f->str_1)])){
    return strlen(f->str_1);
  }
  return 0;
}

int regex_char(symbol_finder* f, char* text){
  if(re_match(f->str_1, text) == 0){
    return 1;
  }
  return 0;
}

void fold_whitespace_and_comments(ycf_symbol_list* symbols){
  ycf_symbol* prev = NULL;
  ycf_symbol* current = symbols->head;
  ycf_symbol* dummy = ycf_malloc(sizeof(ycf_symbol));
  while(current != NULL){
    current->whitespace_or_comment_before = NULL;
    if(prev != NULL && (prev->type == ycf_symbol_type_whitespace ||
                        prev->type == ycf_symbol_type_comment)){
      current->whitespace_or_comment_before = prev;
    }
    prev = current;
    current = current->next;
  }
  // remove ycf_symbol_type_whitespace and comments from list
  dummy->type = ycf_symbol_type_void;
  dummy->next = symbols->head;
  prev = dummy;
  current = prev->next;
  while(current != NULL &&
        current != symbols->last){
    if(current->type == ycf_symbol_type_whitespace ||
       current->type == ycf_symbol_type_comment){
      prev->next = current->next;
      current = current->next;
    }else {
      prev = current;
      current = current->next;
    }
  }
  symbols->head = dummy->next;
}

ycf_symbol_list ycf_symbol_list_from_text(char* text){
  int pos = 0;
  int nr_of_finders = 41;
  int i;
  ycf_symbol_list ret = ycf_symbol_list_empty();
  symbol_finder symbol_finders[] =
    {
      {
        .type = ycf_symbol_type_special_code_start,
        .str_1 = "/*special_code_start:",
        .str_2 = "*/",
        .finder = starts_with_ends_with
      },
      {
        .type = ycf_symbol_type_special_code_end,
        .str_1 = "/*special_code_end*/",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_comment,
        .str_1 = "/*",
        .str_2 = "*/",
        .finder = starts_with_ends_with
      },
      {
        .type = ycf_symbol_type_string_literal,
        .finder = string_litteral_finder
      },
      {
        .type = ycf_symbol_type_macro_define,
        .finder = macro_define_finder
      },
      {
        .type = ycf_symbol_type_macro_command,
        .str_1 = "#",
        .str_2 = "\n",
        .finder = starts_with_ends_with
      },
      {
        .type = ycf_symbol_type_whitespace,
        .str_1 = "\\s",
        .finder = until_no_match
      },
      {
        .type = ycf_symbol_type_void,
        .str_1 = "void",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_static,
        .str_1 = "static",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_inline,
        .str_1 = "inline",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_const,
        .str_1 = "const",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_volatile,
        .str_1 = "volatile",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_consume_reds,
        .str_1 = "YCF_CONSUME_REDS",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_return,
        .str_1 = "return",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_if,
        .str_1 = "if",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_else,
        .str_1 = "else",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_goto,
        .str_1 = "goto",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_break,
        .str_1 = "break",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_continue,
        .str_1 = "continue",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_while,
        .str_1 = "while",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_do,
        .str_1 = "do",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_for,
        .str_1 = "for",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_switch,
        .str_1 = "switch",
        .finder = fixed_alpha_string
      },
      {
        .type = ycf_symbol_type_identifier,
        .str_1 = "[a-zA-Z]",
        .str_2 = "\\w",
        .finder = starts_with_until_no_match
      },
      {
        .type = ycf_symbol_type_number,
        .str_1 = "\\d",
        .finder = until_no_match
      },
      {
        .type = ycf_symbol_type_open_parenthesis,
        .str_1 = "(",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_end_parenthesis,
        .str_1 = ")",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_open_curly_brace,
        .str_1 = "{",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_end_curly_brace,
        .str_1 = "}",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_open_square_bracket,
        .str_1 = "[",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_end_square_bracket,
        .str_1 = "]",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_equal_sign,
        .str_1 = "=",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_not_equal_sign,
        .str_1 = "!=",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_equal_sign,
        .str_1 = "==",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_star,
        .str_1 = "*",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_neg,
        .str_1 = "!",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_semicolon,
        .str_1 = ";",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_comma,
        .str_1 = ",",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_period,
        .str_1 = ".",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_pointer_field_access,
        .str_1 = "->",
        .finder = fixed_string
      },
      {
        .type = ycf_symbol_type_something_else,
        .str_1 = ".",
        .finder = regex_char
      }
    };
  while(text[pos] != 0){
    int last_pos = pos;
    for(i = 0; i < nr_of_finders; i++) {
      symbol_finder f = symbol_finders[i];
      int stop = f.finder(&f, &text[pos]);
      if(stop){
        ycf_symbol* s = ycf_malloc(sizeof(ycf_symbol));
        s->type = f.type;
        s->source = text;
        s->start = pos;
        s->stop = pos + stop;
        s->next = NULL;
        ycf_symbol_list_append(&ret, s);
        pos = s->stop;
        break;
      }
    }
    if (last_pos == pos){
      printf("Lexer: NOTHING MATCH Stuck at: \n%s\n", &text[pos]);
      exit(1);
    }
  }
  fold_whitespace_and_comments(&ret);
  return ret;
}

void ycf_symbol_list_print(char* text){
  ycf_symbol_list symbols = ycf_symbol_list_from_text(text);
  ycf_symbol* s = symbols.head;
  while(s != NULL){
    printf("TYPE %s, START=%d, STOP=%d\n",
           get_symbol_type_text(s->type),
           s->start,
           s->stop);
    s = s->next;
  }
  printf("||||| END OF SYMBOLS\n");
}
