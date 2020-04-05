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


#include "ycf_utils.h"
#include "ycf_yield_fun.h"
#include "ycf_node.h"
#include "lib/simple_c_gc/simple_c_gc.h"

#include <stdlib.h>
#include <stdio.h>


char* file_to_str(const char* filename) {
  char * buffer;
  long length;
  FILE * f = fopen (filename, "rb");

  if (f) {
    fseek (f, 0, SEEK_END);
    length = ftell (f);
    fseek (f, 0, SEEK_SET);
    buffer = ycf_malloc (length+1);
    size_t nr_of_read_bytes =
      fread (buffer, 1, length, f);
    if (nr_of_read_bytes != length) {
      printf("error: while reading file %s\n", filename);
      exit (1);
    }
    fclose (f);
    buffer[length] = 0;
  } else {
    printf("error: could not open file %s\n", filename);
    exit(1);
  }

  return buffer;
}

void str_to_file(const char *filepath, const char *data)
{
    FILE *fp = fopen(filepath, "w");
    if (fp != NULL)
    {
        fputs(data, fp);
        fclose(fp);
    }
}

void parse_and_repeat_src_from_ast(const char* file_name){
  char* content = file_to_str(file_name);
  ycf_node* tree = ycf_node_from_string(content);
    ycf_node_print(tree, NULL);
}

void parse_and_print_symbols(const char* file_name){
  char* content = file_to_str(file_name);
    ycf_symbol_list_print(content);
}

void parse_and_print_ast(const char* file_name){
  char* content = file_to_str(file_name);
  ycf_node* tree = ycf_node_from_string(content);
  print_abstract_syntax_tree(tree);
}

void print_help_text_and_exit(char* program_name, int error_code) {
  printf("Usage: %s [-h]\n"
         "       %s [-use_gc [-print_gc_info]]\n"
         "                              [-log_max_mem_usage log_file]\n"
         "                              [(( -f | -frec | -fnoauto ) function_name)...\n"
         "                               [-output_file_name output_file]\n"
         "                               [-header_file_name header_file]\n"
         "                               [-debug]\n"
         "                               [-only_yielding_funs]\n"
         "                               [-static_aux_funs]\n"
         "                               input_c_file]]\n"
         "\n"
         "Please see the README.md file for more details.\n",
         program_name,
         program_name);
  exit(0);
}

int ycf_main( int argc, char* argv[] )
{
  int i = 1;
  for(i = 1; i < argc; i++ ){
    bool repeat = ycf_string_is_equal("-repeat", argv[i]);
    bool print_symbols = ycf_string_is_equal("-print_symbols", argv[i]);
    bool print_ast = ycf_string_is_equal("-print_ast", argv[i]);;
    bool yield = ycf_string_is_equal("-yield", argv[i]);
    if(repeat || print_symbols || print_ast || yield){
      i++;
      if(i >= argc){
        printf("ERROR: Expected at least one more argument\n\n");
        print_help_text_and_exit(argv[0], 1);
      }
    }
    if (print_ast) {
      parse_and_print_ast(argv[i]);
    } else if (repeat) {
      parse_and_repeat_src_from_ast(argv[i]);
    } else if (print_symbols) {
      parse_and_print_symbols(argv[i]);
    } else /* Default is yield */ {
      ycf_string_item_list funs_to_yield = ycf_string_item_list_empty();
      ycf_string_item_list funs_to_yield_no_auto = ycf_string_item_list_empty();
      ycf_string_item_list funs_to_yield_frec = ycf_string_item_list_empty();
      ycf_string_item_list all_yield_funs = ycf_string_item_list_empty();
      char* header_file_name = NULL;
      char* output_file_name = NULL;
      bool debug = false;
      bool only_yielding_funs = false;
      bool static_aux_funs = false;
      while(i < argc && (ycf_string_is_equal(argv[i], "-f") ||
                         ycf_string_is_equal(argv[i], "-frec") ||
                         ycf_string_is_equal(argv[i], "-fnoauto") ||
                         ycf_string_is_equal(argv[i], "-header_file_name") ||
                         ycf_string_is_equal(argv[i], "-output_file_name") ||
                         ycf_string_is_equal(argv[i], "-debug") ||
                         ycf_string_is_equal(argv[i], "-only_yielding_funs") ||
                         ycf_string_is_equal(argv[i], "-static_aux_funs"))){
        bool frec = ycf_string_is_equal(argv[i], "-frec");
        bool noauto = ycf_string_is_equal(argv[i], "-fnoauto");
        bool header = ycf_string_is_equal(argv[i], "-header_file_name");
        bool output = ycf_string_is_equal(argv[i], "-output_file_name");
        i++;
        if(ycf_string_is_equal(argv[i-1], "-debug")){
          debug = true;
          continue;
        }
        if(ycf_string_is_equal(argv[i-1], "-only_yielding_funs")){
          only_yielding_funs = true;
          continue;
        }
        if(ycf_string_is_equal(argv[i-1], "-static_aux_funs")){
          static_aux_funs = true;
          continue;
        }
        if(i >= argc){
          fprintf(stderr, "ERROR: Expected a name after %s\n\n", argv[i-1]);
          print_help_text_and_exit(argv[0], 1);
        }
        if(header){
          if(header_file_name != NULL){
            fprintf(stderr, "ERROR: Can only print a single header file\n\n");
            print_help_text_and_exit(argv[0], 1);
          }
          header_file_name = (char*)argv[i];
        } else if(output){
          if(output_file_name != NULL){
            fprintf(stderr, "ERROR: Can only  print a single output file\n\n");
            print_help_text_and_exit(argv[0], 1);
          }
          output_file_name = (char*)argv[i];
        } else if(frec){
            ycf_string_item_list_append(&funs_to_yield_frec, ycf_string_item_new((char *) argv[i]));
        } else if(noauto){
            ycf_string_item_list_append(&funs_to_yield_no_auto, ycf_string_item_new((char *) argv[i]));
        } else{
            ycf_string_item_list_append(&funs_to_yield, ycf_string_item_new((char *) argv[i]));
        }
        i++;
      }
      ycf_string_item_list funs_to_yield_copy = ycf_string_item_list_shallow_copy(funs_to_yield);
      ycf_string_item_list funs_to_yield_no_auto_copy = ycf_string_item_list_shallow_copy(funs_to_yield_no_auto);
      ycf_string_item_list funs_to_yield_frec_copy = ycf_string_item_list_shallow_copy(funs_to_yield_frec);
      ycf_string_item_list_concat(&all_yield_funs, &funs_to_yield_copy);
      ycf_string_item_list_concat(&all_yield_funs, &funs_to_yield_no_auto_copy);
      ycf_string_item_list_concat(&all_yield_funs, &funs_to_yield_frec_copy);
      if(funs_to_yield.head == NULL && funs_to_yield_no_auto.head == NULL && funs_to_yield_frec.head == NULL){
        fprintf(stderr, "ERROR: Expected at least one \"(-f|-frec|-fnoauto) function_name\" argument\n\n");
        print_help_text_and_exit(argv[0], 1);
      }
      if(i >= argc){
        fprintf(stderr, "ERROR: Expected an input file name as the last parameter\n\n");
        print_help_text_and_exit(argv[0], 1);
      }
      {
        char* file_name = (char*)argv[i];
        char* src = file_to_str(file_name);
        ycf_string_item* current_fun = funs_to_yield.head;
        ycf_node* header_file = ycf_node_from_string("");
        ycf_node* tree = ycf_node_from_string(src);
        ycf_node* only_yielding_funs_tree = NULL;
        while(current_fun != NULL){
          tree = ast_get_ast_with_yieldified_function(tree,
                                                      header_file,
                                                      current_fun->str,
                                                      &all_yield_funs,
                                                      true,
                                                      false,
                                                      debug,
                                                      only_yielding_funs,
                                                      &only_yielding_funs_tree,
                                                      static_aux_funs);
          current_fun = current_fun->next;
        }
        current_fun = funs_to_yield_no_auto.head;
        while(current_fun != NULL){
          tree = ast_get_ast_with_yieldified_function(tree,
                                                      header_file,
                                                      current_fun->str,
                                                      &all_yield_funs,
                                                      false,
                                                      false,
                                                      debug,
                                                      only_yielding_funs,
                                                      &only_yielding_funs_tree,
                                                      static_aux_funs);
          current_fun = current_fun->next;
        }
        current_fun = funs_to_yield_frec.head;
        while(current_fun != NULL){
          tree = ast_get_ast_with_yieldified_function(tree,
                                                      header_file,
                                                      current_fun->str,
                                                      &all_yield_funs,
                                                      true,
                                                      true,
                                                      debug,
                                                      only_yielding_funs,
                                                      &only_yielding_funs_tree,
                                                      static_aux_funs);
          current_fun = current_fun->next;
        }
        ast_add_yield_code_generated_define(tree, debug);
        ast_add_yield_code_generated_define(header_file, debug);
        if(only_yielding_funs){
          ast_add_yield_code_generated_define(only_yielding_funs_tree, debug);
        }
        if(header_file_name != NULL){
          ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
            ycf_node_print(header_file, b);
          str_to_file(header_file_name, b->buffer);
        }
        if(only_yielding_funs){
          tree = only_yielding_funs_tree;
        }
        if(output_file_name != NULL){
          ycf_string_printable_buffer* b = ycf_string_printable_buffer_new();
          ycf_node_print(tree, b);
          str_to_file(output_file_name, b->buffer);
        } else {
            ycf_node_print(tree, NULL);
        }
      }
    }
  }
  return 0;
}

bool is_help_option(char* string) {
return
  ycf_string_is_equal("-h", string) ||
  ycf_string_is_equal("--h", string) ||
  ycf_string_is_equal("-help", string) ||
  ycf_string_is_equal("--help", string);
}

int main( int argc, char* argv[] )
{
  bool log_max_mem_usage = false;
  bool use_gc = false;
  char * log_max_mem_usage_file = NULL;
  int i = 1;
  if (argc == 1) {
    print_help_text_and_exit(argv[0], 0);
  }
  while(i < argc &&
        (ycf_string_is_equal("-use_gc", argv[i]) ||
         ycf_string_is_equal("-log_max_mem_usage", argv[i]) ||
         ycf_string_is_equal("-print_gc_info", argv[i]) ||
         is_help_option(argv[i]))) {
    if (is_help_option(argv[i])) {
      print_help_text_and_exit(argv[0], 0);
    } else if (ycf_string_is_equal("-use_gc", argv[i])) {
      use_gc = true;
      i++;
    } else if(ycf_string_is_equal("-print_gc_info", argv[i])) {
      scgc_enable_print_gc_info();
      i++;
    } else if(ycf_string_is_equal(argv[i], "-log_max_mem_usage")) {
      ycf_enable_memory_tracking();
      log_max_mem_usage = true;
      i++;
      log_max_mem_usage_file = (char*)argv[i];
      i++;
    }
  }
  int nr_of_params_to_remove =  i - 1;
  int ret;
  if (!use_gc) {
    ret = ycf_main(argc - nr_of_params_to_remove ,
                       argv + nr_of_params_to_remove);
  } else {
    ycf_enable_gc();
    ret = scgc_start_gced_code(ycf_main,
                                   argc - nr_of_params_to_remove,
                                   argv + nr_of_params_to_remove,
                                   ycf_raw_malloc,
                                   ycf_free);
  }
  if(log_max_mem_usage){
    ycf_malloc_log(log_max_mem_usage_file, "all");
  }
  return ret;
}
