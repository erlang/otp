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

#ifndef YIELDING_C_FUN_LISTS_H
#define YIELDING_C_FUN_LISTS_H

#include <stdlib.h>
#include <stdio.h>


#define INIT_LIST(L)                            \
  do{                                           \
    (L)->head = NULL;                           \
    (L)->last = NULL;                           \
  }while(0)

#define APPEND_LIST(L,E)                        \
  do{                                           \
    void* e = E;                                \
    if((L)->head == NULL){                      \
      (L)->head = e;                            \
    } else {                                    \
      (L)->last->next = e;                      \
    }                                           \
    (L)->last = e;                              \
  }while(0)

#define PREPEND_LIST(T,L,E)                     \
  do{                                           \
    T* e = E;                                   \
    if((L)->head == NULL){                      \
      (L)->last = e;                            \
    } else {                                    \
      e->next = (L)->head;                      \
    }                                           \
    (L)->head = e;                              \
  }while(0)

#define CONCAT_LIST(T,L1,L2)                    \
  do{                                           \
    T* current = (L2)->head;                    \
    while(current != NULL){                     \
      APPEND_LIST(L1, current);                 \
      current = current->next;                  \
    }                                           \
  }while(0)

#define PRINT_LIST(T,L, NAME)                   \
  do{                                           \
    printf("NAME %s\n", NAME);                  \
    printf("HEAD %p\n", (L)->head);             \
    printf("LAST %p\n", (L)->last);             \
    printf("ELEMS:\n");                         \
    T* current = (L)->head;                     \
    while(current != NULL){                     \
      printf("E: %p\n", current);               \
      current = current->next;                  \
    }                                           \
  }while(0)

#define REMOVE_LIST(T,L,E)                              \
  do{                                                   \
    T* prev = NULL;                                     \
    T* rl_current  = (L)->head;                         \
    T* e = E;                                           \
    while(rl_current  != e && rl_current  != NULL){     \
      prev = rl_current ;                               \
      rl_current  = rl_current ->next;                  \
    }                                                   \
    if(rl_current  == NULL){                            \
      exit(1);                                          \
    }                                                   \
    if(prev == NULL){                                   \
      if((L)->head == (L)->last){                       \
        (L)->last = NULL;                               \
      }                                                 \
      (L)->head = (L)->head->next;                      \
    }else{                                              \
      if(rl_current  == (L)->last){                     \
        (L)->last = prev;                               \
      }                                                 \
      prev->next = rl_current ->next;                   \
    }                                                   \
  }while(0)

#define INSERT_AFTER_LIST(T,L,E,TO_INSERT)              \
  do{                                                   \
    T* current_y = (L)->head;                           \
    T* elem_x = E;                                      \
    T* to_insert2 = TO_INSERT;                          \
    if(elem_x == NULL){                                 \
      PREPEND_LIST(T,L,to_insert2);                     \
      break;                                            \
    }else if(elem_x == (L)->last){                      \
      APPEND_LIST(L,to_insert2);                        \
      break;                                            \
    }                                                   \
    while(current_y != elem_x && current_y != NULL){    \
      current_y = current_y->next;                      \
    }                                                   \
    if(current_y == NULL){                              \
      printf("CANNOT INSERT AFTER NONEXISTING\n");      \
      exit(1);                                          \
    }                                                   \
    to_insert2->next = current_y->next;                 \
    current_y->next = to_insert2;                       \
  }while(0)

#define INSERT_BEFORE_LIST(T,L,E_BEFORE,TO_INSERT)              \
  do{                                                           \
    T* prev_x = NULL;                                           \
    T* current_x = (L)->head;                                   \
    T* to_insert_x = TO_INSERT;                                 \
    T* e_before_x = E_BEFORE;                                   \
    while(current_x != e_before_x && current_x != NULL){        \
      prev_x = current_x;                                       \
      current_x = current_x->next;                              \
    }                                                           \
    if(current_x == NULL){                                      \
      printf("CANNOT INSERT AFTER NONEXISTING\n");              \
      exit(1);                                                  \
    }                                                           \
    INSERT_AFTER_LIST(T,L,prev_x,to_insert_x);                  \
  }while(0)


#define REPLACE_LIST(T,L,OLD,NEW)               \
  do{                                           \
    T* old = OLD;                               \
    T* new = NEW;                               \
    T* prev_old_next = old->next;               \
    INSERT_AFTER_LIST(T,L,old,new);             \
    REMOVE_LIST(T,L,old);                       \
    old->next = prev_old_next;                  \
  }while(0)


/* list functions */

#define GENERATE_LIST_FUNCTIONS(NODE_TYPE)                              \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_empty(){                            \
    NODE_TYPE##_list list;                                              \
    INIT_LIST(&list);                                                   \
    return list;                                                        \
  }                                                                     \
                                                                        \
  NODE_TYPE* NODE_TYPE##_shallow_copy(NODE_TYPE* n){                    \
    NODE_TYPE* new = ycf_malloc(sizeof(NODE_TYPE));                     \
    *new = *n;                                                          \
    new->next = NULL;                                                   \
    return new;                                                         \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_shallow_copy(NODE_TYPE##_list n){   \
    NODE_TYPE##_list new;                                               \
    NODE_TYPE* current = n.head;                                        \
    INIT_LIST(&new);                                                    \
    while(current != NULL){                                             \
      APPEND_LIST(&new, NODE_TYPE##_shallow_copy(current));             \
      current = current->next;                                          \
    }                                                                   \
    return new;                                                         \
  }                                                                     \
                                                                        \
  int NODE_TYPE##_list_get_item_position(NODE_TYPE##_list* list, NODE_TYPE* node){ \
    NODE_TYPE* current = list->head;                                    \
    int pos = 0;                                                        \
    while(current != NULL){                                             \
      if(current == node){                                              \
        return pos;                                                     \
      }                                                                 \
      pos = pos + 1;                                                    \
      current = current->next;                                          \
    }                                                                   \
    return -1;                                                          \
  }                                                                     \
                                                                        \
  NODE_TYPE* NODE_TYPE##_list_get_item_at_position(NODE_TYPE##_list* list, int pos){ \
    NODE_TYPE* current = list->head;                                    \
    int current_pos = 0;                                                \
    while(current != NULL){                                             \
      if(current_pos == pos){                                           \
        return current;                                                 \
      }                                                                 \
      current_pos = current_pos + 1;                                    \
      current = current->next;                                          \
    }                                                                   \
    return NULL;                                                        \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_append(NODE_TYPE##_list* list, NODE_TYPE* node){ \
    APPEND_LIST(list, node);                                            \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_copy_append(NODE_TYPE##_list list, NODE_TYPE* node){ \
    NODE_TYPE##_list list_copy = NODE_TYPE##_list_shallow_copy(list);   \
    NODE_TYPE* node_copy = NODE_TYPE##_shallow_copy(node);              \
    NODE_TYPE##_list_append(&list_copy, node_copy);                     \
    return list_copy;                                                   \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_prepend(NODE_TYPE##_list* list, NODE_TYPE* node){ \
    PREPEND_LIST(NODE_TYPE, list, node);                                \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_copy_prepend(NODE_TYPE##_list list, NODE_TYPE* node){ \
    NODE_TYPE##_list list_copy = NODE_TYPE##_list_shallow_copy(list);   \
    NODE_TYPE* node_copy = NODE_TYPE##_shallow_copy(node);              \
    NODE_TYPE##_list_prepend(&list_copy, node_copy);                    \
    return list_copy;                                                   \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_insert_before(NODE_TYPE##_list* list, NODE_TYPE* before_this, NODE_TYPE* to_insert_z){ \
    INSERT_BEFORE_LIST(NODE_TYPE, list, before_this, to_insert_z);      \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_copy_insert_before(NODE_TYPE##_list list, NODE_TYPE* before_this, NODE_TYPE* to_insert){ \
    int pos = NODE_TYPE##_list_get_item_position(&list, before_this);   \
    NODE_TYPE##_list list_copy = NODE_TYPE##_list_shallow_copy(list);;  \
    NODE_TYPE* actual_this = NODE_TYPE##_list_get_item_at_position(&list_copy, pos); \
    NODE_TYPE* to_insert_copy = NODE_TYPE##_shallow_copy(to_insert);    \
    NODE_TYPE##_list_insert_before(&list_copy, actual_this, to_insert_copy); \
    return list_copy;                                                   \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_insert_after(NODE_TYPE##_list* list, NODE_TYPE* after_this, NODE_TYPE* to_insert_z){ \
    INSERT_AFTER_LIST(NODE_TYPE, list, after_this, to_insert_z);        \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_copy_insert_after(NODE_TYPE##_list list, NODE_TYPE* after_this, NODE_TYPE* to_insert){ \
    int pos = NODE_TYPE##_list_get_item_position(&list, after_this);    \
    NODE_TYPE##_list list_copy = NODE_TYPE##_list_shallow_copy(list);   \
    NODE_TYPE* actual_this = NODE_TYPE##_list_get_item_at_position(&list_copy, pos); \
    NODE_TYPE* to_insert_copy = NODE_TYPE##_shallow_copy(to_insert);    \
    NODE_TYPE##_list_insert_after(&list_copy, actual_this, to_insert_copy); \
    return list_copy;                                                   \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_remove(NODE_TYPE##_list* list, NODE_TYPE* to_remove){ \
    REMOVE_LIST(NODE_TYPE, list, to_remove);                            \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_copy_remove(NODE_TYPE##_list list, NODE_TYPE* to_remove){ \
    int pos = NODE_TYPE##_list_get_item_position(&list, to_remove);     \
    NODE_TYPE##_list list_copy = NODE_TYPE##_list_shallow_copy(list);   \
    NODE_TYPE* actual_this = NODE_TYPE##_list_get_item_at_position(&list_copy, pos); \
    NODE_TYPE##_list_remove(&list_copy, actual_this);                   \
    return list_copy;                                                   \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_replace(NODE_TYPE##_list* list, NODE_TYPE* to_replace, NODE_TYPE* replace_with){ \
    REPLACE_LIST(NODE_TYPE, list, to_replace, replace_with);            \
  }                                                                     \
                                                                        \
  NODE_TYPE##_list NODE_TYPE##_list_copy_replace(NODE_TYPE##_list list, NODE_TYPE* to_replace, NODE_TYPE* replace_with){ \
    int pos = NODE_TYPE##_list_get_item_position(&list, to_replace);    \
    NODE_TYPE##_list list_copy = NODE_TYPE##_list_shallow_copy(list);   \
    NODE_TYPE* actual_this = NODE_TYPE##_list_get_item_at_position(&list_copy, pos); \
    NODE_TYPE* replace_with_copy = NODE_TYPE##_shallow_copy(replace_with); \
    NODE_TYPE##_list_replace(&list_copy, actual_this, replace_with_copy); \
    return list_copy;                                                   \
  }                                                                     \
                                                                        \
  void NODE_TYPE##_list_concat(NODE_TYPE##_list* list1, NODE_TYPE##_list* list2){ \
    CONCAT_LIST(NODE_TYPE, list1, list2);                               \
  }                                                                     \
  NODE_TYPE##_list NODE_TYPE##_list_copy_concat(NODE_TYPE##_list list1, NODE_TYPE##_list list2){ \
    NODE_TYPE##_list list1_copy = NODE_TYPE##_list_shallow_copy(list1); \
    NODE_TYPE##_list list2_copy = NODE_TYPE##_list_shallow_copy(list2); \
    CONCAT_LIST(NODE_TYPE, &list1_copy, &list2_copy);                   \
    return list1_copy;                                                  \
  }                                                                     \
                                                                        \
  size_t NODE_TYPE##_list_length(NODE_TYPE##_list list){                \
    NODE_TYPE* current = list.head;                                     \
    size_t count = 0;                                                   \
    while(current != NULL){                                             \
      count = count + 1;                                                \
      current = current->next;                                          \
    }                                                                   \
    return count;                                                       \
  }

/* void print_string_list(string_list n){ */
/*   string_list_item* current = n.head; */
/*   printf("START\n"); */
/*   while(current != NULL){ */
/*     printf("%s\n", current->str); */
/*     current = current->next; */
/*   } */
/*   printf("END\n"); */
/* } */

#endif //YIELDING_C_FUN_LISTS_H
