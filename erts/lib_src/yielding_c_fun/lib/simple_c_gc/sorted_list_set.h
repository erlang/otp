/*
 * %CopyrightBegin%
 *
 * Copyright 2019 Kjell Winblad (kjellwinblad@gmail.com, http://winsh.me).
 * All Rights Reserved.
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
 *
 * Author: Kjell Winblad
 *
 */

#ifndef SORTED_LIST_SET_H
#define SORTED_LIST_SET_H

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct SortedListSetNodeImpl {
  struct SortedListSetNodeImpl *next;
  unsigned int key_hash_value;
  unsigned int valueSize;
  char value[]; /* Flexible size array member */
} SortedListSetNode;

typedef struct {
  SortedListSetNode *head;
  unsigned int keyPosition;
  void *(*extract_key)(void *v, int keyPos);
  unsigned int (*hash_key)(void *k);
  bool (*are_equal)(void *v1, void *v2);
  char *(*to_string)(void *v1);
  void *(*malloc)(size_t size);
  void (*free)(void *ptr);
} SortedListSet;

static inline SortedListSet *plain_sl_set_create(
    unsigned int keyPosition, void *(*extract_key)(void *v, int keyPos),
    unsigned int (*hash_key)(void *k), bool (*are_equal)(void *v1, void *v2),
    char *(*to_string)(void *v1), void *(*my_malloc)(size_t size),
    void (*my_free)(void *ptr)) {
  SortedListSet *set = my_malloc(sizeof(SortedListSet));
  set->head = NULL;
  set->keyPosition = keyPosition;
  set->extract_key = extract_key;
  set->hash_key = hash_key;
  set->are_equal = are_equal;
  set->to_string = to_string;
  set->malloc = my_malloc;
  set->free = my_free;
  return set;
}

static inline int compare_hash_codes(unsigned int code1, unsigned int code2) {
  return code1 - code2;
}

static inline bool sl_set_insert_opt(SortedListSetNode **root, void *valuePtr,
                                     unsigned int valueSize,
                                     unsigned int keyHashValue, int keyPosition,
                                     bool overwrite,
                                     void *(*extract_key)(void *v, int keyPos),
                                     bool (*are_equal)(void *v1, void *v2),
                                     void *(*my_malloc)(size_t size),
                                     void (*my_free)(void *ptr)) {
  void *key = extract_key(valuePtr, keyPosition);
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  bool oneMore = true;
  int compareResult;
  while (current != NULL) {
    compareResult = compare_hash_codes(current->key_hash_value, keyHashValue);
    if (compareResult < 0) {
      previous = current;
      current = previous->next;
    } else if (compareResult > 0) {
      break;
    } else {
      if (are_equal(extract_key(current->value, keyPosition), key)) {
        if (overwrite) {
          SortedListSetNode *oldCurrent = current;
          current = current->next;
          previous->next = current;
          my_free(oldCurrent);
          oneMore = false;
          break;
        } else {
          return false;
        }
      } else {
        previous = current;
        current = previous->next;
      }
    }
  }
  SortedListSetNode *newNode = my_malloc(sizeof(SortedListSetNode) + valueSize);
  previous->next = newNode;
  newNode->next = current;
  newNode->key_hash_value = keyHashValue;
  memcpy(newNode->value, valuePtr, valueSize);
  newNode->valueSize = valueSize;
  return oneMore;
}

static inline void sl_set_insert(void *setParam, void *valuePtr,
                                 unsigned int valueSize) {
  SortedListSet *set = (SortedListSet *)setParam;
  sl_set_insert_opt(&set->head, valuePtr, valueSize,
                    set->hash_key(set->extract_key(valuePtr, set->keyPosition)),
                    set->keyPosition, true, set->extract_key, set->are_equal,
                    set->malloc, set->free);
}

static inline bool sl_set_insert_new(void *setParam, void *valuePtr,
                                     unsigned int valueSize) {
  SortedListSet *set = (SortedListSet *)setParam;
  return sl_set_insert_opt(
      &set->head, valuePtr, valueSize,
      set->hash_key(set->extract_key(valuePtr, set->keyPosition)),
      set->keyPosition, false, set->extract_key, set->are_equal, set->malloc,
      set->free);
}

static inline void *sl_set_lookup_opt(SortedListSetNode **root, void *key,
                                      unsigned int keyHashValue,
                                      int keyPosition,
                                      void *(*extract_key)(void *v, int keyPos),
                                      bool (*are_equal)(void *v1, void *v2),
                                      bool copyOut,
                                      void *(*my_malloc)(size_t size)) {
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  int compareResult;
  while (current != NULL) {
    compareResult = compare_hash_codes(current->key_hash_value, keyHashValue);
    if (compareResult < 0) {
      previous = current;
      current = previous->next;
    } else if (compareResult > 0) {
      return NULL;
    } else if (are_equal(extract_key(current->value, keyPosition), key)) {
      if (copyOut) {
        void *toReturn = my_malloc(current->valueSize);
        memcpy(toReturn, current->value, current->valueSize);
        return toReturn;
      } else {
        return current->value;
      }
    } else {
      previous = current;
      current = previous->next;
    }
  }
  return NULL;
}

static inline void *sl_set_lookup(void *setParam, void *key) {
  SortedListSet *set = (SortedListSet *)setParam;
  return sl_set_lookup_opt(&set->head, key, set->hash_key(key),
                           set->keyPosition, set->extract_key, set->are_equal,
                           false, set->malloc);
}

static inline bool sl_set_delete_opt(SortedListSetNode **root, void *key,
                                     unsigned int keyHashValue, int keyPosition,
                                     void *(*extract_key)(void *v, int keyPos),
                                     bool (*are_equal)(void *v1, void *v2),
                                     void (*my_free)(void *ptr)) {
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  int compareResult;
  while (current != NULL) {
    compareResult = compare_hash_codes(current->key_hash_value, keyHashValue);
    if (compareResult < 0) {
      previous = current;
      current = previous->next;
    } else if (compareResult > 0) {
      return false;
    } else if (are_equal(extract_key(current->value, keyPosition), key)) {
      previous->next = current->next;
      my_free(current);
      return true;
    } else {
      previous = current;
      current = previous->next;
    }
  }
  return false;
}

static inline void sl_set_delete(void *setParam, void *key,
                                 unsigned int keySize) {
  (void)keySize;
  SortedListSet *set = (SortedListSet *)setParam;
  sl_set_delete_opt(&set->head, key, set->hash_key(key), set->keyPosition,
                    set->extract_key, set->are_equal, set->free);
}

static inline void sl_set_free_opt(SortedListSetNode **root,
                                   void (*my_free)(void *ptr)) {
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  while (current != NULL) {
    previous = current;
    current = current->next;
    my_free(previous);
  }
}

static inline void sl_set_free(void *setParam) {
  SortedListSet *set = (SortedListSet *)setParam;
  sl_set_free_opt(&set->head, set->free);
  set->free(set);
}

static inline SortedListSetNode *sl_set_split_opt(SortedListSetNode **root,
                                                  unsigned int splitPattern) {
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  while (current != NULL) {
    if (current->key_hash_value & splitPattern) {
      previous->next = NULL;
      return current;
    }
    previous = current;
    current = previous->next;
  }
  return NULL;
}

static inline void sl_set_concat_opt(SortedListSetNode **root,
                                     SortedListSetNode *list) {
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  while (current != NULL) {
    previous = current;
    current = previous->next;
  }
  previous->next = list;
}

static inline void sl_set_append_opt(SortedListSetNode **root,
                                     SortedListSetNode *list) {
  if (list == NULL) {
    return;
  }
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  while (current != NULL) {
    previous = current;
    current = previous->next;
  }
  previous->next = list;
}

static inline void *
sl_set_fold_opt(SortedListSetNode **root, void *initialValue,
                void *(*f)(void *soFar, void *currentValue)) {
  SortedListSetNode *previous = (SortedListSetNode *)root;
  SortedListSetNode *current = previous->next;
  void *soFar = initialValue;
  while (current != NULL) {
    soFar = f(soFar, current->value);
    current = current->next;
  }
  return soFar;
}

static inline void *sl_set_fold(SortedListSet *set, void *initialValue,
                                void *(*f)(void *soFar, void *currentValue)) {
  return sl_set_fold_opt(&set->head, initialValue, f);
}

static inline void *_______size_helper(void *soFarParam, void *currentValue) {
  unsigned int *soFar = (unsigned int *)soFarParam;
  (void)currentValue;
  unsigned int prev = *soFar;
  *soFar = prev + 1;
  return soFar;
}

static inline unsigned int sl_set_size_opt(SortedListSetNode **root) {
  unsigned int size = 0;
  sl_set_fold_opt(root, &size, _______size_helper);
  return size;
}

static inline unsigned int sl_set_size(SortedListSet *set) {
  return sl_set_size_opt(&set->head);
}

static inline void sl_set_print(void *setParam) {
  SortedListSet *set = (SortedListSet *)setParam;
  SortedListSetNode *previous = (SortedListSetNode *)set;
  SortedListSetNode *current = previous->next;
  printf("[");
  while (current != NULL) {
    char *string = set->to_string(current->value);
    printf("%s", string);
    set->free(string);
    previous = current;
    current = previous->next;
    if (current != NULL) {
      printf(",");
    }
  }
  printf("]\n");
}

static inline char *sl_set_to_string(void *setParam) {
  SortedListSet *set = (SortedListSet *)setParam;
  unsigned int numberOfElements = sl_set_size(set);
  char **stringArray = malloc(sizeof(char*)*numberOfElements);
  unsigned int *stringLengths = malloc(sizeof(unsigned int)*numberOfElements);
  SortedListSetNode *previous = (SortedListSetNode *)set;
  SortedListSetNode *current = previous->next;
  unsigned int elementNumber = 0;
  unsigned int totalCharCount = 2;
  if (numberOfElements == 0) {
    char *buffer = (char *)set->malloc(3);
    sprintf(buffer, "[]");
    free(stringArray);
    free(stringLengths);
    return buffer;
  }
  while (current != NULL) {
    stringArray[elementNumber] = set->to_string(current->value);
    stringLengths[elementNumber] = strlen(stringArray[elementNumber]);
    totalCharCount = totalCharCount + stringLengths[elementNumber];
    current = current->next;
    elementNumber++;
  }
  totalCharCount = totalCharCount + numberOfElements;
  char *stringBuffer = set->malloc(totalCharCount);
  stringBuffer[0] = '[';
  unsigned int currentPosition = 1;
  for (unsigned int i = 0; i < numberOfElements; i++) {
    sprintf(&stringBuffer[currentPosition], "%s", stringArray[i]);
    set->free(stringArray[i]);
    currentPosition = currentPosition + stringLengths[i];
    if (i != (numberOfElements - 1)) {
      sprintf(&stringBuffer[currentPosition], ",");
      currentPosition = currentPosition + 1;
    }
  }
  sprintf(&stringBuffer[currentPosition], "]");
  free(stringArray);
  free(stringLengths);
  return stringBuffer;
}

#endif
