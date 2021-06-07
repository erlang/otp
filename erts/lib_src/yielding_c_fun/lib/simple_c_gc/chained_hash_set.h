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

#ifndef CHAINED_HASH_SET_H
#define CHAINED_HASH_SET_H


#if defined(_MSC_VER)
#define inline __inline
#endif

#include "sorted_list_set.h"
#include <stdint.h>

#define CHAIN_LENGHT_EXPAND_THRESHOLD 2
#define CHAIN_LENGHT_SHRINK_THRESHOLD 0.5
/*Must be power of two*/
#define INITIAL_NUMBER_OF_BUCKETS 4

typedef struct {
  unsigned int keyPosition;
  void *(*extract_key)(void *v, int keyPos);
  unsigned int (*hash_key)(void *key);
  bool (*are_equal)(void *v1, void *v2);
  char *(*to_string)(void *v1);
  void *(*malloc)(size_t size);
  void (*free)(void *ptr);
  unsigned int numberOfBuckets;
  unsigned int expandTreshold;
  unsigned int shrinkTreshold;
  unsigned int size;
  SortedListSetNode **buckets;
} ChainedHashSet;

/*
 * The reverse_bits function is inspired by:
 * https://stackoverflow.com/questions/746171/efficient-algorithm-for-bit-reversal-from-msb-lsb-to-lsb-msb-in-c
 */
static const unsigned char BitReverseTable256[] = {
    0x00, 0x80, 0x40, 0xC0, 0x20, 0xA0, 0x60, 0xE0, 0x10, 0x90, 0x50, 0xD0,
    0x30, 0xB0, 0x70, 0xF0, 0x08, 0x88, 0x48, 0xC8, 0x28, 0xA8, 0x68, 0xE8,
    0x18, 0x98, 0x58, 0xD8, 0x38, 0xB8, 0x78, 0xF8, 0x04, 0x84, 0x44, 0xC4,
    0x24, 0xA4, 0x64, 0xE4, 0x14, 0x94, 0x54, 0xD4, 0x34, 0xB4, 0x74, 0xF4,
    0x0C, 0x8C, 0x4C, 0xCC, 0x2C, 0xAC, 0x6C, 0xEC, 0x1C, 0x9C, 0x5C, 0xDC,
    0x3C, 0xBC, 0x7C, 0xFC, 0x02, 0x82, 0x42, 0xC2, 0x22, 0xA2, 0x62, 0xE2,
    0x12, 0x92, 0x52, 0xD2, 0x32, 0xB2, 0x72, 0xF2, 0x0A, 0x8A, 0x4A, 0xCA,
    0x2A, 0xAA, 0x6A, 0xEA, 0x1A, 0x9A, 0x5A, 0xDA, 0x3A, 0xBA, 0x7A, 0xFA,
    0x06, 0x86, 0x46, 0xC6, 0x26, 0xA6, 0x66, 0xE6, 0x16, 0x96, 0x56, 0xD6,
    0x36, 0xB6, 0x76, 0xF6, 0x0E, 0x8E, 0x4E, 0xCE, 0x2E, 0xAE, 0x6E, 0xEE,
    0x1E, 0x9E, 0x5E, 0xDE, 0x3E, 0xBE, 0x7E, 0xFE, 0x01, 0x81, 0x41, 0xC1,
    0x21, 0xA1, 0x61, 0xE1, 0x11, 0x91, 0x51, 0xD1, 0x31, 0xB1, 0x71, 0xF1,
    0x09, 0x89, 0x49, 0xC9, 0x29, 0xA9, 0x69, 0xE9, 0x19, 0x99, 0x59, 0xD9,
    0x39, 0xB9, 0x79, 0xF9, 0x05, 0x85, 0x45, 0xC5, 0x25, 0xA5, 0x65, 0xE5,
    0x15, 0x95, 0x55, 0xD5, 0x35, 0xB5, 0x75, 0xF5, 0x0D, 0x8D, 0x4D, 0xCD,
    0x2D, 0xAD, 0x6D, 0xED, 0x1D, 0x9D, 0x5D, 0xDD, 0x3D, 0xBD, 0x7D, 0xFD,
    0x03, 0x83, 0x43, 0xC3, 0x23, 0xA3, 0x63, 0xE3, 0x13, 0x93, 0x53, 0xD3,
    0x33, 0xB3, 0x73, 0xF3, 0x0B, 0x8B, 0x4B, 0xCB, 0x2B, 0xAB, 0x6B, 0xEB,
    0x1B, 0x9B, 0x5B, 0xDB, 0x3B, 0xBB, 0x7B, 0xFB, 0x07, 0x87, 0x47, 0xC7,
    0x27, 0xA7, 0x67, 0xE7, 0x17, 0x97, 0x57, 0xD7, 0x37, 0xB7, 0x77, 0xF7,
    0x0F, 0x8F, 0x4F, 0xCF, 0x2F, 0xAF, 0x6F, 0xEF, 0x1F, 0x9F, 0x5F, 0xDF,
    0x3F, 0xBF, 0x7F, 0xFF};

static inline uint32_t reverse_bits(uint32_t v) {
  return (((uint32_t)BitReverseTable256[v & 0xff]) << 24) |
         (((uint32_t)BitReverseTable256[(v >> 8) & 0xff]) << 16) |
         (((uint32_t)BitReverseTable256[(v >> 16) & 0xff]) << 8) |
         (((uint32_t)BitReverseTable256[(v >> 24) & 0xff]));
}

static inline void ch_set_increase_size(ChainedHashSet *set) {
  set->size = set->size + 1;
  if (set->size > set->expandTreshold) {
    unsigned int oldNumberOfBuckets = set->numberOfBuckets;
    unsigned int newNumberOfBuckets = oldNumberOfBuckets * 2;
    unsigned int splitUpMask = reverse_bits(newNumberOfBuckets - 1) ^
                               reverse_bits(oldNumberOfBuckets - 1);
    SortedListSetNode **newBuckets =
        set->malloc(sizeof(SortedListSetNode *) * newNumberOfBuckets);
    SortedListSetNode **oldBuckets = set->buckets;
    SortedListSetNode *moveTemp;
    for (unsigned int i = 0; i < oldNumberOfBuckets; i++) {
      moveTemp = sl_set_split_opt(&oldBuckets[i], splitUpMask);
      newBuckets[i] = oldBuckets[i];
      newBuckets[i + oldNumberOfBuckets] = moveTemp;
    }
    set->free(oldBuckets);
    set->buckets = newBuckets;
    set->numberOfBuckets = newNumberOfBuckets;
    set->expandTreshold = newNumberOfBuckets * CHAIN_LENGHT_EXPAND_THRESHOLD;
    set->shrinkTreshold = newNumberOfBuckets * CHAIN_LENGHT_SHRINK_THRESHOLD;
  }
}

static inline void ch_set_decrease_size(ChainedHashSet *set) {
  set->size = set->size - 1;
  if (set->size < set->shrinkTreshold) {
    unsigned int oldNumberOfBuckets = set->numberOfBuckets;
    unsigned int newNumberOfBuckets = oldNumberOfBuckets / 2;
    SortedListSetNode **newBuckets =
        set->malloc(sizeof(SortedListSetNode *) * newNumberOfBuckets);
    SortedListSetNode **oldBuckets = set->buckets;
    for (unsigned int i = 0; i < newNumberOfBuckets; i++) {
      newBuckets[i] = oldBuckets[i];
      sl_set_concat_opt(&newBuckets[i], oldBuckets[i + newNumberOfBuckets]);
    }
    set->free(oldBuckets);
    set->buckets = newBuckets;
    set->numberOfBuckets = newNumberOfBuckets;
    set->expandTreshold = newNumberOfBuckets * CHAIN_LENGHT_EXPAND_THRESHOLD;
    if (set->numberOfBuckets == INITIAL_NUMBER_OF_BUCKETS) {
      set->shrinkTreshold = 0;
    } else {
      set->shrinkTreshold = newNumberOfBuckets * CHAIN_LENGHT_SHRINK_THRESHOLD;
    }
  }
}

static inline void ch_set_initialize(ChainedHashSet *set,
                                     unsigned int keyPosition,
                                     void *(*extract_key)(void *v, int keyPos),
                                     unsigned int (*hash_key)(void *k),
                                     bool (*are_equal)(void *v1, void *v2),
                                     char *(*to_string)(void *v1),
                                     void *(*my_malloc)(size_t size),
                                     void (*my_free)(void *ptr)) {
  set->keyPosition = keyPosition;
  set->extract_key = extract_key;
  set->hash_key = hash_key;
  set->are_equal = are_equal;
  set->to_string = to_string;
  set->malloc = my_malloc;
  set->free = my_free;
  set->numberOfBuckets = INITIAL_NUMBER_OF_BUCKETS;
  set->expandTreshold = set->numberOfBuckets * CHAIN_LENGHT_EXPAND_THRESHOLD;
  set->shrinkTreshold = set->numberOfBuckets * CHAIN_LENGHT_EXPAND_THRESHOLD;
  set->size = 0;
  set->buckets =
      set->malloc(sizeof(SortedListSetNode *) * INITIAL_NUMBER_OF_BUCKETS);
  for (int i = 0; i < INITIAL_NUMBER_OF_BUCKETS; i++) {
    set->buckets[i] = NULL;
  }
}

static inline ChainedHashSet *ch_set_create(
    unsigned int keyPosition, void *(*extract_key)(void *v, int keyPos),
    unsigned int (*hash_key)(void *k), bool (*are_equal)(void *v1, void *v2),
    char *(*to_string)(void *v1), void *(*my_malloc)(size_t size),
    void (*my_free)(void *ptr)) {
  ChainedHashSet *set = my_malloc(sizeof(ChainedHashSet));
  ch_set_initialize(set, keyPosition, extract_key, hash_key, are_equal,
                    to_string, my_malloc, my_free);
  return set;
}

static inline bool ch_set_insert_opt(ChainedHashSet *set, void *value,
                                     unsigned int valueSize,
                                     unsigned int hashValue, bool overwrite) {
  unsigned int bucketIndex = hashValue & (set->numberOfBuckets - 1);
  SortedListSetNode **bucket = &set->buckets[bucketIndex];
  bool oneAdded = sl_set_insert_opt(
      bucket, value, valueSize, reverse_bits(hashValue), set->keyPosition,
      overwrite, set->extract_key, set->are_equal, set->malloc, set->free);
  if (oneAdded) {
    ch_set_increase_size(set);
  }
  return oneAdded;
}

static inline void ch_set_insert(void *setParam, void *value,
                                 unsigned int valueSize) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  void *key = set->extract_key(value, set->keyPosition);
  unsigned int hashValue = set->hash_key(key);
  ch_set_insert_opt(set, value, valueSize, hashValue, true);
}

static inline bool ch_set_insert_new(void *setParam, void *value,
                                     unsigned int valueSize) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  void *key = set->extract_key(value, set->keyPosition);
  unsigned int hashValue = set->hash_key(key);
  return ch_set_insert_opt(set, value, valueSize, hashValue, false);
}

static inline void *ch_set_lookup(void *setParam, void *key) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  unsigned int hashValue = set->hash_key(key);
  unsigned int bucketIndex = hashValue & (set->numberOfBuckets - 1);
  SortedListSetNode **bucket = &set->buckets[bucketIndex];
  return sl_set_lookup_opt(bucket, key, reverse_bits(hashValue),
                           set->keyPosition, set->extract_key, set->are_equal,
                           false, set->malloc);
}

static inline void ch_set_delete(void *setParam, void *key,
                                 unsigned int keySize) {
  (void)keySize;
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  unsigned int hashValue = set->hash_key(key);
  unsigned int bucketIndex = hashValue & (set->numberOfBuckets - 1);
  SortedListSetNode **bucket = &set->buckets[bucketIndex];
  bool oneRemoved =
      sl_set_delete_opt(bucket, key, reverse_bits(hashValue), set->keyPosition,
                        set->extract_key, set->are_equal, set->free);
  if (oneRemoved) {
    ch_set_decrease_size(set);
  }
}

static inline void ch_set_traverse(void *setParam,
                                   void (*traverser)(size_t index, void *v,
                                                     void *context),
                                   void *context) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  unsigned int numberOfBuckets = set->numberOfBuckets;
  SortedListSetNode *itemNode;
  size_t index = 0;
  for (unsigned int i = 0; i < numberOfBuckets; i++) {
    itemNode = set->buckets[i];
    while (itemNode != NULL) {
      traverser(index, (void *)itemNode->value, context);
      index++;
      itemNode = itemNode->next;
    }
  }
}

static inline void ch_set_free(void *setParam) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  unsigned int numberOfBuckets = set->numberOfBuckets;
  for (unsigned int i = 0; i < numberOfBuckets; i++) {
    sl_set_free_opt(&set->buckets[i], set->free);
  }
  set->free(set->buckets);
  set->free(set);
}

static inline char *ch_set_to_string(void *setParam) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  unsigned int numberOfBuckets = set->numberOfBuckets;
  char **bucketStrings = malloc(sizeof(char*)*numberOfBuckets);
  unsigned int *bucketStringSizes = malloc(sizeof(unsigned int)*numberOfBuckets);
  unsigned int totalBucketsStringSize = 0;
  SortedListSet *tempListSet = plain_sl_set_create(
      set->keyPosition, set->extract_key, set->hash_key, set->are_equal,
      set->to_string, set->malloc, set->free);
  for (unsigned int i = 0; i < numberOfBuckets; i++) {
    tempListSet->head = set->buckets[i];
    bucketStrings[i] = sl_set_to_string(tempListSet);
    bucketStringSizes[i] = strlen(bucketStrings[i]);
    totalBucketsStringSize = totalBucketsStringSize + bucketStringSizes[i];
  }
  tempListSet->head = NULL;
  sl_set_free(tempListSet);
  char *resultString =
      set->malloc(totalBucketsStringSize + numberOfBuckets * 3 - 3 + 3);
  resultString[0] = '[';
  unsigned int currentPosition = 1;
  for (unsigned int i = 0; i < numberOfBuckets; i++) {
    sprintf(&resultString[currentPosition], "%s", bucketStrings[i]);
    set->free(bucketStrings[i]);
    currentPosition = currentPosition + bucketStringSizes[i];
    if (i != (numberOfBuckets - 1)) {
      sprintf(&resultString[currentPosition], ",\n ");
      currentPosition = currentPosition + 3;
    }
  }
  sprintf(&resultString[currentPosition], "]");
  free(bucketStrings);
  free(bucketStringSizes);
  return resultString;
}

static inline void ch_set_print(void *setParam) {
  ChainedHashSet *set = (ChainedHashSet *)setParam;
  char *str = ch_set_to_string(set);
  printf("%s\n", str);
  set->free(str);
}

static inline bool ch_set_is_concurrent() { return false; }

#endif
