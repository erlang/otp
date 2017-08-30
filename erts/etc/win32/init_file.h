/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

typedef struct {
    char *key;
    char *value; /* Key and value points into same buffer */
} InitEntry;

typedef struct {
    int num_entries;
    int size_entries;
    char *section_name;
    InitEntry **entries;
} InitSection;

typedef struct {
    int num_sections;
    int size_sections;
    InitSection **sections;
} InitFile;

/* Load a file structure from a disk file */
InitFile *load_init_file(wchar_t *filename);

/* Stores a file structure into a disk file */
int store_init_file(InitFile *inif, wchar_t *filename);

/* Create an empty  file structure */
InitFile *create_init_file(void);

/* Free a file structure and everything associateed (including sections,keys 
   and values and anything looked up but not copied) */
void free_init_file(InitFile *inif);

/* Create empty section */
InitSection *create_init_section(char *section_name);

/* Add section to file Overwrites and destroys old sections with same key */
int add_init_section(InitFile *inif, InitSection *inis); 

/* Kills a named section from a file. Destroys so that previously looked up
   sections (with this key) need to be copied before the delete */
int delete_init_section(InitFile *inif, char *section_name); 

/* lookup returns pointer into existing data. If data is to be preserved
   across deletes or overwrites, it has to be copied */
InitSection *lookup_init_section(InitFile *inif, char *section_name);

/* Returns the name of the nth init section, n is >= 0, ret NULL when done */
char *nth_init_section_name(InitFile *inif, int n);

/* To save an init section so that delete or overwrite does not destroy it,
   one needs to copy it */
InitSection *copy_init_section(InitSection *inis, char *new_name); 

/* Frees up everything in the section, keys and values as well. */
void free_init_section(InitSection *inis);

/* Key and value are copied in add_entry */
int add_init_entry(InitSection *inis, char *key, char *value); 

/* Returns pointer into internal string, use strcpy to save across 
   updates/deletes */
char *lookup_init_entry(InitSection *inis, char *key); 

/* Returns the name of the nth entry key, n is >= 0, ret NULL when done */
char *nth_init_entry_key(InitSection *inis, int n);

/* Destroys entry, prevoiusly looked up entries need be 
   copied before deleted */
int delete_init_entry(InitSection *inis, char *key); 

#define INIT_FILE_NO_ERROR 0
#define INIT_FILE_OPEN_ERROR -1
#define INIT_FILE_WRITE_ERROR -2
#define INIT_FILE_PRESENT 0
#define INIT_FILE_NOT_PRESENT 1
