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
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "init_file.h"

#define ALLOC malloc
#define REALLOC realloc
#define FREE free 

#define CONTEXT_BUFFER_SIZE 1024

typedef struct {
    HANDLE fd;
    int eof;
    int num;
    int pos;
    char buff[CONTEXT_BUFFER_SIZE];
} FileContext;

static char *read_one_line(FileContext *fc)
{
    char *buff = ALLOC(10);
    int size = 10;
    int num = 0;
    int skipping;
    int escaped;

#define PUSH(Char) 				\
    do {					\
	if (num == size) {			\
	    size += 10;				\
	    buff = REALLOC(buff,size);		\
	}					\
	buff[num++] = (Char);			\
    } while(0)

#define POP() (buff[--num])
#define TOP() (buff[(num-1)])

    skipping = 0;
    escaped = 0;
    for(;;) {
	char current;
	if (fc->eof) {
	    break;
	}
	if (fc->pos == fc->num) {
	    if (!ReadFile(fc->fd, fc->buff, CONTEXT_BUFFER_SIZE, 
			  &(fc->num), NULL) || !(fc->num)) {
		fc->eof = 1;
		break;
	    }
	    fc->pos = 0;
	}
	current = fc->buff[fc->pos];
	++(fc->pos);
	switch (current) {
	case ' ':
	    if (!skipping && num) {
		PUSH(current);
	    }
	    escaped = 0;
	    break;
	case ';':
	    if (!skipping) {
		if (!escaped) {
		    skipping = 1;
		} else {
		    PUSH(current);
		}
	    }
	    escaped = 0;
	    break;
	case '\\':
	    if (!skipping) {
		if (!escaped) {
		    escaped = 1;
		} else {
		    PUSH(current);
		    escaped = 0;
		}
	    }
	    break;
	case '\r':
	    break;
	case '\n':
	    if (!escaped) {
		while (num && TOP() == ' ') {
			POP();
		}
		if (num) {
		    goto done;
		}
	    } 
	    skipping = 0;
	    escaped = 0;
	    break;
	default:
	    if (!skipping) {
		PUSH(current);
	    }
	    escaped = 0;
	    break;
	}
    }
    /* EOF comes here */
    while (num && TOP() == ' ') {
	POP();
    }
    if (!num) {
	FREE(buff);
	return NULL;
    }
 done:
    PUSH('\0');
    return buff;
#undef PUSH
#undef POP
#undef TOP
}

static int is_section_header(char *line) 
{
    int x = strlen(line);
    return (x > 2 && *line == '[' && line[x-1] == ']');
}

static int is_key_value(char *line)
{
    char *p = strchr(line,'=');

    return (p != NULL && p > line);
}

static char *digout_section_name(char *line)
     /* Moving it because it shall later be freed. */
{
    int x = strlen(line);
    memmove(line,line+1,x-1);
    line[x-2] = '\0';
    return line;
}

static void digout_key_value(char *line, char **key, char **value)
{
    char *e = strchr(line,'=');
    *key = line;
    *value = (e+1);
    *e = '\0';
    while (*(--e) == ' ') {
	*e = '\0';
    }
    while (**value == ' ') {
	++(*value);
    }
}

InitFile *load_init_file(wchar_t *filename)
{
    HANDLE infile;
    InitFile *inif;
    InitSection *inis;
    InitEntry *inie;
    FileContext fc;
    char *line;
    char **lines;
    int size_lines;
    int num_lines;

    int i;

    if ( (infile = CreateFileW(filename, 
			       GENERIC_READ,
			       FILE_SHARE_READ,
			       NULL,
			       OPEN_EXISTING,
			       FILE_ATTRIBUTE_NORMAL,
			       NULL)) == INVALID_HANDLE_VALUE) {
	return NULL;
    }
    
    size_lines = 10;
    num_lines = 0;
    lines = ALLOC(size_lines * sizeof(char *));

    fc.fd = infile;
    fc.eof = 0;
    fc.num = 0;
    fc.pos = 0;
    while ((line = read_one_line(&fc)) != NULL) {
	if (num_lines == size_lines) {
	    size_lines += 10;
	    lines = REALLOC(lines,size_lines * sizeof(char *));
	}
	lines[num_lines] = line;
	++num_lines;
    }
    CloseHandle(infile);
    /* Now check the lines before doing anything else, so that 
       we don't need any error handling while creating the data
       structures */
    /* 
       The file should contain:
       [section]
       Key=Value
       ...
       [section]
       ...
    */
    i = 0;
    while (i < num_lines && is_section_header(lines[i])) {
	++i;
	while (i < num_lines && is_key_value(lines[i])) {
	    ++i;
	}
    }
    if (i < num_lines) {
	for (i = 0; i < num_lines; ++i) {
	    FREE(lines[i]);
	}
	FREE(lines);
	return NULL;
    }
    
    /* So, now we know it's consistent... */
    i = 0;
    inif = ALLOC(sizeof(InitFile));
    inif->num_sections = 0;
    inif->size_sections = 10;
    inif->sections = ALLOC(sizeof(InitSection *) * 10); 
    while (i < num_lines) {
	inis = ALLOC(sizeof(InitSection));
	inis->num_entries = 0;
	inis->size_entries = 10;
	inis->section_name = digout_section_name(lines[i]);
	inis->entries = ALLOC(sizeof(InitEntry *) * 10);
	++i;
	while (i < num_lines && is_key_value(lines[i])) {
	    inie = ALLOC(sizeof(InitEntry));
	    digout_key_value(lines[i], &(inie->key), &(inie->value));
	    if (inis->num_entries == inis->size_entries) {
		inis->size_entries += 10;
		inis->entries = 
		    REALLOC(inis->entries,
			    sizeof(InitEntry *) * inis->size_entries);
	    }
	    inis->entries[inis->num_entries] = inie;
	    ++(inis->num_entries);
	    ++i;
	}
	if (inif->num_sections == inif->size_sections) {
	    inif->size_sections += 10;
	    inif->sections = 
		REALLOC(inif->sections,
			sizeof(InitSection *) * inif->size_sections);
	}
	inif->sections[inif->num_sections] = inis;
	++(inif->num_sections);
    }
    FREE(lines); /* Only the array of strings, not the actual strings, they
		    are kept in the data structures. */
    return inif;
}

int store_init_file(InitFile *inif, wchar_t *filename)
{
    char *buff;
    int size = 10;
    int num = 0;
    int i,j;
    HANDLE outfile;

#define PUSH(Char) 				\
    do {					\
	if (num == size) {			\
	    size += 10;				\
	    buff = REALLOC(buff,size);		\
	}					\
	buff[num++] = (Char);			\
    } while(0)

    if ( (outfile = CreateFileW(filename, 
				GENERIC_WRITE,
				FILE_SHARE_WRITE,
				NULL,
				CREATE_ALWAYS,
				FILE_ATTRIBUTE_NORMAL,
				NULL)) == INVALID_HANDLE_VALUE) {
	return INIT_FILE_OPEN_ERROR;
    }
    buff = ALLOC(size);

    for(i = 0; i < inif->num_sections; ++i) {
	int len;
	int written;
	InitSection *inis = inif->sections[i];

	if (!WriteFile(outfile,"[",1,&written,NULL) || written != 1) {
	    goto write_error;
	}
	len = strlen(inis->section_name);
	if (!WriteFile(outfile,inis->section_name,len,&written,NULL) ||
	    written != len) {
	    goto write_error;
	}
	if (!WriteFile(outfile,"]\n",2,&written,NULL) || written != 2) {
	    goto write_error;
	}
	for (j = 0; j < inis->num_entries; ++j) {
	    InitEntry *inie = inis->entries[j];
	    char *p = inie->key;
	    num = 0;
	    for (;*p != '\0';++p) {
		switch (*p) {
		case '\\': 
		case ';': 
		    PUSH('\\');
		default:
		    PUSH(*p);
		    break;
		}
	    }
	    PUSH('=');
	    p = inie->value;
	    for (;*p != '\0';++p) {
		switch (*p) {
		case '\\': 
		case ';': 
		    PUSH('\\');
		default:
		    PUSH(*p);
		    break;
		}
	    }
	    PUSH('\n');
	    if (!WriteFile(outfile,buff,num,&written,NULL) || written != num) {
		goto write_error;
	    }
	}
    }
    FREE(buff);
    CloseHandle(outfile);
    return INIT_FILE_NO_ERROR;
 write_error:
    FREE(buff);
    CloseHandle(outfile);
    return INIT_FILE_WRITE_ERROR;
#undef PUSH
}

InitFile *create_init_file(void)
{
    InitFile *inif = ALLOC(sizeof(InitFile));
    inif->num_sections = 0;
    inif->size_sections = 10;
    inif->sections = ALLOC(sizeof(InitSection *) * 10);
    return inif;
}

InitSection *create_init_section(char *section_name)
{
    InitSection *inis = ALLOC(sizeof(InitSection));
    inis->num_entries = 0;
    inis->section_name = ALLOC(sizeof(char) * (strlen(section_name) + 1));
    strcpy(inis->section_name, section_name);
    inis->size_entries = 10;
    inis->entries = ALLOC(sizeof(InitEntry *) * 10);
    return inis;
}

static void free_init_entry(InitEntry *inie)
{
    FREE(inie->key);
    /* Value is part of the same buffer */
    FREE(inie);
}

void free_init_section(InitSection *inis)
{
    int i;
    for (i = 0;i < inis->num_entries; ++i) {
	free_init_entry(inis->entries[i]);
    }
    FREE(inis->entries);
    FREE(inis->section_name);
    FREE(inis);
}

void free_init_file(InitFile *inif)
{
    int i;
    for (i = 0; i < inif->num_sections; ++i) {
	free_init_section(inif->sections[i]);
    }
    FREE(inif->sections);
    FREE(inif);
}

static int find_init_section(InitFile *inif, char *section_name)
{
    int i;
    for (i = 0; i < inif->num_sections; ++i) {
	if (!strcmp(inif->sections[i]->section_name, section_name)) {
	    return i;
	}
    }
    return -1;
}

int delete_init_section(InitFile *inif, char *section_name)
{
    int i;

    if ((i = find_init_section(inif, section_name)) < 0) {
	return INIT_FILE_NOT_PRESENT;
    }

    free_init_section(inif->sections[i]);
    --(inif->num_sections);
    inif->sections[i] = inif->sections[inif->num_sections];

    return INIT_FILE_PRESENT;
}

int add_init_section(InitFile *inif, InitSection *inis)
{
    int i;
    InitSection *oinis;
    if ((i = find_init_section(inif, inis->section_name)) >= 0) {
	oinis = inif->sections[i];
	inif->sections[i] = inis;
	free_init_section(oinis);
	return INIT_FILE_PRESENT;
    } 
    if (inif->num_sections == inif->size_sections) {
	inif->size_sections += 10;
	inif->sections = REALLOC(inif->sections, 
				 sizeof(InitSection *) * inif->size_sections);
    }
    inif->sections[inif->num_sections] = inis;
    ++(inif->num_sections);
    return INIT_FILE_NOT_PRESENT;
}

InitSection *lookup_init_section(InitFile *inif, char *section_name)
{
    int i;
    if ((i = find_init_section(inif,section_name)) < 0) {
	return NULL;
    }
    return inif->sections[i];
}

char *nth_init_section_name(InitFile *inif, int n)
{
    if (n >= inif->num_sections) {
	return NULL;
    }
    return inif->sections[n]->section_name;
}

/* Inefficient... */
InitSection *copy_init_section(InitSection *inis, char *new_name) 
{
    int i;
    char *key;
    InitSection *ninis = create_init_section(new_name);
    i = 0;
    while ((key = nth_init_entry_key(inis,i)) != NULL) {
	add_init_entry(ninis, key, lookup_init_entry(inis, key));
	++i;
    }
    return ninis;
}

static int find_init_entry(InitSection *inis, char *key)
{
    int i;
    for (i = 0; i < inis->num_entries; ++i) {
	if (!strcmp(inis->entries[i]->key,key)) {
	    return i;
	}
    }
    return -1;
}

int add_init_entry(InitSection *inis, char *key, char *value)
{
    int keylen = strlen(key);
    char *buff = ALLOC(sizeof(char) * (keylen + strlen(value) + 2));
    InitEntry *inie;
    char *obuff;
    int i;

    strcpy(buff,key);
    strcpy(buff+keylen+1,value);

    if ((i = find_init_entry(inis,key)) >= 0) {
	inie = inis->entries[i];
	FREE(inie->key);
	inie->key = buff;
	inie->value = buff+keylen+1;
	return INIT_FILE_PRESENT;
    }
    inie = ALLOC(sizeof(InitEntry));
    inie->key = buff;
    inie->value = buff+keylen+1;
    if (inis->num_entries == inis->size_entries) {
	inis->size_entries += 10;
	inis->entries = REALLOC(inis->entries, 
				sizeof(InitEntry *) * inis->size_entries);
    }
    inis->entries[inis->num_entries] = inie;
    ++(inis->num_entries);
    return INIT_FILE_NOT_PRESENT;
}

char *lookup_init_entry(InitSection *inis, char *key)
{
    int i;
    if ((i = find_init_entry(inis,key)) < 0) {
	return NULL;
    }
    return inis->entries[i]->value;
}

char *nth_init_entry_key(InitSection *inis, int n)
{
    if (n >= inis->num_entries) {
	return NULL;
    }
    return inis->entries[n]->key;
}

int delete_init_entry(InitSection *inis, char *key)
{
    int i;
    InitEntry *inie;
    if ((i = find_init_entry(inis, key)) < 0) {
	return INIT_FILE_NOT_PRESENT;
    }
    free_init_entry(inis->entries[i]);
    --(inis->num_entries);
    inis->entries[i] = inis->entries[inis->num_entries];
    return INIT_FILE_PRESENT;
}

