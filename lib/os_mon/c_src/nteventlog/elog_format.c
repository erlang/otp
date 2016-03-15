/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#include "elog_global.h"
#include "elog_format.h"

/*
 * The Newline treatment bits of FormatMessage
 * This value should suppress all other than hardcoded newlines
 */
#define NL_TREATMENT FORMAT_MESSAGE_MAX_WIDTH_MASK

/*
 * Expands %%NNN formats in strings with strings from a
 * ParameterMessageFile (open).
 * A return of NULL means there's nothing to expand
 * or that the buffer is to small, which probably means the 
 * same thing to the caller, that is use the
 * original string as it is.
 */
static char *expand_message(char *toexpand, 
			    HINSTANCE paramlib,
			    char *buff, int bufflen){
  char *oldpos;
  int buffpos = 0;
  char *pos = toexpand;
  char *end;
  unsigned long num;
  char *replbuff = malloc(bufflen);
  char *repl;
  int replaced = 0;

  while((oldpos = pos, pos = strstr(pos,"%%"))){
    num = strtoul(pos + 2, &end, 0);
    replaced = 1;
    if(end == pos + 2 || num == 0){
      repl = "%%";
    } else {
      if(!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE | 
			FORMAT_MESSAGE_IGNORE_INSERTS | 
			NL_TREATMENT,
			(LPCVOID) paramlib,
			(DWORD) num,
			DEFAULT_LANGID,
			replbuff,
			(DWORD) bufflen,
			NULL)){
	repl = ""; /* this is how the event logger treats it... */
      } else {
	repl = replbuff;
      }
    }
    if((int)(buffpos + strlen(repl) + (pos - oldpos) + 1) > bufflen){
      free(replbuff);
      return NULL;
    }
    strncpy(&(buff[buffpos]),oldpos, pos - oldpos);
    buffpos += pos - oldpos;
    strcpy(&(buff[buffpos]), repl);
    buffpos += strlen(repl);
    pos = end;
  }
  free(replbuff);
  if(!replaced)
    return NULL;
  if((int) (buffpos + strlen(oldpos) + 1) > bufflen)
    return NULL;
  strcpy(&(buff[buffpos]),oldpos);
  return buff;
}

/* 
 * A lot to free when returning from format_message, lets make it easier
 */
static char *fm_free_up(char **argv, char *tmpbuff, 
			char * tmpbuff2,
			HINSTANCE elibrary, 
			HINSTANCE plibrary){
  if(plibrary != NULL){
    FreeLibrary(plibrary);
    while(*argv)
      free(*argv++);
  }
  free(tmpbuff);
  free(tmpbuff2);
  if(elibrary != NULL)
    FreeLibrary(elibrary);
  return NULL;
}

#define FM_RETURN(X) \
return (fm_free_up(argv, tmpbuff, tmpbuff2, elibrary, plibrary), (X))

/*
 * Formats an eventlog message into a string buffer.
 * Returns NULL if message could not be formatted (buffer to small or 
 * library error). 
 */
char *format_message(MessageFiles mf, DWORD id, 
		     char *strings, int numstrings, 
		     char *buff, int bufflen){
  char *argv[MAX_PARAM_STRINGS];
  int argc,i;
  HINSTANCE elibrary = NULL;
  HINSTANCE plibrary = NULL;
  char *tmpbuff = malloc(bufflen);
  char *tmpbuff2 = malloc(bufflen);

  for(argc=0;argc < numstrings && argc < MAX_PARAM_STRINGS - 1; ++argc)
    argv[argc] = 
      (argc) ? argv[argc - 1] + strlen(argv[argc - 1]) + 1 : strings;

  argv[argc] = NULL; 
  
  if((elibrary = LoadLibraryEx(mf.event, NULL, DONT_RESOLVE_DLL_REFERENCES)) 
     == NULL)
    FM_RETURN(NULL);

  if(!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE | 
		    FORMAT_MESSAGE_IGNORE_INSERTS | NL_TREATMENT,
		    (LPCVOID) elibrary,
		    id,
		    DEFAULT_LANGID,
		    tmpbuff2,
		    (DWORD) bufflen,
		    NULL)){
    FM_RETURN(NULL);
  }

  if(mf.param != NULL)
    plibrary = LoadLibraryEx(mf.param, NULL, DONT_RESOLVE_DLL_REFERENCES);

  if(plibrary){
    for(i=0;argv[i];++i)
      if(expand_message(argv[i], plibrary, tmpbuff, bufflen) != NULL)
	argv[i] = strdup(tmpbuff);
      else 
	argv[i] = strdup(argv[i]); /* All gets malloced, so I don't have to
				      bother what to free... */ 
    if(expand_message(tmpbuff2, plibrary, tmpbuff, bufflen) != NULL)
      strcpy(tmpbuff2,tmpbuff);
  }

  if(!FormatMessage(FORMAT_MESSAGE_FROM_STRING | 
		    FORMAT_MESSAGE_ARGUMENT_ARRAY | NL_TREATMENT,
		    (LPCVOID) tmpbuff2,
		    id,
		    DEFAULT_LANGID,
		    buff,
		    (DWORD) bufflen,
		    argv)){
    FM_RETURN(NULL);
  }

  FM_RETURN(buff);

}
#undef FM_RETURN
