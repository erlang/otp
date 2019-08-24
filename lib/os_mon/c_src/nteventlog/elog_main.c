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
/*
 * Module: elog_main
 * Purpouse: Main program and logic in the nteventlog program
 * which waits for logging events and sends them to erlang.
 * At startup, the registry keys for the identifier given on the
 * command line are read to see what log record was the last 
 * displayed for this identifier. All "new" records are then 
 * displayed. After that the program waits for new events to arrive 
 * and displays them (sends them to erlang). If stdin is a pipe
 * (not a console) an ACK is requested for every entry displayed.
 * when the ACK is received, the registry entries for the identifier
 * is updated, so that next time the program starts, only the new
 * entries are displayed.
 */
#include "elog_global.h"
#include "elog_util.h"
#include "elog_pipe_stdin.h"
#include "elog_format.h"
#include "elog_registry.h"

/*
 * A race condition in the event log notification and the 
 * event log reading results in us having to retry reading the
 * eventlog after some time. One second seems to do it...
 */
#define RETRY_TIMEOUT 1000

/*
 * Constants for the logging formats.
 */
#define LOG_FORMAT "%s [%s] %s, %s: %s\n" 
#define PIPE_LOG_FORMAT "%dH%s%dH%s%dH%s%dH%s%dH%s"
#define PIPE_LOG_ACK "A"
#define PIPE_LOG_EXTRA (5*10) /* 5 int */
#define ACK_MAX 100
#define TIME_FORMAT "%d-%b-%Y %H:%M:%S GMT"
/*                    2  3  4  2  2  2     */
#define TIME_BUFSIZ 25
#define CANT_FORMAT_MESSAGE "[EventLogReader unable to format message text]"

/* the default registry identification */
#define DEFAULT_IDENT "DefaultIdent"

/*#define HARDDEBUG*/

/* Flag set if eventlog is purged and need reopening.*/
static int reopen_event_log = 0;

/*
 * Calculates the needed buffersize for a record in the eventlog.
 * if recordnum == 0, looks at next record, otherwise looks
 * at the record with the given number.
 */
static DWORD needed(HANDLE elog, DWORD recordnum){
  EVENTLOGRECORD dummy;
  DWORD red, need;
  DWORD last_error;
  DWORD flags = 
    EVENTLOG_FORWARDS_READ | 
    ((recordnum) ? EVENTLOG_SEEK_READ : EVENTLOG_SEQUENTIAL_READ);
#ifdef HARDDEBUG
  fprintf(stderr,"Calculating need when recordnum = %lu\n",recordnum);
#endif
  if(!ReadEventLog(elog,
		   flags,
		   recordnum,
		   &dummy,
		   (DWORD) sizeof(EVENTLOGRECORD),
		   &red,
		   &need) &&
     (last_error = GetLastError()) == ERROR_INSUFFICIENT_BUFFER){
    return need;
  } else if(last_error == ERROR_EVENTLOG_FILE_CHANGED){
    reopen_event_log = 1;
    return (DWORD) 0;
  } else {
#ifdef HARDDEBUG
    output_error(last_error,"needed() failed to read eventlog");
#endif
    return (DWORD) 0;
  }
}  


/*
 * Checks (any type of) stdin for end of file.
 * Expects data present on stdin.
 */
BOOL eof_on_stdin(void){
  HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
  char x[1];
  DWORD y;
  INPUT_RECORD input;
  DWORD red;
  static int state = 1; /* Return pressed = 1, ^Z after that = 2 */

  if(!console_stdin()){
    return peek_pipe_stdin_eof();
  }
  /* Console input, may be just about every type of event, look for
     ^Z pressed... */
  if(!ReadConsoleInput(in,&input,1,&red))
    return FALSE;
  if(input.EventType != KEY_EVENT)
    return FALSE;
  if(input.Event.KeyEvent.bKeyDown){
    switch(input.Event.KeyEvent.uChar.AsciiChar){
    case 0:
      break;
    case 13:
      x[0] = '\r';
      WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),"\r\n",2,&y,NULL);
      if(state == 2) 
	return TRUE;
      else
	state = 1;
      break;
    case 26:
      if(state == 1)
	state = 2;
      WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),"^Z",2,&y,NULL);
      break;
    default:
      if(((unsigned char) input.Event.KeyEvent.uChar.AsciiChar) < ' '){
	WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),"^",1,&y,NULL);
	x[0] = input.Event.KeyEvent.uChar.AsciiChar + '@';
      } else 
	x[0] = input.Event.KeyEvent.uChar.AsciiChar;
      WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),x,1,&y,NULL);
      state = 0;
      break;
    }
    return FALSE;
  }
  return FALSE;
}

/*
 * Writes eventlog entries to erlang and requires ACK for
 * each record.
 */
BOOL data_to_pipe(char *string, char *answer, int answer_siz){
  unsigned char len[2];
  unsigned char *ptr;
  int siz = strlen(string);
  HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
  HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
  DWORD written, red;
  DWORD left;

  len[0] = (siz >> 8) & 0xFF;
  len[1] = siz & 0xFF;
  if(!WriteFile(out, len, 2, 
	    &written, NULL) || written != 2)
    return FALSE;
  if(!WriteFile(out, string, siz, &written, NULL) ||
     written != (DWORD) siz)
    return FALSE;
  /* Read ACK from erlang */
  left = 2;
  ptr = len;
  for(;;){
    if(!(red = read_pipe_stdin(ptr, left)))
      return FALSE;
    else if(red < left){
      ptr += red;
      left -= red;
    } else {
      break;
    }
  }
  siz = len[0] << 8 | len[1];

  if(siz >= answer_siz - 1)
    return FALSE;

  left = siz;
  ptr = (unsigned char *) answer;
  for(;;){
    if(!(red = read_pipe_stdin(ptr, left))){
      return FALSE;
    } else if(red < left){
      ptr += red;
      left -= red;
    } else {
      break;
    }
  }
  answer[siz] = '\0';
  return TRUE;
}

/*
 * The actual writing of records.
 * Behaves differently if stdout is a pipe (erlang)
 * or a console (test run).
 */
  
BOOL output_record(char *category, EVENTLOGRECORD *event){
  char *strbeg, *fac, *sev;
  char eventfilename[MAX_PATH];
  char paramfilename[MAX_PATH];
  char bigbuff[BIGBUFSIZ];
  MessageFiles mf;
  char tbuff[TIME_BUFSIZ];
  BOOL ret;
  DWORD written;
  char *buff;
  char ackbuff[ACK_MAX];

  mf = get_messagefiles(category,((char *)event)+sizeof(EVENTLOGRECORD),
			eventfilename, MAX_PATH,
			paramfilename, MAX_PATH);
  if(!mf.event){
    strcpy(bigbuff, CANT_FORMAT_MESSAGE);
  } else {
    strbeg = (char *) event;
    strbeg += event->StringOffset;
    if(!format_message(mf, event->EventID,
		       strbeg, event->NumStrings, bigbuff, BIGBUFSIZ)){
      strcpy(bigbuff, CANT_FORMAT_MESSAGE);
    }
  }
  fac = ((char *)event)+sizeof(EVENTLOGRECORD);
  sev = lookup_severity(event->EventType);
  if(console_stdin()){
    *tbuff = '\0';
    strftime(tbuff, (size_t) TIME_BUFSIZ, TIME_FORMAT, 
	     gmtime((time_t *)&(event->TimeGenerated)));
    buff = 
      malloc(strlen(bigbuff) + TIME_BUFSIZ /* date-time */ + 
	     strlen(category) +
	     strlen(fac) /* facility */   +
	     strlen(sev) /*severity */+
	     strlen(LOG_FORMAT) /* More than actually needed */ + 1);
    sprintf(buff, LOG_FORMAT, tbuff, fac, category, sev, bigbuff); 
    ret = WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), buff, strlen(buff), 
		    &written, NULL); /* Overlapped structure for stdout? */
  free(buff);
  } else { /* pipe */
    sprintf(tbuff,"%lu", event->TimeGenerated); 
    buff = malloc(strlen(bigbuff) + 
		  strlen(tbuff) + 
		  strlen(category) +
		  strlen(fac) +
		  strlen(sev) +
		  strlen(PIPE_LOG_FORMAT) +
		  PIPE_LOG_EXTRA);
    sprintf(buff,PIPE_LOG_FORMAT,
	    strlen(tbuff),tbuff,
	    strlen(category), category,
	    strlen(fac), fac,
	    strlen(sev), sev,
	    strlen(bigbuff), bigbuff);
    ret = data_to_pipe(buff,ackbuff, ACK_MAX);
    if(ret && strcmp(ackbuff,PIPE_LOG_ACK))
      ret = FALSE;
    free(buff);
  }
  return ret;
}


/*
 * Read eventlog entries FOLLOWING the given record
 * number and timestamp, and sends them to
 * stdout. If timestamp does
 * not correspond with record number, the
 * log is concidered wrapped around
 * and is reread from the beginning.
 * time is ignored if 0.
 * If record_number is 0, the whole log is read (if there is one).
 * If the function is unsuccessful, the global variable 
 * reopen_event_log may be set to TRUE, which means 
 * that the eventlog has changed and has to be reopened.
 * It's the callers responsibility to set reopen_event_log to
 * 0 before calling.
 */ 
static int read_from(DWORD *record_number, DWORD *time, 
		     HANDLE elog, char *category){
  DWORD dummy;
  static EVENTLOGRECORD *event = NULL;
  static DWORD eventsiz = 0;
  DWORD red, need;
  /* Always begin reading from record 1, record 0 never exists */
  DWORD tmp = (*record_number) ? *record_number : 1; 
  DWORD ttmp = *time;
  int skip = !!(*record_number); /* Dont skip if record_number == 0 */
  int maybe_done = 0;
  int i;

  /* First try seeking to the correct place in the eventlog */
  /* the variable skip tells us that we are rereading the */ 
  /* last read eventlog record and the variable */
  /* maybe_done tells us that we have read the last eventlog */
  /* and an EOF is OK. */
  for(;;){
    if(!(need = needed(elog, tmp))){
      if(maybe_done) /* Has read one correct record
			and are satisfied with that. */
	return 0;
      /* Hmm, could not find the record? Try the oldest... */
      if(!GetOldestEventLogRecord(elog, &tmp) ||
	 !(need = needed(elog, tmp))){
	/* Something's terribly wrong. */
#ifdef HARDDEBUG
	fprintf(stderr,"Could not get oldest eventlog record!\n",
		need);
#endif
	return -1;
      }
      skip = 0;
    }
    /* need == number of bytes for this record, 
       tmp == this recordnumber */
    if(!event)
      event = malloc(eventsiz = need);
    else if(eventsiz < need)
      event = realloc(event, eventsiz = need);
    if(!ReadEventLog(elog,
		     (DWORD) (EVENTLOG_FORWARDS_READ | 
		     EVENTLOG_SEEK_READ),
		     tmp,
		     event,
		     need,
		     &red,
		     &dummy)){
      if(GetLastError() == ERROR_EVENTLOG_FILE_CHANGED){
	reopen_event_log = 1;
      } 
#ifdef HARDDEBUG
      output_error(GetLastError(), 
		   "Failed first eventlog read in read_from\n");
#endif
      return -1;
    }
    if(skip){
      if(ttmp && event->TimeWritten != ttmp){
	/* Wrapped around eventlog */
	tmp = 1;
      } else {
	maybe_done = 1;
	++tmp;
      }
      skip = 0;
    } else
      break;
  }
  /* Got the first record in buffer, display and continue reading */
  /* sequentially from here. */
  for(i=1;;++i){
    if(!output_record(category, event))
      return -1;
    *record_number = event->RecordNumber;
    *time = event->TimeWritten;
    if(!(need = needed(elog,(DWORD) 0)))
      break; /* End of log */
    if(eventsiz < need)
      event = realloc(event, eventsiz = need);
    if(!ReadEventLog(elog,
		     (DWORD) EVENTLOG_FORWARDS_READ | 
		     EVENTLOG_SEQUENTIAL_READ,
		     (DWORD) 0,
		     event,
		     need,
		     &red,
		     &dummy)){
      if(GetLastError() == ERROR_EVENTLOG_FILE_CHANGED){
	reopen_event_log = 1;
      } 
      return -1;
    }
  }
  return i;
}

/*
 * Read unread events and wait for new to arrive.
 */
int main(int argc, char **argv){
  HANDLE elog[NUM_CATEGORIES]; 
  HANDLE events[NUM_CATEGORIES + 1]; /* The stdin handle goes 
				      in here to */
  int eventlen;
  char *ident;
  DWORD record = 0, time = 0; 
  RegKeys rk[NUM_CATEGORIES];
  int rklen = NUM_CATEGORIES;
  int i;
  int ret;
  int x = 0;
  int retry = 0;

  if(argc < 2){
    ident = DEFAULT_IDENT;
  } else {
    ident = argv[1];
  }

  if(!setup_pipe_stdin()){
    fprintf(stderr,"%s: Stdin could not be initialized.\n",argv[0]);
    return 1;
  }
  if(get_regkeys(ident, rk, &rklen) != 0){
    fprintf(stderr, 
	    "%s: Could not get/create registry parameters.\n", argv[0]);
    return 1;
  }
  for(;;){
    for(i=0; i<rklen; ++i){
      elog[i] = OpenEventLog(NULL, rk[i].facility_name);
      
      if(elog[i] == NULL){
	fprintf(stderr, 
		"%s: Could not open \"%s\" eventlog.\n",
		argv[0], rk[i].facility_name);
	return 1;
      }
      if((events[i] = CreateEvent(NULL,FALSE,FALSE,NULL)) 
	 == NULL){
	fprintf(stderr,"%s: Could not create event object.\n", argv[0]);
	return 1;
      }
      if(!NotifyChangeEventLog(elog[i],events[i])){
	fprintf(stderr,"%s: Could not get eventlog notification.\n", argv[0]);
	return 1;
      }
      if(read_from(&(rk[i].latest_record), 
		   &(rk[i].latest_time),
		   elog[i],
		   rk[i].facility_name) > 0)
	set_regkeys(ident, rk + i, 1); 
    }
    eventlen = rklen;
    events[eventlen] = get_stdin_event();
    ++eventlen;
  
    for(;;){  
#ifdef HARDDEBUG
      fprintf(stderr,"Entering Wait...\n");
#endif
      reopen_event_log = 0;
      ret = WaitForMultipleObjects(eventlen,
				   events,
				   FALSE,
				   (retry) ? 
				   RETRY_TIMEOUT : 
				   INFINITE);
#ifdef HARDDEBUG
      fprintf(stderr,"Wait returned!\n");
#endif
      if(ret == WAIT_TIMEOUT){ 
	if(!retry){
	  fprintf(stderr,"%s: Timeout when no such possible!\n", 
		  argv[0]);
	  return 1;
	}
	retry = 0;
      } else {
	if(((int) (ret - WAIT_OBJECT_0)) >= rklen && eof_on_stdin())
	  goto done;
	retry = 1;
      }
      for(i=0;i<rklen;++i){
	if(read_from(&(rk[i].latest_record), 
		     &(rk[i].latest_time),
		     elog[i],
		     rk[i].facility_name) > 0)
	  set_regkeys(ident, rk + i, 1);
      }
      if(reopen_event_log)
	break;
    }
    for(i=0; i < rklen; ++i){
      CloseEventLog(elog[i]);
      CloseHandle(events[i]);
    }
  }
done:
#ifdef DEBUG
  fprintf(stderr,"%s: EOF\n", argv[0]);
#endif
  for(i=0; i < rklen; ++i)
    CloseEventLog(elog[i]);
  return 0;
}
