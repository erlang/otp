#include <stdlib.h>
#include <stdio.h>

#if defined __WIN32__
#include <windows.h> 
#include <fcntl.h>
#endif

static int read_exact(char *buffer, int len);
static int write_exact(char *buffer, int len);

int main(void)
{
    char msg[100];
    int msg_len;
#ifdef __WIN32__
    _setmode(_fileno( stdin),  _O_BINARY);
    _setmode(_fileno( stdout), _O_BINARY);
#endif    
    msg_len = read_exact(msg, 100);

    write_exact("Content-type: text/plain\r\n\r\n", 28);
    write_exact(msg, msg_len);
    exit(EXIT_SUCCESS);
}
  

/* read from stdin */ 
#ifdef __WIN32__
static int read_exact(char *buffer, int len)
{
    HANDLE standard_input = GetStdHandle(STD_INPUT_HANDLE);
    
    unsigned read_result;
    unsigned sofar = 0;
    
    if (!len) { /* Happens for "empty packages */
	return 0;
    }
    for (;;) {
	if (!ReadFile(standard_input, buffer + sofar,
		      len - sofar, &read_result, NULL)) {
	    return -1; /* EOF */
	}
	if (!read_result) {
	    return -2; /* Interrupted while reading? */
	}
	sofar += read_result;
	if (sofar == len) {
	    return len;
	}
    }
} 
#else
static int read_exact(char *buffer, int len) {
    int i, got = 0;
    
    do {
	if ((i = read(0, buffer + got, len - got)) <= 0)
	    return(i);
	got += i;
    } while (got < len);
    return len;
   
}
#endif

/* write to stdout */
#ifdef __WIN32__
 static int write_exact(char *buffer, int len)
   {
     HANDLE standard_output = GetStdHandle(STD_OUTPUT_HANDLE);
     unsigned written;

     if (!WriteFile(standard_output, buffer, len, &written, NULL)) {
       return -1; /* Broken Pipe */
     }
     if (written < ((unsigned) len)) {
       /* This should not happen, standard output is not blocking? */
       return -2;
     }

    return (int) written;
}

#else 
 static int write_exact(char *buffer, int len) {
   int i, wrote = 0;

   do {
     if ((i = write(1, buffer + wrote, len - wrote)) <= 0)
       return i;
     wrote += i;
   } while (wrote < len);
   return len;
 }
#endif
