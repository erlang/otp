/*
 * Author:  Bjorn Gustavsson
 * Purpose: A port program to be used for testing the open_port bif.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>

#ifndef __WIN32__
#include <unistd.h>
#include <limits.h>

#include <sys/time.h>

#define O_BINARY 0
#define _setmode(fd, mode)
#endif

#ifdef __WIN32__
#include "windows.h"
#include "winbase.h"
#endif


#define REDIR_STDOUT(fd) if (dup2(fd, 1) == -1) { \
    fprintf(stderr, "%s: failed to duplicate handle %d to 1: %d\n", \
    port_data->progname, fd, errno); \
    exit(1); \
}

#define ASSERT(e) ((void) ((e) ? 1 : abort()))

extern int errno;

typedef struct {
    char* progname;	        /* Name of this program (from argv[0]). */
    int header_size;	        /* Number of bytes in each packet header:
				 * 1, 2, or 4, or 0 for a continuous byte stream. */
    int fd_from_erl;	        /* File descriptor from Erlang. */
    int fd_to_erl;	        /* File descriptor to Erlang. */
    unsigned char* io_buf;      /* Buffer for file i/o. */
    int io_buf_size;		/* Current size of i/o buffer. */
    int delay_mode;		/* If set, this program will wait 5 seconds
				 * after reading the header for a packet
				 * before reading the rest.
				 */
    int fd_count;               /* Count the number of open fds */
    int break_mode;		/* If set, this program will close standard
				 * input, which should case broken pipe
				 * error in the writer.
				 */
    int quit_mode;		/* If set, this program will exit
				 * just after reading the packet header.
				 */
    int slow_writes;		/* Writes back the reply in chunks with
				 * sleeps in between.  The value is the
				 * chunk size.  If 0, normal writes are done.
				 */
    char* output_file;          /* File into which the result will be written. */
    int no_packet_loop;	        /* No packet loop. */

    int limited_bytecount;      /* Only answer a limited number of bytes, then exit (stream mode) */

} PORT_TEST_DATA;

PORT_TEST_DATA* port_data;

static int packet_loop();
static void reply();
static void write_reply();
static void ensure_buf_big_enough();
static int readn();
static void delay(unsigned ms);
static void dump(unsigned char* buf, int sz, int max);
static void replace_stdout(char* filename);
static void generate_reply(char* spec);

#ifndef HAVE_STRERROR
extern int sys_nerr;
#ifndef sys_errlist /* sys_errlist is sometimes defined to
		       call a function on win32 */
extern char *sys_errlist[];
#endif

char*
strerror(err)
int err;
{
    static char msgstr[1024];

    if (err == 0) {
	msgstr[0] = '\0';
    } else if (0 < err && err < sys_nerr) {
	strcpy(msgstr, sys_errlist[err]);
    } else {
	sprintf(msgstr, "Unknown error %d", err);
    }
    return msgstr;
}
#endif


int main(int argc, char *argv[])
{
  int ret, fd_count;
  if((port_data = (PORT_TEST_DATA *) malloc(sizeof(PORT_TEST_DATA))) == NULL) {
    fprintf(stderr, "Couldn't malloc for port_data");
    exit(1);
  }
  port_data->header_size = 0;
  port_data->io_buf_size = 0;	
  port_data->delay_mode = 0;	
  port_data->fd_count = 0;
  port_data->break_mode = 0;	
  port_data->quit_mode = 0;	
  port_data->slow_writes = 0;	
  port_data->output_file = NULL;
  port_data->no_packet_loop = 0;

  port_data->progname = argv[0];
  port_data->fd_from_erl = 0;
  port_data->fd_to_erl = 1;

  port_data->limited_bytecount = 0;

  _setmode(0, _O_BINARY);
  _setmode(1, _O_BINARY);

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'b':			/* Break mode. */
      port_data->break_mode = 1;
      break;
    case 'c':			/* Close standard output. */
      close(port_data->fd_to_erl);
      break;
    case 'd':			/* Delay mode. */
      port_data->delay_mode = 1;
      break;
    case 'e': 
      port_data->fd_to_erl = 2;
      break;
    case 'f':
      port_data->fd_count = 1;
      break;
    case 'h':			/* Header size for packets. */
      switch (argv[1][2]) {
      case '0': port_data->header_size = 0; break;
      case '1': port_data->header_size = 1; break;
      case '2': port_data->header_size = 2; break;
      case '4': port_data->header_size = 4; break;
      case '\0': 
	fprintf(stderr, "%s: missing header size for -h\n", port_data->progname);
	return 1;
      default:
	fprintf(stderr, "%s: illegal packet header size: %c\n", 
		port_data->progname, argv[1][2]);
	return 1;
      }
      break;
    case 'l':
      port_data->limited_bytecount = atoi(argv[1]+2);
      break;
    case 'n':			/* No packet loop. */
      port_data->no_packet_loop = 1;
      break;
    case 'o':			/* Output to file. */
      port_data->output_file = argv[1]+2;
      break;
    case 'q':			/* Quit mode. */
      port_data->quit_mode = 1;
      break;
    case 'r':			/* Generate reply. */
      generate_reply(argv[1]+2);
      break;
    case 's':			/* Slow writes. */
      port_data->slow_writes = atoi(argv[1]+2);
      break;
    default:
      fprintf(stderr, "Unrecognized switch: %s\n", argv[1]);
      free(port_data);
      exit(1);
    }
    argc--, argv++;
  }

  if (argc > 1) {
    /* XXX Add error printout here */
  }

  if (port_data->fd_count) {
#ifdef __WIN32__
      DWORD handles;
      GetProcessHandleCount(GetCurrentProcess(), &handles);
      fd_count = handles;
#else
      int i;
      for (i = 0, fd_count = 0; i < 1024; i++)
          if (fcntl(i, F_GETFD) >= 0) {
              fd_count++;
          }
#endif
  }

  if (port_data->output_file)
      replace_stdout(port_data->output_file);

  if (port_data->fd_count)
      reply(&fd_count, sizeof(fd_count));

  if (port_data->no_packet_loop){
      free(port_data);
      exit(0);
  }

  ret = packet_loop();
  if(port_data->io_buf_size > 0)
      free(port_data->io_buf);
  free(port_data);
  return ret;
}

static int
packet_loop(void)
{
  int total_read = 0;
  port_data->io_buf = (unsigned char*) malloc(1);		/* Allocate once, so realloc works (SunOS) */
  

  for (;;) {
    int packet_length;		/* Length of current packet. */
    int i;
    int bytes_read;		/* Number of bytes read. */

    /*
     * Read the packet header, if any.
     */

    if (port_data->header_size == 0) {
      if(port_data->limited_bytecount &&
	 port_data->limited_bytecount - total_read < 4096)
	packet_length = port_data->limited_bytecount - total_read;
      else
	packet_length = 4096;
    } else {
      ensure_buf_big_enough(port_data->header_size);
      if (readn(port_data->fd_from_erl, port_data->io_buf, port_data->header_size) != port_data->header_size) {
	return(1);
      }

      /*
       * Get the length of this packet.
       */

      packet_length = 0;
      for (i = 0; i < port_data->header_size; i++)
	packet_length = (packet_length << 8) | port_data->io_buf[i];
    }


    /*
     * Delay if delay mode.
     */

    if (port_data->delay_mode) {
      delay(5000L);
    }

    if (port_data->quit_mode) {
      return(1);
    } else if (port_data->break_mode) {
      close(0);
      delay(32000L);
      return(1);
    }

    /*
     * Read the packet itself.
     */

    ensure_buf_big_enough(packet_length+4+1); /* At least five bytes. */
    port_data->io_buf[4] = '\0';
    if (port_data->header_size == 0) {
      bytes_read = read(port_data->fd_from_erl, port_data->io_buf+4, packet_length);
      if (bytes_read == 0)
	return(1);
      if (bytes_read < 0) {
	fprintf(stderr, "Error reading %d bytes: %s\n",
		packet_length, strerror(errno));
	return(1);
      }
      total_read += bytes_read;
    } else {
      bytes_read = readn(port_data->fd_from_erl, port_data->io_buf+4, packet_length);
      if (bytes_read != packet_length) {
	fprintf(stderr, "%s: couldn't read packet of length %d\r\n",
		port_data->progname, packet_length);
	return(1);
      }
    }
    
    /*
     * Act on the command.
     */
    if (port_data->header_size == 0) {
      reply(port_data->io_buf+4, bytes_read);
      if(port_data->limited_bytecount && 
	 port_data->limited_bytecount <= total_read){
	delay(5000L);
	return(0); 
      }
    } else {
      switch (port_data->io_buf[4]) {
      case 'p':			/* ping */
	port_data->io_buf[4] = 'P';
	reply(port_data->io_buf+4, bytes_read);
	break;
      case 'e':			/* echo */
	reply(port_data->io_buf+4, bytes_read);
	break;
      case 'x':			/* exit */
	return(5);
	break;
      default:
	fprintf(stderr, "%s: bad packet of length %d received: ",
		port_data->progname, bytes_read);
	dump(port_data->io_buf+4, bytes_read, 10);
	fprintf(stderr, "\r\n");
	return(1);
      }
    }
  }
}

/*
 * Sends a packet back to Erlang.
 */

static void
reply(buf, size)
     char* buf;			/* Buffer with reply. The four bytes before
				 * this pointer must be allocated so that
				 * this function can put the header there.
				 */
     int size;			/* Size of buffer to send. */
{
    int n;			/* Temporary to hold size. */
    int i;			/* Loop counter. */

    /*
     * Fill the header starting with the least significant byte
     * (this will work even if there is no header).
     */

    n = size;
    for (i = 0; i < port_data->header_size; i++) {
	*--buf = (char) n;	/* Store least significant byte. */
	n = n >> 8;
    }

    size += port_data->header_size;
    write_reply(buf, size);
}



static void
write_reply(buf, size)
     char* buf;			/* Buffer with reply.  Must contain header. */
     int size;			/* Size of buffer to send. */
{
    int n;			/* Temporary to hold size. */
    int rv;

    if (port_data->slow_writes <= 0) {	/* Normal, "fast", write. */
	rv = write(port_data->fd_to_erl, buf, size);
        ASSERT(rv == size);
    } else {
	/*
	 * Write chunks with delays in between.
	 */

	while (size > 0) {
	    n = size > port_data->slow_writes ? port_data->slow_writes : size;
	    rv = write(port_data->fd_to_erl, buf, n);
            ASSERT(rv == n);
	    size -= n;
	    buf += n;
	    if (size)
		delay(500L);
	}
    }
}


/*
 * Ensures that our I/O buffer is big enough for the packet to come.
 */

static void
ensure_buf_big_enough(size)
     int size;		/* Needed size of buffer. */
{
  if (port_data->io_buf_size >= size)
    return;

  port_data->io_buf = (unsigned char*) realloc(port_data->io_buf, size);
  if (port_data->io_buf == NULL) {
    fprintf(stderr, "%s: insufficient memory for i/o buffer of size %d\n",
	    port_data->progname, size);
    exit(1);
  }
  port_data->io_buf_size = size;
}

/*
 * Reads len number of bytes.
 */
static int
readn(fd, buf, len)
     int fd;			/* File descriptor to read from. */
     unsigned char *buf;	/* Store in this buffer. */
     int len;			/* Number of bytes to read. */
{
    int n;			/* Byte count in last read call. */
    int sofar;		        /* Bytes read so far. */

    sofar = 0;
    do {
	if ((n = read(fd, buf+sofar, len-sofar)) <= 0)
	    /* error or EOF in read */
	    return(n);
	sofar += n;
    } while (sofar < len);
    return sofar;
}

static void
replace_stdout(filename)
char* filename;			/* Name of file to replace standard output. */
{
  int fd;

  fd = open(filename, O_CREAT|O_TRUNC|O_WRONLY|O_BINARY, 0666);
  if (fd == -1) {
    fprintf(stderr, "%s: failed to open %s for writing: %d\n",
	    port_data->progname, filename, errno);
    exit(1);
  }
  REDIR_STDOUT(fd);
}

static void
dump(buf, sz, max)
     unsigned char* buf;
     int sz;
     int max;
{
  int i, imax;
  char comma[5];
  
  comma[0] = ',';
  comma[1] = '\0';
  if (!sz)
    return;
  if (sz > max)
    imax = max;
  else
    imax = sz;
  
  for (i=0; i<imax; i++) {
    if (i == imax-1) {
      if (sz > max)
	strcpy(comma, ",...");
      else
	comma[0] = 0;
    }
    if (isdigit(buf[i])) {
      fprintf(stderr, "%u%s", (int)(buf[i]), comma);
    } else {
      if (isalpha(buf[i])) {
	fprintf(stderr, "%c%s", buf[i], comma);
      }
      else {
	fprintf(stderr, "%u%s", (int)(buf[i]), comma);
      }
    }
  }
}

/*
 * Delays (sleeps) the given number of milli-seconds.
 */

static void
delay(unsigned ms)
{
#ifdef __WIN32__
  Sleep(ms);
#else
  struct timeval t;
  t.tv_sec = ms/1000;
  t.tv_usec = (ms % 1000) * 1000;

  select(0, NULL, NULL, NULL, &t);
#endif
}

/*
 * Generates a reply buffer given the specification.
 *
 * <packet-bytes>,<start-character>,<increment>,<size>
 *
 * Where:
 *	<packet-bytes> is 
 */
static void
generate_reply(spec)
char* spec;			/* Specification for reply. */
{
    typedef struct item {
	int start;		/* Start character. */
	int incrementer;	/* How much to increment. */
	size_t size;		/* Size of reply buffer. */
    } Item;

    Item items[256];
    int last;
    int cur;
    size_t total_size;
    char* buf;			/* Reply buffer. */
    char* s;			/* Current pointer into buffer. */
    int c;

    total_size = 0;
    last = 0;
    while (*spec) {
	char* colon;

	items[last].incrementer = 1;
	items[last].start = *spec++;
	items[last].size = atoi(spec);

	total_size += port_data->header_size+items[last].size;
	last++;
	if ((colon = strchr(spec, ':')) == NULL) {
	    spec += strlen(spec);
	} else {
	    *colon = '\0';
	    spec = colon+1;
	}
    }

    buf = (char *) malloc(total_size);
    if (buf == NULL) {
	fprintf(stderr, "%s: insufficent memory for reply buffer of size %d\n",
		port_data->progname, (int)total_size);
	exit(1);
    }

    s = buf;
    for (cur = 0; cur < last; cur++) {
	int i;
	size_t n;

	n = items[cur].size;
	s += port_data->header_size;
	for (i = 0; i < port_data->header_size; i++) {
	    *--s = (char) n;	/* Store least significant byte. */
	    n = n >> 8;
	}
	s += port_data->header_size;

	c = items[cur].start;
	for (i = 0; i < items[cur].size; i++) {
	    *s++ = c;
	    c++;
	    if (c > 126) {
		c = 33;
	    }
	}
    }
    write_reply(buf, s-buf);
}

