/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
 * Module: to_erl.c
 * 
 * This module implements a process that opens two specified FIFOs, one
 * for reading and one for writing; reads from its stdin, and writes what
 * it has read to the write FIF0; reads from the read FIFO, and writes to
 * its stdout.
 *
  ________                            _________ 
 |        |--<-- pipe.r (fifo1) --<--|         |
 | to_erl |                          | run_erl | (parent)
 |________|-->-- pipe.w (fifo2) -->--|_________|
                                          ^ master pty
                                          |
                                          | slave pty
                                      ____V____ 
                                     |         |
                                     |  "erl"  | (child)
                                     |_________|
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <dirent.h>
#include <signal.h>
#include <errno.h>
#ifdef HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif

#include "run_erl.h"
#include "safe_string.h"   /* strn_cpy, strn_catf, sn_printf, etc. */

#if defined(O_NONBLOCK)
# define DONT_BLOCK_PLEASE O_NONBLOCK
#else
# define DONT_BLOCK_PLEASE O_NDELAY
# if !defined(EAGAIN)
#  define EAGAIN -3898734
# endif
#endif

#ifdef HAVE_STRERROR
#  define STRERROR(x) strerror(x)
#else
#  define STRERROR(x) ""
#endif

#define noDEBUG

#define PIPE_DIR        "/tmp/"
#define PIPE_STUBNAME   "erlang.pipe"
#define PIPE_STUBLEN    strlen(PIPE_STUBNAME)

#ifdef DEBUG
#define STATUS(s)  { fprintf(stderr, (s)); fflush(stderr); }
#else
#define STATUS(s)
#endif

#ifndef FILENAME_MAX
#define FILENAME_MAX 250
#endif

static struct termios tty_smode, tty_rmode;
static int tty_eof = 0;
static int recv_sig = 0;
static int protocol_ver = RUN_ERL_LO_VER; /* assume lowest to begin with */

static int write_all(int fd, const char* buf, int len);
static int window_size_seq(char* buf, size_t bufsz);
static int version_handshake(char* buf, int len, int wfd);
#ifdef DEBUG
static void show_terminal_settings(struct termios *);
#endif

static void handle_ctrlc(int sig)
{
    /* Reinstall the handler, and signal break flag */
    signal(SIGINT,handle_ctrlc);
    recv_sig = SIGINT;
}  

static void handle_sigwinch(int sig)
{
    recv_sig = SIGWINCH;
}

static void usage(char *pname)
{
    fprintf(stderr, "Usage: %s [-h|-F] [pipe_name|pipe_dir/]\n", pname);
    fprintf(stderr, "\t-h\tThis help text.\n");
    fprintf(stderr, "\t-F\tForce connection even though pipe is locked by other to_erl process.\n");
}

int main(int argc, char **argv)
{
    char  FIFO1[FILENAME_MAX], FIFO2[FILENAME_MAX];
    int i, len, wfd, rfd;
    fd_set readfds;
    char buf[BUFSIZ];
    char pipename[FILENAME_MAX];
    int pipeIx = 1;
    int force_lock = 0;
    int got_some = 0;

    if (argc >= 2 && argv[1][0]=='-') {
	switch (argv[1][1]) {
	case 'h':
	    usage(argv[0]);
	    exit(1);
	case 'F':
	    force_lock = 1;
	    break;
	default:
	    fprintf(stderr,"Invalid option '%s'\n",argv[1]);
	    exit(1);
	}
	pipeIx = 2;
    }
    
#ifdef DEBUG
    fprintf(stderr, "%s: pid is : %d\n", argv[0], (int)getpid());
#endif
    
    strn_cpy(pipename, sizeof(pipename),
	     (argv[pipeIx] ? argv[pipeIx] : PIPE_DIR));
    
    if(*pipename && pipename[strlen(pipename)-1] == '/') {
	/* The user wishes us to find a pipe name in the specified */
	/* directory */
	int highest_pipe_num = 0;
	DIR *dirp;
	struct dirent *direntp;

	dirp = opendir(pipename);
	if(!dirp) {
	    fprintf(stderr, "Can't access pipe directory %s: %s\n", pipename, strerror(errno));
	    exit(1);
	}

	/* Check the directory for existing pipes */
    
	while((direntp=readdir(dirp)) != NULL) {
	    if(strncmp(direntp->d_name,PIPE_STUBNAME,PIPE_STUBLEN)==0) {
		int num = atoi(direntp->d_name+PIPE_STUBLEN+1);
		if(num > highest_pipe_num)
		    highest_pipe_num = num;
	    }
	}	
	closedir(dirp);
	strn_catf(pipename, sizeof(pipename), (highest_pipe_num?"%s.%d":"%s"),
		  PIPE_STUBNAME, highest_pipe_num);
    } /* if */

    /* read FIFO */
    sn_printf(FIFO1,sizeof(FIFO1),"%s.r",pipename);
    /* write FIFO */
    sn_printf(FIFO2,sizeof(FIFO2),"%s.w",pipename);

    /* Check that nobody is running to_erl on this pipe already */
    if ((wfd = open (FIFO1, O_WRONLY|DONT_BLOCK_PLEASE, 0)) >= 0) {
	/* Open as server succeeded -- to_erl is already running! */
	close(wfd);
	fprintf(stderr, "Another to_erl process already attached to pipe "
			"%s.\n", pipename);
	if (force_lock) {
	    fprintf(stderr, "But we proceed anyway by force (-F).\n");
	} 
	else {
	    exit(1);
	}
    }

    if ((rfd = open (FIFO1, O_RDONLY|DONT_BLOCK_PLEASE, 0)) < 0) {
#ifdef DEBUG
	fprintf(stderr, "Could not open FIFO %s for reading.\n", FIFO1);
#endif
	fprintf(stderr, "No running Erlang on pipe %s: %s\n", pipename, strerror(errno));
	exit(1);
    }
#ifdef DEBUG
    fprintf(stderr, "to_erl: %s opened for reading\n", FIFO1);
#endif
    
    if ((wfd = open (FIFO2, O_WRONLY|DONT_BLOCK_PLEASE, 0)) < 0) {
#ifdef DEBUG
	fprintf(stderr, "Could not open FIFO %s for writing.\n", FIFO2);
#endif
	fprintf(stderr, "No running Erlang on pipe %s: %s\n", pipename, strerror(errno));
	close(rfd);
	exit(1);
    }
#ifdef DEBUG
    fprintf(stderr, "to_erl: %s opened for writing\n", FIFO2);
#endif
    
    fprintf(stderr, "Attaching to %s (^D to exit)\n\n", pipename);
    
    /* Set break handler to our handler */
    signal(SIGINT,handle_ctrlc);

    /* 
     * Save the current state of the terminal, and set raw mode.
     */
    if (tcgetattr(0, &tty_rmode) , 0) {
	fprintf(stderr, "Cannot get terminals current mode\n");
	exit(-1);
    }
    tty_smode = tty_rmode;
    tty_eof = '\004'; /* Ctrl+D to exit */
#ifdef DEBUG
    show_terminal_settings(&tty_rmode);
#endif
    tty_smode.c_iflag =
	1*BRKINT |/*Signal interrupt on break.*/
	    1*IGNPAR |/*Ignore characters with parity errors.*/
		    0;
    
#if 0
0*IGNBRK |/*Ignore break condition.*/
0*PARMRK |/*Mark parity errors.*/
0*INPCK  |/*Enable input parity check.*/
0*INLCR  |/*Map NL to CR on input.*/
0*IGNCR  |/*Ignore CR.*/
0*ICRNL  |/*Map CR to NL on input.*/
0*IUCLC  |/*Map upper-case to lower-case on input.*/
0*IXON   |/*Enable start/stop output control.*/
0*IXANY  |/*Enable any character to restart output.*/
0*IXOFF  |/*Enable start/stop input control.*/
0*IMAXBEL|/*Echo BEL on input line too long.*/
#endif
						
    tty_smode.c_oflag =
	1*OPOST  |/*Post-process output.*/
	    1*ONLCR  |/*Map NL to CR-NL on output.*/
#ifdef XTABS
		1*XTABS  |/*Expand tabs to spaces. (Linux)*/
#endif
#ifdef OXTABS
		    1*OXTABS  |/*Expand tabs to spaces. (FreeBSD)*/
#endif
#ifdef NL0
			1*NL0    |/*Select newline delays*/
#endif
#ifdef CR0
			    1*CR0    |/*Select carriage-return delays*/
#endif
#ifdef TAB0
				1*TAB0   |/*Select horizontal tab delays*/
#endif
#ifdef BS0
				    1*BS0    |/*Select backspace delays*/
#endif
#ifdef VT0
					1*VT0    |/*Select vertical tab delays*/
#endif
#ifdef FF0
					    1*FF0    |/*Select form feed delays*/
#endif
											    0;
    
#if 0
0*OLCUC  |/*Map lower case to upper on output.*/
0*OCRNL  |/*Map CR to NL on output.*/
0*ONOCR  |/*No CR output at column 0.*/
0*ONLRET |/*NL performs CR function.*/
0*OFILL  |/*Use fill characters for delay.*/
0*OFDEL  |/*Fill is DEL, else NULL.*/
0*NL1    |
0*CR1    |
0*CR2    |
0*CR3    |
0*TAB1   |
0*TAB2   |
0*TAB3   |/*Expand tabs to spaces.*/
0*BS1    |
0*VT1    |
0*FF1    |
#endif
								    
    /* JALI: removed setting the tty_smode.c_cflag flags, since this is not */
    /* advisable if this is a *real* terminal, such as the console. In fact */
    /* this may hang the entire machine, deep, deep down (signalling break */
    /* or toggling the abort switch doesn't help) */
    
    tty_smode.c_lflag =
									0;
    
#if 0
0*ISIG   |/*Enable signals.*/
0*ICANON |/*Canonical input (erase and kill processing).*/
0*XCASE  |/*Canonical upper/lower presentation.*/
0*ECHO   |/*Enable echo.*/
0*ECHOE  |/*Echo erase character as BS-SP-BS.*/
0*ECHOK  |/*Echo NL after kill character.*/
0*ECHONL |/*Echo NL.*/
0*NOFLSH |/*Disable flush after interrupt or quit.*/
0*TOSTOP |/*Send SIGTTOU for background output.*/
0*ECHOCTL|/*Echo control characters as ^char, delete as ^?.*/
0*ECHOPRT|/*Echo erase character as character erased.*/
0*ECHOKE |/*BS-SP-BS erase entire line on line kill.*/
0*FLUSHO |/*Output is being flushed.*/
0*PENDIN |/*Retype pending input at next read or input character.*/
0*IEXTEN |/*Enable extended (implementation-defined) functions.*/
#endif
								
    tty_smode.c_cc[VMIN]      =0;/* Note that VMIN is the same as VEOF! */
    tty_smode.c_cc[VTIME]     =0;/* Note that VTIME is the same as VEOL! */
    tty_smode.c_cc[VINTR]     =3;
    
    tcsetattr(0, TCSADRAIN, &tty_smode);
    
#ifdef DEBUG
    show_terminal_settings(&tty_smode);
#endif
    /*
     * 	 "Write a ^L to the FIFO which causes the other end to redisplay
     *    the input line."
     * This does not seem to work as was intended in old comment above.
     * However, this control character is now (R12B-3) used by run_erl
     * to trigger the version handshaking between to_erl and run_erl
     * at the start of every new to_erl-session.
     */

    if (write(wfd, "\014", 1) < 0) {
	fprintf(stderr, "Error in writing ^L to FIFO.\n");
    }

    /*
     * read and write
     */
    while (1) {
	FD_ZERO(&readfds);
	FD_SET(0, &readfds);
	FD_SET(rfd, &readfds);
	if (select(rfd + 1, &readfds, NULL, NULL, NULL) < 0) {
	    if (recv_sig) {
		FD_ZERO(&readfds);
	    }
	    else {
		fprintf(stderr, "Error in select.\n");
		break;
	    }
	}
	len = 0;

	/*
	 * Read from terminal and write to FIFO
         */
	if (recv_sig) {
	    switch (recv_sig) {
	    case SIGINT:
		fprintf(stderr, "[Break]\n\r");
		buf[0] = '\003';
		len = 1;
		break;
	    case SIGWINCH:
		len = window_size_seq(buf,sizeof(buf));
		break;
	    default:
		fprintf(stderr,"Unexpected signal: %u\n",recv_sig);
	    }
	    recv_sig = 0;
	}
	else if (FD_ISSET(0, &readfds)) {
	    len = read(0, buf, sizeof(buf));
	    if (len <= 0) {
		close(rfd);
		close(wfd);
		if (len < 0) {
		    fprintf(stderr, "Error in reading from stdin.\n");
		} else {
		    fprintf(stderr, "[EOF]\n\r");
		}
		break;
	    }
	    /* check if there is an eof character in input */
	    for (i = 0; i < len && buf[i] != tty_eof; i++);
	    if (buf[i] == tty_eof) {
		fprintf(stderr, "[Quit]\n\r");
		break;
	    }
	}

	if (len) {
#ifdef DEBUG
	    write_all(1, buf, len);
#endif
	    if (write_all(wfd, buf, len) != len) {
		fprintf(stderr, "Error in writing to FIFO.\n");
		close(rfd);
		close(wfd);
		break;
	    }
	    STATUS("\" OK\r\n");
	}

	/*
	 * Read from FIFO, write to terminal.
	 */
	if (FD_ISSET(rfd, &readfds)) {
	    STATUS("FIFO read: ");
	    len = read(rfd, buf, BUFSIZ);
	    if (len < 0 && errno == EAGAIN) {
		/*
		 * No data this time, but the writing end of the FIFO is still open.
		 * Do nothing.
		 */
		;
	    } else if (len <= 0) {
		/*
		 * Either an error or end of file. In either case, break out
		 * of the loop.
		 */
		close(rfd);
		close(wfd);
		if (len < 0) {
		    fprintf(stderr, "Error in reading from FIFO.\n");
		} else
		    fprintf(stderr, "[End]\n\r");
		break;
	    } else {
		if (!got_some) {
		    if ((len=version_handshake(buf,len,wfd)) < 0) {
			close(rfd);
			close(wfd);
			break;
		    }
		    if (protocol_ver >= 1) {
			/* Tell run_erl size of terminal window */
			signal(SIGWINCH, handle_sigwinch);
			raise(SIGWINCH);
		    }
		    got_some = 1;
		}

		/*
		 * We successfully read at least one character. Write what we got.
		 */
		STATUS("Terminal write: \"");
		if (write_all(1, buf, len) != len) {
		    fprintf(stderr, "Error in writing to terminal.\n");
		    close(rfd);
		    close(wfd);
		    break;
		}
		STATUS("\" OK\r\n");
	    }
	}
    }

    /* 
     * Reset terminal characterstics 
     * XXX
     */
    tcsetattr(0, TCSADRAIN, &tty_rmode);
    return 0;
}

/* Call write() until entire buffer has been written or error.
 * Return len or -1.
 */
static int write_all(int fd, const char* buf, int len)
{
    int left = len;
    int written;
    while (left) {
	written = write(fd,buf,left);
	if (written < 0) {
	    return -1;
	}
	left -= written;
	buf += written;
    }
    return len;
}

static int window_size_seq(char* buf, size_t bufsz)
{
#ifdef TIOCGWINSZ
    struct winsize ws;
    static const char prefix[] = "\033_";
    static const char suffix[] = "\033\\";
    /* This Esc sequence is called "Application Program Command"
       and seems suitable to use for our own customized stuff. */

    if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) == 0) {
	int len = sn_printf(buf, bufsz, "%swinsize=%u,%u%s",
			    prefix, ws.ws_col, ws.ws_row, suffix);
	return len;
    }
#endif /* TIOCGWINSZ */
    return 0;
}

/*   to_erl                     run_erl
 *     |                           |
 *     |---------- '\014' -------->| (session start)
 *     |                           |
 *     |<---- "[run_erl v1-0]" ----| (version interval)
 *     |                           |
 *     |--- Esc_"version=1"Esc\ -->| (common version)
 *     |                           |
 */
static int version_handshake(char* buf, int len, int wfd)
{
    unsigned re_high=0, re_low;
    char *end = find_str(buf,len,"]\n");
    
    if (end && sscanf(buf,"[run_erl v%u-%u",&re_high,&re_low)==2) {
	char wbuf[30];
	int wlen;

	if (re_low > RUN_ERL_HI_VER || re_high < RUN_ERL_LO_VER) {
	    fprintf(stderr,"Incompatible versions: to_erl=v%u-%u run_erl=v%u-%u\n",
		    RUN_ERL_HI_VER, RUN_ERL_LO_VER, re_high, re_low);
	    return -1;
	}
	/* Choose highest common version */
	protocol_ver = re_high < RUN_ERL_HI_VER ? re_high : RUN_ERL_HI_VER;

	wlen = sn_printf(wbuf, sizeof(wbuf), "\033_version=%u\033\\",
			 protocol_ver);
	if (write_all(wfd, wbuf, wlen) < 0) {
	    fprintf(stderr,"Failed to send version handshake\n");
	    return -1;
	}
	end += 2;
	len -= (end-buf);
	memmove(buf,end,len);

    }
    else {  /* we assume old run_erl without version handshake */
	protocol_ver = 0;
    }

    if (re_high != RUN_ERL_HI_VER) {
	fprintf(stderr,"run_erl has different version, "
		"using common protocol level %u\n", protocol_ver);
    }

    return len;
}


#ifdef DEBUG
#define S(x)  ((x) > 0 ? 1 : 0)

static void show_terminal_settings(struct termios *t)
{
  fprintf(stderr,"c_iflag:\n");
  fprintf(stderr,"Signal interrupt on break:   BRKINT  %d\n", S(t->c_iflag & BRKINT));
  fprintf(stderr,"Map CR to NL on input:       ICRNL   %d\n", S(t->c_iflag & ICRNL));
  fprintf(stderr,"Ignore break condition:      IGNBRK  %d\n", S(t->c_iflag & IGNBRK));
  fprintf(stderr,"Ignore CR:                   IGNCR   %d\n", S(t->c_iflag & IGNCR));
  fprintf(stderr,"Ignore char with par. err's: IGNPAR  %d\n", S(t->c_iflag & IGNPAR));
  fprintf(stderr,"Map NL to CR on input:       INLCR   %d\n", S(t->c_iflag & INLCR));
  fprintf(stderr,"Enable input parity check:   INPCK   %d\n", S(t->c_iflag & INPCK));
  fprintf(stderr,"Strip character              ISTRIP  %d\n", S(t->c_iflag & ISTRIP));
  fprintf(stderr,"Enable start/stop input ctrl IXOFF   %d\n", S(t->c_iflag & IXOFF));
  fprintf(stderr,"ditto output ctrl            IXON    %d\n", S(t->c_iflag & IXON));
  fprintf(stderr,"Mark parity errors           PARMRK  %d\n", S(t->c_iflag & PARMRK));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_oflag:\n");
  fprintf(stderr,"Perform output processing    OPOST   %d\n", S(t->c_oflag & OPOST));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_cflag:\n");
  fprintf(stderr,"Ignore modem status lines    CLOCAL  %d\n", S(t->c_cflag & CLOCAL));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_local:\n");
  fprintf(stderr,"Enable echo                  ECHO    %d\n", S(t->c_lflag & ECHO));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_cc:\n");
  fprintf(stderr,"c_cc[VEOF]                           %d\n", t->c_cc[VEOF]);
}
#endif
