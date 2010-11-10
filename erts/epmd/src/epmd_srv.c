/* -*- c-indent-level: 2; c-continued-statement-offset: 2 -*- */ 
/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "epmd.h"     /* Renamed from 'epmd_r4.h' */
#include "epmd_int.h"

#ifndef INADDR_NONE
#  define INADDR_NONE 0xffffffff
#endif

/*
 *  
 *  This server is a local name server for Erlang nodes. Erlang nodes can
 *  ask this server for the listening port of other Erlang nodes on the
 *  machine EPMD is running on. New distributed nodes that are started
 *  register their names with this server.
 *
 *  To be accessible to all Erlang nodes this server listens to a well
 *  known port EPMD_PORT_NO (curently port 4369) where requests
 *  for connections can be sent.
 *
 *  To keep track of when registered Erlang nodes are terminated this
 *  server keeps the socket open where the request for registration was
 *  made.
 *
 *  The protocol is briefly documented in the ERTS User's Guide, see
 *  http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html
 *
 *  All requests to this server are done with a packet
 *
 *      2        n
 *  +--------+---------+
 *  | Length | Request |
 *  +--------+---------+
 *
 */

/* We use separate data structures for node names and connections
   so that a request will not use a slot with a name that we
   want to resuse later incrementing the "creation" */


/* forward declarations */

static void do_request(EpmdVars*,int,Connection*,char*,int);
static int do_accept(EpmdVars*,int);
static void do_read(EpmdVars*,Connection*);
static time_t current_time(EpmdVars*);

static Connection *conn_init(EpmdVars*);
static int conn_open(EpmdVars*,int);
static int conn_close_fd(EpmdVars*,int);

static void node_init(EpmdVars*);
static Node *node_reg2(EpmdVars*,char*, int, int, unsigned char, unsigned char, int, int, int, char*);
static int node_unreg(EpmdVars*,char*);
static int node_unreg_sock(EpmdVars*,int);

static int reply(EpmdVars*,int,char *,int);
static void dbg_print_buf(EpmdVars*,char *,int);
static void print_names(EpmdVars*);


void run(EpmdVars *g)
{
  struct EPMD_SOCKADDR_IN iserv_addr[MAX_LISTEN_SOCKETS];
  int listensock[MAX_LISTEN_SOCKETS];
  int num_sockets;
  int i;
  int opt;
  unsigned short sport = g->port;

  node_init(g);
  g->conn = conn_init(g);

  dbg_printf(g,2,"try to initiate listening port %d", g->port);

  if (g->addresses != NULL)
    {
      char *tmp;
      char *token;
      int loopback_ok = 0;

      if ((tmp = (char *)malloc(strlen(g->addresses) + 1)) == NULL)
	{
	  dbg_perror(g,"cannot allocate memory");
	  epmd_cleanup_exit(g,1);
	}
      strcpy(tmp,g->addresses);

      for(token = strtok(tmp,", "), num_sockets = 0;
	  token != NULL;
	  token = strtok(NULL,", "), num_sockets++)
	{
	  struct EPMD_IN_ADDR addr;
#ifdef HAVE_INET_PTON
	  int ret;

	  if ((ret = inet_pton(FAMILY,token,&addr)) == -1)
	    {
	      dbg_perror(g,"cannot convert IP address to network format");
	      epmd_cleanup_exit(g,1);
	    }
	  else if (ret == 0)
#elif !defined(EPMD6)
	  if ((addr.EPMD_S_ADDR = inet_addr(token)) == INADDR_NONE)
#endif
	    {
	      dbg_tty_printf(g,0,"cannot parse IP address \"%s\"",token);
	      epmd_cleanup_exit(g,1);
	    }

	  if (IS_ADDR_LOOPBACK(addr))
	    loopback_ok = 1;

	  if (num_sockets - loopback_ok == MAX_LISTEN_SOCKETS - 1)
	    {
	      dbg_tty_printf(g,0,"cannot listen on more than %d IP addresses",
			     MAX_LISTEN_SOCKETS);
	      epmd_cleanup_exit(g,1);
	    }

	  SET_ADDR(iserv_addr[num_sockets],addr.EPMD_S_ADDR,sport);
	}

      free(tmp);

      if (!loopback_ok)
	{
	  SET_ADDR(iserv_addr[num_sockets],EPMD_ADDR_LOOPBACK,sport);
	  num_sockets++;
	}
    }
  else
    {
      SET_ADDR(iserv_addr[0],EPMD_ADDR_ANY,sport);
      num_sockets = 1;
    }

#if !defined(__WIN32__)
  /* We ignore the SIGPIPE signal that is raised when we call write
     twice on a socket closed by the other end. */
  signal(SIGPIPE, SIG_IGN);
#endif

  /*
   * Initialize number of active file descriptors.
   * Stdin, stdout, and stderr are still open.
   */
  g->active_conn = 3 + num_sockets;
  g->max_conn -= num_sockets;

  FD_ZERO(&g->orig_read_mask);

  for (i = 0; i < num_sockets; i++)
    {
      if ((listensock[i] = socket(FAMILY,SOCK_STREAM,0)) < 0)
	{
	  dbg_perror(g,"error opening stream socket");
	  epmd_cleanup_exit(g,1);
	}
      g->listenfd[i] = listensock[i];
  
      /*
       * Note that we must not enable the SO_REUSEADDR on Windows,
       * because addresses will be reused even if they are still in use.
       */
  
#if !defined(__WIN32__)
      opt = 1;
      if (setsockopt(listensock[i],SOL_SOCKET,SO_REUSEADDR,(char* ) &opt,
		     sizeof(opt)) <0)
	{
	  dbg_perror(g,"can't set sockopt");
	  epmd_cleanup_exit(g,1);
	}
#endif
  
      /* In rare cases select returns because there is someone
	 to accept but the request is withdrawn before the
	 accept function is called. We set the listen socket
	 to be non blocking to prevent us from being hanging
	 in accept() waiting for the next request. */
#if (defined(__WIN32__) || defined(NO_FCNTL))
      opt = 1;
      /* Gives warning in VxWorks */
      if (ioctl(listensock[i], FIONBIO, &opt) != 0)
#else
      opt = fcntl(listensock[i], F_GETFL, 0);
      if (fcntl(listensock[i], F_SETFL, opt | O_NONBLOCK) == -1)
#endif /* __WIN32__ || VXWORKS */
	dbg_perror(g,"failed to set non-blocking mode of listening socket %d",
		   listensock[i]);

      if (bind(listensock[i], (struct sockaddr*) &iserv_addr[i],
	  sizeof(iserv_addr[i])) < 0)
	{
	  if (errno == EADDRINUSE)
	    {
	      dbg_tty_printf(g,1,"there is already a epmd running at port %d",
			     g->port);
	      epmd_cleanup_exit(g,0);
	    }
	  else
	    {
	      dbg_perror(g,"failed to bind socket");
	      epmd_cleanup_exit(g,1);
	    }
	}
      listen(listensock[i],SOMAXCONN);
      FD_SET(listensock[i],&g->orig_read_mask);
    }

  dbg_tty_printf(g,2,"entering the main select() loop");

 select_again:
  while(1)
    {	
      fd_set read_mask = g->orig_read_mask;
      struct timeval timeout;
      int ret;

      /* If we are idle we time out now and then to enable the code
	 below to close connections that are old and probably
	 hanging. Make sure that select will return often enough. */

      timeout.tv_sec = (g->packet_timeout < IDLE_TIMEOUT) ? 1 : IDLE_TIMEOUT;
      timeout.tv_usec = 0;

      if ((ret = select(g->max_conn,&read_mask,(fd_set *)0,(fd_set *)0,&timeout)) < 0) {
	dbg_perror(g,"error in select ");
        switch (errno) {
          case EAGAIN:
          case EINTR:
            break;
          default:
            epmd_cleanup_exit(g,1);
        }
      }
      else {
	time_t now;
	if (ret == 0) {
	  FD_ZERO(&read_mask);
	}
	if (g->delay_accept) {		/* Test of busy server */
	  sleep(g->delay_accept);
	}

	for (i = 0; i < num_sockets; i++)
	  if (FD_ISSET(listensock[i],&read_mask)) {
	    if (do_accept(g, listensock[i]) && g->active_conn < g->max_conn) {
	      /*
	       * The accept() succeeded, and we have at least one file
	       * descriptor still free, which means that another accept()
	       * could succeed. Go do do another select(), in case there
	       * are more incoming connections waiting to be accepted.
	       */
	      goto select_again;
	    }
	  }
	  
	/* Check all open streams marked by select for data or a
	   close.  We also close all open sockets except ALIVE
	   with no activity for a long period */

	now = current_time(g);
	for (i = 0; i < g->max_conn; i++) {
	  if (g->conn[i].open == EPMD_TRUE) {
	    if (FD_ISSET(g->conn[i].fd,&read_mask))
	      do_read(g,&g->conn[i]);
	    else if ((g->conn[i].keep == EPMD_FALSE) &&
		     ((g->conn[i].mod_time + g->packet_timeout) < now)) {
	      dbg_tty_printf(g,1,"closing because timed out on receive");
	      epmd_conn_close(g,&g->conn[i]);
	    }
	  }
	}
      }
    }
}

/*
 *  This routine read as much of the packet as possible and
 *  if completed calls "do_request()" to fullfill the request.
 *
 */

static void do_read(EpmdVars *g,Connection *s)
{
  int val, pack_size;

  if (s->open == EPMD_FALSE)
    {
      dbg_printf(g,0,"read on unknown socket");
      return;
    }

  /* Check if we already got the whole packet but we keep the
     connection alive to find out when a node is terminated. We then
     want to check for a close */

  if (s->keep == EPMD_TRUE)
    {
      val = read(s->fd, s->buf, INBUF_SIZE);

      if (val == 0)
	{
	  node_unreg_sock(g,s->fd);
	  epmd_conn_close(g,s);
	}
      else if (val < 0)
	{
	    dbg_tty_printf(g,1,"error on ALIVE socket %d (%d; errno=0x%x)",
			   s->fd, val, errno);
	  node_unreg_sock(g,s->fd);
	  epmd_conn_close(g,s);
	}
      else
	{
	  dbg_tty_printf(g,1,"got more than expected on ALIVE socket %d (%d)",
		 s->fd,val);
	  dbg_print_buf(g,s->buf,val);

	  node_unreg_sock(g,s->fd);
	  epmd_conn_close(g,s);
	}
      return;
    }

  /* If unknown size we request the whole buffer - what we got - 1
     We subtract 1 because we will add a "\0" in "do_request()".
     This is not needed for R3A or higher versions of Erlang,
     because the '\0' is included in the request,
     but is kept for backwards compatibility to allow R2D to use
     this epmd. */

  pack_size = s->want ? s->want : INBUF_SIZE - 1;
  val = read(s->fd, s->buf + s->got, pack_size - s->got);

  if (val == 0)
    {
      /* A close when we haven't got all data */
      dbg_printf(g,0,"got partial packet only on file descriptor %d (%d)",
		 s->fd,s->got);
      epmd_conn_close(g,s);
      return;
    }

  if (val < 0)
    {
      dbg_perror(g,"error in read");
      epmd_conn_close(g,s);
      return;
    }

  dbg_print_buf(g,s->buf,val);

  s->got += val;

  if ((s->want == 0) && (s->got >= 2))
    {
      /* The two byte header that specify the length of the packet
	 doesn't count the header as part of the packet so we add 2
	 to "s->want" to make us talk about all bytes we get. */

      s->want = get_int16(s->buf) + 2;

      if ((s->want < 3) || (s->want >= INBUF_SIZE))
	{
	  dbg_printf(g,0,"invalid packet size (%d)",s->want - 2);
	  epmd_conn_close(g,s);
	  return;
	}

      if (s->got > s->want)
	{
	  dbg_printf(g,0,"got %d bytes in packet, expected %d",
		     s->got - 2, s->want - 2);
	  epmd_conn_close(g,s);
	  return;
	}
    }
  
  s->mod_time = current_time(g); /* Note activity */
  
  if (s->want == s->got)
    {
      /* Do action and close up */
      /* Skip header bytes */

      do_request(g, s->fd, s, s->buf + 2, s->got - 2);

      if (!s->keep)
	epmd_conn_close(g,s);		/* Normal close */
    }
}

static int do_accept(EpmdVars *g,int listensock)
{
    int msgsock;
    struct EPMD_SOCKADDR_IN icli_addr; /* workaround for QNX bug - cannot */
    int icli_addr_len;            /* handle NULL pointers to accept.  */

    icli_addr_len = sizeof(icli_addr);

    msgsock = accept(listensock,(struct sockaddr*) &icli_addr,
		     (unsigned int*) &icli_addr_len);

    if (msgsock < 0) {
        dbg_perror(g,"error in accept");
        switch (errno) {
            case EAGAIN:
            case ECONNABORTED:
            case EINTR:
	            return EPMD_FALSE;
            default:
	            epmd_cleanup_exit(g,1);
        }
    }

    return conn_open(g,msgsock);
}

/* buf is actually one byte larger than bsize,
   giving place for null termination */
static void do_request(g, fd, s, buf, bsize)
     EpmdVars *g;
     int fd;
     Connection *s;
     char *buf;
     int bsize;
{
  char wbuf[OUTBUF_SIZE];	/* Buffer for writing */
  int i;

  buf[bsize] = '\0'; /* Needed for strcmp in PORT2 and STOP requests
			buf is always large enough */

  switch (*buf)
    {
    case EPMD_ALIVE2_REQ:
      dbg_printf(g,1,"** got ALIVE2_REQ");
      if (!s->local_peer) {
	   dbg_printf(g,0,"ALIVE2_REQ from non local address");
	   return;
      }

      /* The packet has the format "axxyyyyyy" where xx is port, given
	 in network byte order, and yyyyyy is symname, possibly null
	 terminated. */

      if (bsize <= 13) /* at least one character for the node name */
	{
	  dbg_printf(g,0,"packet to small for request ALIVE2_REQ (%d)",bsize);
	  return;
	}

      {
	Node *node;
	int eport;
	unsigned char nodetype;
	unsigned char protocol;
	unsigned short highvsn;
	unsigned short lowvsn;
	int namelen;
	int extralen;
	char *name; 
	char *extra;
	eport = get_int16(&buf[1]);
	nodetype = buf[3];
	protocol = buf[4];
	highvsn = get_int16(&buf[5]);
	lowvsn = get_int16(&buf[7]);
	namelen = get_int16(&buf[9]);
	if (namelen + 13 > bsize) {
	    dbg_printf(g,0,"Node name size error in ALIVE2_REQ");
	    return;
	}
	extralen = get_int16(&buf[11+namelen]);

	if (extralen + namelen + 13 > bsize) {
	    dbg_printf(g,0,"Extra info size error in ALIVE2_REQ");
	    return;
	}

	for (i = 11 ; i < 11 + namelen; i ++)
	    if (buf[i] == '\000')  {
		dbg_printf(g,0,"node name contains ascii 0 in ALIVE2_REQ");
		return;
	    }
	name = &buf[11];
	name[namelen]='\000';
	extra = &buf[11+namelen+2];
	extra[extralen]='\000';
	wbuf[0] = EPMD_ALIVE2_RESP;
	if ((node = node_reg2(g, name, fd, eport, nodetype, protocol,
			      highvsn, lowvsn, extralen, extra)) == NULL) {
	    wbuf[1] = 1; /* error */
	    put_int16(99, wbuf+2);
	} else {
	    wbuf[1] = 0; /* ok */
	    put_int16(node->creation, wbuf+2);
	}
  
	if (g->delay_write)		/* Test of busy server */
	  sleep(g->delay_write);

	if (reply(g, fd, wbuf, 4) != 4)
	  {
	    dbg_tty_printf(g,1,"** failed to send ALIVE2_RESP for \"%s\"",
			   name);
	    return;
	  }

	dbg_tty_printf(g,1,"** sent ALIVE2_RESP for \"%s\"",name);
	s->keep = EPMD_TRUE;		/* Don't close on inactivity */
      }
      break;

    case EPMD_PORT2_REQ:
      dbg_printf(g,1,"** got PORT2_REQ");

      if (buf[bsize - 1] == '\000') /* Skip null termination */
	bsize--;
	
      if (bsize <= 1)
	{
	  dbg_printf(g,0,"packet too small for request PORT2_REQ (%d)", bsize);
	  return;
	}

      for (i = 1; i < bsize; i++)
	if (buf[i] == '\000')
	  {
	    dbg_printf(g,0,"node name contains ascii 0 in PORT2_REQ");
	    return;
	  }

      {
	char *name = &buf[1]; /* Points to node name */
	Node *node;
	
	wbuf[0] = EPMD_PORT2_RESP;
	for (node = g->nodes.reg; node; node = node->next) {
	    int offset;
	    if (strcmp(node->symname, name) == 0) {
		wbuf[1] = 0; /* ok */
		put_int16(node->port,wbuf+2);
		wbuf[4] = node->nodetype;
		wbuf[5] = node->protocol;
		put_int16(node->highvsn,wbuf+6);
		put_int16(node->lowvsn,wbuf+8);
		put_int16(strlen(node->symname),wbuf+10);
		offset = 12;
		strcpy(wbuf + offset,node->symname);
		offset += strlen(node->symname);
		put_int16(node->extralen,wbuf + offset);
		offset += 2;
		memcpy(wbuf + offset,node->extra,node->extralen);
		offset += node->extralen;
		if (reply(g, fd, wbuf, offset) != offset)
		  {
		    dbg_tty_printf(g,1,"** failed to send PORT2_RESP (ok) for \"%s\"",name);
		    return;
		  }
		dbg_tty_printf(g,1,"** sent PORT2_RESP (ok) for \"%s\"",name);
		return;
	    }
	}
	wbuf[1] = 1; /* error */
	if (reply(g, fd, wbuf, 2) != 2)
	  {
	    dbg_tty_printf(g,1,"** failed to send PORT2_RESP (error) for \"%s\"",name);
	    return;
	  }
	dbg_tty_printf(g,1,"** sent PORT2_RESP (error) for \"%s\"",name);
	return;
      }
      break;

    case EPMD_NAMES_REQ:
      dbg_printf(g,1,"** got NAMES_REQ");
      {
	Node *node;

	i = htonl(g->port);
	memcpy(wbuf,&i,4);

	if (reply(g, fd,wbuf,4) != 4)
	  {
	    dbg_tty_printf(g,1,"failed to send NAMES_RESP");
	    return;
	  }

	for (node = g->nodes.reg; node; node = node->next)
	  {
	    int len;

	    /* CAREFUL!!! These are parsed by "erl_epmd.erl" so a slight
	       change in syntax will break < OTP R3A */

	    sprintf(wbuf,"name %s at port %d\n",node->symname, node->port);
	    len = strlen(wbuf);
	    if (reply(g, fd, wbuf, len) != len)
	      {
		dbg_tty_printf(g,1,"failed to send NAMES_RESP");
		return;
	      }
	  }
      }
      dbg_tty_printf(g,1,"** sent NAMES_RESP");
      break;

    case EPMD_DUMP_REQ:
      dbg_printf(g,1,"** got DUMP_REQ");
      if (!s->local_peer) {
	   dbg_printf(g,0,"DUMP_REQ from non local address");
	   return;
      }
      {
	Node *node;

	i = htonl(g->port);
	memcpy(wbuf,&i,4);
	if (reply(g, fd,wbuf,4) != 4)
	  {
	    dbg_tty_printf(g,1,"failed to send DUMP_RESP");
	    return;
	  }

	for (node = g->nodes.reg; node; node = node->next)
	  {
	    int len;

	    /* CAREFUL!!! These are parsed by "erl_epmd.erl" so a slight
	       change in syntax will break < OTP R3A */

	    sprintf(wbuf,"active name     <%s> at port %d, fd = %d\n",
		    node->symname, node->port, node->fd);
	    len = strlen(wbuf) + 1;
	    if (reply(g, fd,wbuf,len) != len)
	      {
		dbg_tty_printf(g,1,"failed to send DUMP_RESP");
		return;
	      }
	  }

	for (node = g->nodes.unreg; node; node = node->next)
	  {
	    int len;

	    /* CAREFUL!!! These are parsed by "erl_epmd.erl" so a slight
	       change in syntax will break < OTP R3A */

	    sprintf(wbuf,"old/unused name <%s>, port = %d, fd = %d \n",
		    node->symname,node->port, node->fd);
	    len = strlen(wbuf) + 1;
	    if (reply(g, fd,wbuf,len) != len)
	      {
		dbg_tty_printf(g,1,"failed to send DUMP_RESP");
		return;
	      }
	  }
      }
      dbg_tty_printf(g,1,"** sent DUMP_RESP");
      break;

    case EPMD_KILL_REQ:
      if (!s->local_peer) {
	   dbg_printf(g,0,"KILL_REQ from non local address");
	   return;
      }
      dbg_printf(g,1,"** got KILL_REQ");

      if (!g->brutal_kill && (g->nodes.reg != NULL)) {
	  dbg_printf(g,0,"Disallowed KILL_REQ, live nodes");
	  if (reply(g, fd,"NO",2) != 2)
	      dbg_printf(g,0,"failed to send reply to KILL_REQ");
	  return;
      }

      if (reply(g, fd,"OK",2) != 2)
	dbg_printf(g,0,"failed to send reply to KILL_REQ");
      dbg_tty_printf(g,1,"epmd killed");
      conn_close_fd(g,fd);	/* We never return to caller so close here */
      dbg_printf(g,0,"got KILL_REQ - terminates normal");
      epmd_cleanup_exit(g,0);			/* Normal exit */

    case EPMD_STOP_REQ:
      dbg_printf(g,1,"** got STOP_REQ");
      if (!s->local_peer) {
	   dbg_printf(g,0,"STOP_REQ from non local address");
	   return;
      }
      if (!g->brutal_kill) {
	  dbg_printf(g,0,"Disallowed STOP_REQ, no relaxed_command_check");
	  return;
      }

      if (bsize <= 1 )
	{
	  dbg_printf(g,0,"packet too small for request STOP_REQ (%d)",bsize);
	  return;
	}

      {
	char *name = &buf[1]; /* Points to node name */
	int node_fd;

	if ((node_fd = node_unreg(g,name)) < 0)
	  {
	    if (reply(g, fd,"NOEXIST",7) != 7)
	      {
		dbg_tty_printf(g,1,"failed to send STOP_RESP NOEXIST");
		return;
	      }
	    dbg_tty_printf(g,1,"** sent STOP_RESP NOEXIST");
	  }

	conn_close_fd(g,node_fd);
	dbg_tty_printf(g,1,"epmd connection stopped");

	if (reply(g, fd,"STOPPED",7) != 7)
	  {
	    dbg_tty_printf(g,1,"failed to send STOP_RESP STOPPED");
	    return;
	  }
	dbg_tty_printf(g,1,"** sent STOP_RESP STOPPED");
      }
      break;

    default:
      dbg_printf(g,0,"got garbage ");
    }
}


/****************************************************************************
 *
 *  Handle database with data for each socket to read
 *
 ****************************************************************************/

static Connection *conn_init(EpmdVars *g)
{
  int nbytes = g->max_conn * sizeof(Connection);
  Connection *connections = (Connection *)malloc(nbytes);

  if (connections == NULL)
    {
      dbg_printf(g,0,"epmd: Insufficient memory");
#ifdef DONT_USE_MAIN
      free(g->argv);
#endif
      exit(1);
    }

  memzero(connections, nbytes);

  return connections;
}

static int conn_open(EpmdVars *g,int fd)
{
  int i;
  Connection *s;

#ifdef VXWORKS
  /*
   * Since file descriptors are global on VxWorks, we might get an fd that
   * does not fit in the FD_SET.
   *
   * Note: This test would be harmless on Unix, but would fail on Windows
   * because socket are numbered differently and FD_SETs are implemented
   * differently.
   */
  if (fd >= FD_SETSIZE) {
      dbg_tty_printf(g,0,"file descriptor %d: too high for FD_SETSIZE=%d",
		     fd,FD_SETSIZE);
      close(fd);
      return EPMD_FALSE;
  }
#endif

  for (i = 0; i < g->max_conn; i++) {
    if (g->conn[i].open == EPMD_FALSE) {
      struct sockaddr_in si;
#ifdef HAVE_SOCKLEN_T
      socklen_t st;
#else
      int st;
#endif
      st = sizeof(si);

      g->active_conn++;
      s = &g->conn[i];
     
      /* From now on we want to know if there are data to be read */
      FD_SET(fd, &g->orig_read_mask);

      s->fd   = fd;
      s->open = EPMD_TRUE;
      s->keep = EPMD_FALSE;

      /* Determine if connection is from localhost */
      if (getpeername(s->fd,(struct sockaddr*) &si,&st) ||
	  st < sizeof(si)) {
	  /* Failure to get peername is regarder as non local host */
	  s->local_peer = EPMD_FALSE;
      } else {
	  s->local_peer =
	      ((((unsigned) ntohl(si.sin_addr.s_addr)) & 0xFF000000U) ==
	       0x7F000000U); /* Only 127.x.x.x allowed, no false positives */
      }
      dbg_tty_printf(g,2,(s->local_peer) ? "Local peer connected" :
		     "Non-local peer connected");

      s->want = 0;		/* Currently unknown */
      s->got  = 0;
      s->mod_time = current_time(g); /* Note activity */

      s->buf = (char *)malloc(INBUF_SIZE);

      if (s->buf == NULL) {
	dbg_printf(g,0,"epmd: Insufficient memory");
	close(fd);
	return EPMD_FALSE;
      }

      dbg_tty_printf(g,2,"opening connection on file descriptor %d",fd);
      return EPMD_TRUE;
    }
  }

  dbg_tty_printf(g,0,"failed opening connection on file descriptor %d",fd);
  close(fd);
  return EPMD_FALSE;
}

static int conn_close_fd(EpmdVars *g,int fd)
{
  int i;

  for (i = 0; i < g->max_conn; i++)
    if (g->conn[i].fd == fd)
      {
	epmd_conn_close(g,&g->conn[i]);
	return EPMD_TRUE;
      }
  
  return EPMD_FALSE;
}


int epmd_conn_close(EpmdVars *g,Connection *s)
{
  dbg_tty_printf(g,2,"closing connection on file descriptor %d",s->fd);

  FD_CLR(s->fd,&g->orig_read_mask);
  close(s->fd);			/* Sometimes already closed but close anyway */
  s->open = EPMD_FALSE;
  if (s->buf != NULL) {		/* Should never be NULL but test anyway */
    free(s->buf);
  }
  g->active_conn--;
  return EPMD_TRUE;
}

/****************************************************************************
 *
 *  Handle database with data for each registered node
 *
 ****************************************************************************/


static void node_init(EpmdVars *g)
{
  g->nodes.reg         = NULL;
  g->nodes.unreg       = NULL;
  g->nodes.unreg_tail  = NULL;
  g->nodes.unreg_count = 0;
}


/* We have got a close on a connection and it may be a
   EPMD_ALIVE_CLOSE_REQ. Note that this call should be called
   *before* calling conn_close() */

static int node_unreg(EpmdVars *g,char *name)
{
  Node **prev = &g->nodes.reg;	/* Point to cell pointing to... */
  Node *node  = g->nodes.reg;	/* Point to first node */

  for (; node; prev = &node->next, node = node->next)
    if (strcmp(node->symname, name) == 0)
      {
	dbg_tty_printf(g,1,"unregistering '%s:%d', port %d",
		       node->symname, node->creation, node->port);

	*prev = node->next;	/* Link out from "reg" list */

	if (g->nodes.unreg == NULL) /* Link into "unreg" list */
	  g->nodes.unreg = g->nodes.unreg_tail = node;
	else
	  {
	    g->nodes.unreg_tail->next = node;
	    g->nodes.unreg_tail = node;
	  }

	g->nodes.unreg_count++;

	node->next = NULL;	/* Last in list == first in FIFO queue */

	print_names(g);

	return node->fd;
      }

  dbg_tty_printf(g,1,"trying to unregister node with unknown name %s", name);
  return -1;
}


static int node_unreg_sock(EpmdVars *g,int fd)
{
  Node **prev = &g->nodes.reg;	/* Point to cell pointing to... */
  Node *node  = g->nodes.reg;	/* Point to first node */

  for (; node; prev = &node->next, node = node->next)
    if (node->fd == fd)
      {
	dbg_tty_printf(g,1,"unregistering '%s:%d', port %d",
		       node->symname, node->creation, node->port);

	*prev = node->next;	/* Link out from "reg" list */

	if (g->nodes.unreg == NULL) /* Link into "unreg" list */
	  g->nodes.unreg = g->nodes.unreg_tail = node;
	else
	  {
	    g->nodes.unreg_tail->next = node;
	    g->nodes.unreg_tail = node;
	  }

	g->nodes.unreg_count++;

	node->next = NULL;	/* Last in list == first in FIFO queue */

	print_names(g);

	return node->fd;
      }

  dbg_tty_printf(g,1,
		 "trying to unregister node with unknown file descriptor %d",
		 fd);
  return -1;
}

/*
 *  Finding a node slot and a (name,creation) name is a bit tricky.
 *  We try in order
 *
 *  1. If the name was used before and we can reuse that slot but use
 *     a new "creation" digit in the range 1..3.
 *
 *  2. We try to find a new unused slot.
 *
 *  3. We try to use an used slot this isn't used any longer.
 *     FIXME: The criteria for *what* slot to steal should be improved.
 *     Perhaps use the oldest or something.
 */

static Node *node_reg2(EpmdVars *g,
		       char* name,
		       int fd,
		       int port,
		       unsigned char nodetype,
		       unsigned char protocol,
		       int highvsn,
		       int lowvsn,
		       int extralen,
		       char* extra)
{
  Node *prev;			/* Point to previous node or NULL */
  Node *node;			/* Point to first node */

  /* Can be NULL; means old style */
  if (extra == NULL)
     extra = "";

  /* Fail if node name is too long */

  if (strlen(name) > MAXSYMLEN)
    {
      dbg_printf(g,0,"node name is too long (%d) %s", strlen(name), name);
      return NULL;
    }
  if (extralen > MAXSYMLEN)
    {
      dbg_printf(g,0,"extra data is too long (%d) %s", strlen(name), name);
      return NULL;
    }

  /* Fail if it is already registered */

  for (node = g->nodes.reg; node; node = node->next)
    if (strcmp(node->symname, name) == 0)
      {
	dbg_printf(g,0,"node name already occupied %s", name);
	return NULL;
      }

  /* Try to find the name in the used queue so that we
     can change "creation" number 1..3 */

  prev = NULL;

  for (node = g->nodes.unreg; node; prev = node, node = node->next)
    if (strcmp(node->symname, name) == 0)
      {
	dbg_tty_printf(g,1,"reusing slot with same name '%s'", node->symname);

	if (prev == NULL)	/* First in list matched */
	  {
	    if (node->next == NULL) /* Only one element */
	      g->nodes.unreg = g->nodes.unreg_tail = NULL;
	    else
	      g->nodes.unreg = node->next;
	  }
	else
	  {
	    if (node->next == NULL) /* Last in list */
	      {
		g->nodes.unreg_tail = prev; /* Point to new last */
		prev->next = NULL; /* New last has no next */
	      }
	    else
	      prev->next = node->next; /* Simple link out from list */
	  }

	g->nodes.unreg_count--;

	/* When reusing we change the "creation" number 1..3 */

	node->creation = node->creation % 3 + 1;

	break;
      }

  if (node == NULL)
    {
      /* A new name. If the "unreg" list is too long we steal the
	 oldest node structure and use it for the new node, else
	 we allocate a new node structure */

      if ((g->nodes.unreg_count > MAX_UNREG_COUNT) ||
	  (g->debug && (g->nodes.unreg_count > DEBUG_MAX_UNREG_COUNT)))
	{
	  /* MAX_UNREG_COUNT > 1 so no need to check unreg_tail */
	  node = g->nodes.unreg;	/* Take first == oldest */
	  g->nodes.unreg = node->next; /* Link out */
	  g->nodes.unreg_count--;
	}
      else
	{
	  if ((node = (Node *)malloc(sizeof(Node))) == NULL)
	    {
	      dbg_printf(g,0,"epmd: Insufficient memory");
	      exit(1);
	    }

	  node->creation = (current_time(g) % 3) + 1; /* "random" 1-3 */
	}
    }

  node->next = g->nodes.reg; /* Link into "reg" queue */
  g->nodes.reg  = node;

  node->fd       = fd;
  node->port     = port;
  node->nodetype = nodetype;
  node->protocol = protocol;
  node->highvsn  = highvsn;
  node->lowvsn   = lowvsn;
  node->extralen = extralen;
  memcpy(node->extra,extra,extralen);
  strcpy(node->symname,name);
  FD_SET(fd,&g->orig_read_mask);

  if (highvsn == 0) {
    dbg_tty_printf(g,1,"registering '%s:%d', port %d",
		   node->symname, node->creation, node->port);
  } else {
    dbg_tty_printf(g,1,"registering '%s:%d', port %d",
		   node->symname, node->creation, node->port);
    dbg_tty_printf(g,1,"type %d proto %d highvsn %d lowvsn %d",
		   nodetype, protocol, highvsn, lowvsn);
  }      

  print_names(g);

  return node;
}
  

static time_t current_time(EpmdVars *g)
{
  time_t t = time((time_t *)0);
  dbg_printf(g,3,"time in seconds: %d",t);
  return t;
}


static int reply(EpmdVars *g,int fd,char *buf,int len)
{
  int val;

  if (len < 0)
    {
      dbg_printf(g,0,"Invalid length in write %d",len);
      return -1;
    }

  if (g->delay_write)		/* Test of busy server */
    sleep(g->delay_write);

  val = write(fd,buf,len);
  if (val < 0)
    dbg_perror(g,"error in write");
  else if (val != len)
    dbg_printf(g,0,"could only send %d bytes out of %d to fd %d",val,len,fd);

  dbg_print_buf(g,buf,len);

  return val;
}
      

#define LINEBYTECOUNT 16

static void print_buf_hex(unsigned char *buf,int len,char *prefix)
{
  int rows, row;

  rows = len / LINEBYTECOUNT; /* Number of rows */
  if (len % LINEBYTECOUNT)
    rows++;			/* If leftovers, add a line for them */

  for (row = 0; row < rows; row++)
    {
      int rowstart = row * LINEBYTECOUNT;
      int rowend   = rowstart + LINEBYTECOUNT;
      int i;

      fprintf(stderr,"%s%.8x",prefix,rowstart);

      for (i = rowstart; i < rowend; i++)
	{
	  if ((i % (LINEBYTECOUNT/2)) == 0)
	    fprintf(stderr," ");

	  if (i < len)
	    fprintf(stderr," %.2x",buf[i]);
	  else
	    fprintf(stderr,"   ");
	}

      fprintf(stderr,"  |");

      for (i = rowstart; (i < rowend) && (i < len); i++)
	{
	  int c = buf[i];

	  /* Used to be isprint(c) but we want ascii only */

	  if ((c >= 32) && (c <= 126))
	    fprintf(stderr,"%c",c);
	  else
	    fprintf(stderr,".");
	}

      fprintf(stderr,"|\r\n");
    }
}

static void dbg_print_buf(EpmdVars *g,char *buf,int len)
{
  int plen;

  if ((g->is_daemon) ||		/* Don't want to write to stderr if daemon */
      (g->debug < 2))		/* or debug level too low */
    return;

  dbg_tty_printf(g,1,"got %d bytes",len);

  plen = len > 1024 ? 1024 : len; /* Limit the number of chars to print */

  print_buf_hex((unsigned char*)buf,plen,"***** ");

  if (len != plen)
    fprintf(stderr,"***** ......and more\r\n");
}

static void print_names(EpmdVars *g)
{
  int count = 0;
  Node *node;

  if ((g->is_daemon) ||		/* Don't want to write to stderr if daemon */
      (g->debug < 3))		/* or debug level too low */
    return;

  for (node = g->nodes.reg; node; node = node->next)
    {
      fprintf(stderr,"*****     active name     \"%s#%d\" at port %d, fd = %d\r\n",
	      node->symname, node->creation, node->port, node->fd);
      count ++;
    }

  fprintf(stderr, "*****     reg calculated count  : %d\r\n", count);

  count = 0;

  for (node = g->nodes.unreg; node; node = node->next)
    {
      fprintf(stderr,"*****     old/unused name \"%s#%d\"\r\n",
	      node->symname, node->creation);
      count ++;
    }

  fprintf(stderr, "*****     unreg counter         : %d\r\n",
	  g->nodes.unreg_count);
  fprintf(stderr, "*****     unreg calculated count: %d\r\n", count);
}
