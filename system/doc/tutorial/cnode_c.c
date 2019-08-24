/* cnode_c.c */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
  int fd;                                  /* fd to Erlang node */

  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *fromp, *tuplep, *fnp, *argp, *resp;
  int res;
  
  erl_init(NULL, 0);

  if (erl_connect_init(1, "secretcookie", 0) == -1)
    erl_err_quit("erl_connect_init");

  if ((fd = erl_connect("e1@idril")) < 0)
    erl_err_quit("erl_connect");
  fprintf(stderr, "Connected to ei@idril\n\r");

  while (loop) {

    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      loop = 0;
    } else {

      if (emsg.type == ERL_REG_SEND) {
	fromp = erl_element(2, emsg.msg);
	tuplep = erl_element(3, emsg.msg);
	fnp = erl_element(1, tuplep);
	argp = erl_element(2, tuplep);

	if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
	  res = foo(ERL_INT_VALUE(argp));
	} else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
	  res = bar(ERL_INT_VALUE(argp));
	}

	resp = erl_format("{cnode, ~i}", res);
	erl_send(fd, fromp, resp);

	erl_free_term(emsg.from); erl_free_term(emsg.msg);
	erl_free_term(fromp); erl_free_term(tuplep);
	erl_free_term(fnp); erl_free_term(argp);
	erl_free_term(resp);
      }
    }
  }
}
