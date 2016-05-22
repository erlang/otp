/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
 *
 */

/* Just include the interface function */
#include "rmod_random.h"


/* Assign your own node name here */
#define CLNODENAME "c50"
#define SNODENAME "babbis"
#define SREGNAME "rmod_random_impl"
#define COOKIE "flash"
#define INBUFSZ 1024
#define OUTBUFSZ 1024
#define HOSTNAMESZ 256



/* Stopping node */
void client_exit(CORBA_Environment *env) {

  /* Free env & buffers */
  CORBA_free(env->_inbuf);
  CORBA_free(env->_outbuf);
  CORBA_free(env);

  erl_close_connection(env->_fd);
  exit(1);
}


int main(){
  
  double result=0;
  int i=0;
  int error = 0;
  erlang_pid pid;
  char host[HOSTNAMESZ];
  char server_node[HOSTNAMESZ];
  char client_node[HOSTNAMESZ];
  CORBA_Environment *env;
  
  /* Initiate names */
#ifdef __WIN32__
  WORD wVersionRequested;
  WSADATA wsaData;

  wVersionRequested = MAKEWORD(1, 1);
  if ((error = WSAStartup(wVersionRequested, &wsaData))) {
      fprintf(stderr,"Can't initialize windows sockets: %d",error);
      return 0;
  }
#endif
  error = gethostname(host,HOSTNAMESZ);
  if (error) {
#ifdef __WIN32__
      fprintf(stderr,"can't find own hostname (error = %ld) !\n",WSAGetLastError());
#else /* not __WIN32__ */
      fprintf(stderr,"can't find own hostname !\n");
#endif
  }
  sprintf(client_node,"%s@%s",CLNODENAME,host);
  sprintf(server_node,"%s@%s",SNODENAME,host);
    
  /* Create and init CORBA_Environment */
  env = CORBA_Environment_alloc(INBUFSZ,OUTBUFSZ);
  
  /* Initiating the connection */
  erl_init(NULL,0);
  erl_connect_init(50,COOKIE,0);

  /* Initiating pid*/
  strcpy(pid.node,client_node);
  pid.num = 99;
  pid.serial = 0;
  pid.creation = 0;

  /* Fixing environment variable */
  env->_fd=erl_connect(server_node);
  strcpy(env->_regname,SREGNAME);
  env->_to_pid = NULL;
  env->_from_pid = &pid;
  
  if (env->_fd < 0) {
    fprintf(stderr,"Error : Cannot connect to Server\n");

    /* Free env & buffers */
    CORBA_free(env->_inbuf);
    CORBA_free(env->_outbuf);
    CORBA_free(env);
    exit(1);
  }
  
  /* Calling the init function */
  rmod_random_init(NULL, 1, 2, 3, env);
 
  switch(env->_major) {
  case CORBA_NO_EXCEPTION: /* Success */
    printf("Init complete !\n");
    break;
  case CORBA_SYSTEM_EXCEPTION: /* System exception */
    printf("Init call failure, reason : %s\n",(char *) CORBA_exception_value(env));
    CORBA_exception_free(env);
    client_exit(env);
  default: /* Should not come here */
    client_exit(env);
  }

  /* Calling the produce function */
  for(i=1; i<=10; i++) {
    result = rmod_random_produce(NULL, env);

    switch(env->_major) {
    case CORBA_NO_EXCEPTION: /* Success */
      break;
    case CORBA_SYSTEM_EXCEPTION: /* System exception */
      printf("Init call failure, reason : %s\n",(char *) CORBA_exception_value(env));
      CORBA_exception_free(env);
      client_exit(env);
    default: /* Should not come here */
      client_exit(env);
    }

    printf("the random number nr%d is %f\n",i,result);
  }

  /* Closing the connection */
  erl_close_connection(env->_fd);
  
  /* Free env & buffers */
  CORBA_free(env->_inbuf);
  CORBA_free(env->_outbuf);
  CORBA_free(env);

  return 0;
}
