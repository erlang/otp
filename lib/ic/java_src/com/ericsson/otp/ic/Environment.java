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
/**
 * The Environment class for Java IDL
 *
 */
package com.ericsson.otp.ic;

/**

  The Environment class handles communication 
  setup and stub state. The methods of this class
  are specially designed for the generated stubs.
  This class must be used when designing asynchronous
  message passing.

  **/
  

public class Environment {

  // Private variables
  private com.ericsson.otp.erlang.OtpSelf self;
  private com.ericsson.otp.erlang.OtpPeer peer;
  private java.lang.Object server;
  private java.lang.String cookie;
  private com.ericsson.otp.erlang.OtpConnection connection;
  private com.ericsson.otp.erlang.OtpErlangRef send_ref;    /* Client side send reference */
  private com.ericsson.otp.erlang.OtpErlangRef receive_ref; /* Client side received reference */
  private com.ericsson.otp.erlang.OtpErlangPid clientP;
  private com.ericsson.otp.erlang.OtpErlangPid serverP;
  private com.ericsson.otp.erlang.OtpOutputStream os;       /* Output stream */
  private com.ericsson.otp.erlang.OtpInputStream is;        /* Input stream */
  private boolean stopped;
 
  // Private variables used by server only
  private int tag;
  private java.lang.String operation;
  private java.lang.String type;
  private com.ericsson.otp.erlang.OtpErlangRef ref;         /* Server side client reference */
  private com.ericsson.otp.erlang.OtpErlangPid caller;      /* Server side client pid */
 
  // Tags to distiguish client / server environments 
  private boolean clientT;
  private boolean serverT;


  /**
    Client stub side constructor.
    **/
  public Environment(com.ericsson.otp.erlang.OtpSelf _Self,
		     com.ericsson.otp.erlang.OtpPeer _Peer,
		     java.lang.Object _Server) throws java.lang.Exception {
		       
		       init();
		       clientT = true;
		       self = _Self;
		       peer = _Peer;
		       server = _Server;
		       os = new com.ericsson.otp.erlang.OtpOutputStream();
  }


  /**
    Client stub side constructor.
    **/  
  public Environment(java.lang.String _SelfNode,
		     java.lang.String _PeerNode,
		     java.lang.String _Cookie,
		     java.lang.Object _Server) throws java.lang.Exception {
		       
		       init();
		       clientT = true;
		       self = new com.ericsson.otp.erlang.OtpSelf(_SelfNode, _Cookie);
		       peer = new com.ericsson.otp.erlang.OtpPeer(_PeerNode);
		       cookie = _Cookie;
		       server = _Server;
		       os = new com.ericsson.otp.erlang.OtpOutputStream();
  }


  /**
    Client stub side constructor.
    **/  
  public Environment(com.ericsson.otp.erlang.OtpConnection _connection,
		     java.lang.Object _Server) throws java.lang.Exception {
		       
		       init();
		       clientT = true;
		       self = _connection.self();
		       peer = _connection.peer();
		       connection = _connection;
		       server = _Server;
		       os = new com.ericsson.otp.erlang.OtpOutputStream();
  }
  

  /** 
    Server skeleton side constructor.
    **/
  public Environment() throws java.lang.Exception {
    
    init();
    serverT = true;
    stopped = false;
    os = new com.ericsson.otp.erlang.OtpOutputStream();

  }

  
  /* Communication toolbox */
  
  /**
    Client stub side connector.
    **/
  public void connect() throws java.lang.Exception {
    
    if (connection == null)
      connection = self.connect(peer);
 
    clientP = self.createPid(); /* This is not perfect */
    send_ref = self.createRef();
    
  }

  /**
    Reconnects a client by closing existing connection 
    and connecting.
    **/
  public void reconnect() throws java.lang.Exception {

    if (connection.isConnected())
	connection.close();

    connection = self.connect(peer);

  }
  
  /**
    Closes the established connection.
    **/
  public void disconnect() {
    
    connection.close();
    
  }


  /**
    Client side message sender.
    **/
  public void send() throws java.lang.Exception {
    
    if (server instanceof java.lang.String)
      connection.sendBuf((java.lang.String)server, os);
    else
      connection.sendBuf((com.ericsson.otp.erlang.OtpErlangPid)server, os);
    
  }
  
  
  /**
    Client message receiver.
    **/
  public void receive() throws java.lang.Exception {
    
    is = connection.receiveBuf();
    
    if (clientT) { // If client, decode message reference too
      is.read_tuple_head();
      receive_ref = is.read_ref();      
    }
  }


  /**
    Universal message receiver.
    **/
  public void receive(com.ericsson.otp.erlang.OtpConnection _connection) throws java.lang.Exception {
    
    is = _connection.receiveBuf();
    
    if (clientT) { // If client, decode message reference too
      is.read_tuple_head();
      receive_ref = is.read_ref();      
    }
  }

  
  /* Accessors */
   
    /**
       Server RegName/OtpErlangPid accessor. 
       Used to access the server Reg/Pid, which 
       initiated the connection.
       @return java.lang.Object, the server for the active OtpConnection.
    **/
  public java.lang.Object server() {
    
    return server;
    
  }

  /**
    Caller identity accessor. Used by a server stub to access the 
    caller identity of the received message.
    @return OtpErlangPid, the caller identity.
    **/
  public com.ericsson.otp.erlang.OtpErlangPid caller_pid() {
    
    return clientP;
    
  }


  /**
    Received message reference accessor.  Used by a server stub to access the 
    reference of the received message.
    @return OtpErlangRef, the reference of the received message.
    **/
  public com.ericsson.otp.erlang.OtpErlangRef received_ref() {
    
    return receive_ref;
    
  }


  /* Encoders */

  /**
    Client Pid Encoder.  Used by a server stub to encode the 
    enclosed client process identity.
    **/
  public void write_client_pid() {

    os.write_pid(clientP.node(),clientP.id(),clientP.serial(),clientP.creation());
    
  }

  /** 
    Client Ref Encoder. Used by a server stub to encode the 
    enclosed client message reference.
    **/
  public void write_client_ref() {
    
    os.write_ref(send_ref.node(),send_ref.id(),send_ref.creation());
    
  }



  /* Field access functions */

  /**
    Output Stream accessor. 
    @return OtpOutputStream, the enclosed output stream.
    **/
  public com.ericsson.otp.erlang.OtpOutputStream getOs() {
    return os;
  }

  /**
    Input Stream accessor. 
    @return OtpInputStream, the enclosed input stream.
    **/
  public com.ericsson.otp.erlang.OtpInputStream getIs() {
    return is;
  }

  /**
    Server skeleton side client (caller) pid accessor.
    @return OtpErlangPid, the caller process identity.
    **/
  public com.ericsson.otp.erlang.OtpErlangPid getScaller() {
    return caller;
  }

  /**
    Server skeleton side client call reference accessor.
    @return OtpErlangRef, the latest call message reference.
    **/
  public com.ericsson.otp.erlang.OtpErlangRef getSref() {
    return ref;
  }
  


  /* Field modifiers */
  


  /* Decoders */
  
  /**
    Decodes the message head from existing stream.
    Assignes message data to private variables of the Environment Object.
    **/
  public void uHead() throws java.lang.Exception {
    uHead(is);
  }

  /**
    Decodes the message head and writes over input stream.
    Assignes message data to private variables of the Environment Object.
    **/
  public void uHead(com.ericsson.otp.erlang.OtpInputStream _is) throws java.lang.Exception {
    
    is = _is;
    is.read_tuple_head();
    type = is.read_atom();
    
    if (type.equals("$gen_call")) {  // Call type operation
      is.read_tuple_head();
      caller = is.read_pid();
      ref = is.read_ref();
      tag = is.peek();
      
      switch (tag) {
      case com.ericsson.otp.erlang.OtpExternal.atomTag:
      case com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag:
      case com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag:
	operation = is.read_atom();
	break;
      default:
	is.read_tuple_head();
	operation = is.read_atom();
      }
    } else {  // Cast type operation
      tag = is.peek();
      switch (tag) {
      case com.ericsson.otp.erlang.OtpExternal.atomTag:
      case com.ericsson.otp.erlang.OtpExternal.atomUtf8Tag:
      case com.ericsson.otp.erlang.OtpExternal.smallAtomUtf8Tag:
	operation = is.read_atom();
	break;
      default:
	is.read_tuple_head();
	operation = is.read_atom();
      }
    }
  }

  /**
    Operation label accessor.
    @return int, the label hash value.
    **/
  public int uLabel(java.util.Dictionary _operations) {

    java.lang.Integer __label =
      (java.lang.Integer) _operations.get(operation);
    
    if(__label == null) 
      return -1;

    return __label.intValue();
  }



  /* Controllers */

  /**
    Operation controller.
    @return boolean, true if the operation variable found in Environment class
    is supported in the input operation dictionary, false otherwize.
    **/
  public boolean validOp(java.util.Dictionary _operations) {
    
    if((_operations.get(operation)) == null)
      return false;
    
    return true; 
  }


  /**
    Server stop request controller.
    @return boolean, true if there is a client request for the server
    to be stopped, false otherwize.
    **/
    public boolean isStopped() {
	return stopped;
    };


  
  /* Destroy functions */ 

    /* 
     Creates and sends a stop message.
     Called by client stub to terminate the server.
    */
  public void client_stop_server() 
      throws java.lang.Exception {

     // Message header assembly
     os.reset();
     os.write_tuple_head(2);
     os.write_atom("$gen_cast");

     os.write_atom("stop");

     send();
	
  }

  /*
  Sets the stop flag for the server.
  Called by server skeleton when stop message is received.
  */
  public void server_stop_server() {

      // Note at server is dead !
      stopped = true;
  }


  /* Private methods */

  /**
    Private variable initialization.
    **/
  public void init() {

    clientT = false;
    serverT = false;
    stopped = false;
    self = null;
    peer = null;
    server = null;
    cookie = null;
    connection = null;
    clientP = null;
    serverP = null;
    send_ref = null;
    receive_ref = null;
    os = null;
    is = null;

    tag = -1;
    operation = null;
    type = null;

  };

}
















