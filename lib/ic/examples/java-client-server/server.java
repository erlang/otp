/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
 *
 */
public class server {

  private static java.lang.String SNode = "babbis";
  private static java.lang.String Cookie = "flash";
  private static java.lang.String RegName = "rmod_random_impl";

  public static void main(String[] args) {

    
    System.out.println("\nServer running.\n");
    boolean serverState = true;
    boolean recState = true;

    try {
      
      com.ericsson.otp.erlang.OtpServer self = new com.ericsson.otp.erlang.OtpServer(SNode, Cookie);
      self.publishPort();
      
      /* Server loop */
      while(serverState == true) { 

	com.ericsson.otp.erlang.OtpConnection connection = self.accept();
	serverImpl srv = new serverImpl();
	com.ericsson.otp.erlang.OtpInputStream request;
	com.ericsson.otp.erlang.OtpOutputStream reply;
	com.ericsson.otp.erlang.OtpErlangPid client;
	
	/* Server loop */
	while(recState == true) { 
	  
	  if (connection.isConnected() == true)
	    try {
	      
	      request = connection.receiveBuf();
	      
	      reply = srv.invoke(request);
	      
	      if (reply != null) {
		client = srv.__getCallerPid();
		
		connection.sendBuf(client,reply);
	      }
	      
	    } catch( Exception e) {
	      System.out.println("Server terminated.\n\n");
	      recState = false;
	      serverState = false;
	    }
	}
	
	connection.close();
      }
      
    } catch( Exception e) {
      System.out.println("Initialization exception :");
      e.printStackTrace();
    }
  }
}




