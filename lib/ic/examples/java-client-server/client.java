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
public class client {

  private static java.lang.String SNode = "client";
  private static java.lang.String PNode = "babbis";
  private static java.lang.String Cookie = "flash";
  private static java.lang.String Server = "rmod_random_impl";
  
  private static rmod._randomStub stub;

  public static void main(String[] args) {

    try {
     
      stub = new rmod._randomStub(SNode,PNode,Cookie,Server);
      int seed1 = 1;
      int seed2 = 2;
      int seed3 = 3;
      double random = 0;
      
      System.out.print("\nClient initialization....");
      stub.init(seed1,seed2,seed3);
      System.out.println("ok\n");
      

      for (int i = 0; i < 10; i++) {
	random = stub.produce();
	System.out.println("Random" + i + " = " + random);
      }
      System.out.println("\nClient terminated.\n");
            
      stub.__disconnect();
      
    } catch( Exception e) {
      System.out.println("Exception :");
      e.printStackTrace();
    }
    
  }
  
}

