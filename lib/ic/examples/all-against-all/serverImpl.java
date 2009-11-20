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
public class serverImpl extends rmod._randomImplBase {
  
  java.util.Random random = null;

  
  public void init(int seed1, int seed2, int seed3) throws java.lang.Exception {
    
    random = new java.util.Random(seed1+seed2+seed3);
  };


  public double produce() throws java.lang.Exception {
  
    return random.nextDouble();
  }

}






