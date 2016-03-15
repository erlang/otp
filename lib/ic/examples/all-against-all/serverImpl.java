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
public class serverImpl extends rmod._randomImplBase {
  
  java.util.Random random = null;

  
  public void init(int seed1, int seed2, int seed3) throws java.lang.Exception {
    
    random = new java.util.Random(seed1+seed2+seed3);
  };


  public double produce() throws java.lang.Exception {
  
    return random.nextDouble();
  }

}






