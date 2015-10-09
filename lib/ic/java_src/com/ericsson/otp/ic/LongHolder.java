/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
 * A Holder class for IDL's out/inout argument passing modes for long
 *
 */
package com.ericsson.otp.ic;

/**

Holder class for Long, used by the Term class.

**/ 

final public class LongHolder implements Holder  {
    public long value;
    
    public LongHolder() {}
    
    public LongHolder(long initial) {
	value = initial;
    }

    /**
      Comparisson method for Longs.
      @return true if the input object equals the current object, false otherwize
      **/
    public boolean equals( Object obj ) {
	if( obj instanceof Long )
	    return ( value == ((Long)obj).longValue());
	else
	    return false;
    }

    /**
      Comparisson method for Longs.
      @return true if the input long value equals the value of the current object, false otherwize
      **/
    public boolean equals( long l ) {
	return ( value == l);
    }
    
}
