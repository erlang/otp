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
/**
 * A Holder class for IDL's out/inout argument passing modes for double
 *
 */
package com.ericsson.otp.ic;

/**

Holder class for Double, according to OMG-IDL java mapping.

**/ 

final public class DoubleHolder implements Holder  {
    public double value;
    
    public DoubleHolder() {}
    
    public DoubleHolder(double initial) {
	value = initial;
    }

    /* Extra methods not in standard. */ 
    /**
      Comparisson method for Doubles.
      @return true if the input object equals the current object, false otherwize
      **/
    public boolean equals( Object obj ) {
	if( obj instanceof Double )
	    return ( value == ((Double)obj).doubleValue());
	else
	    return false;
    }

    /**
      Comparisson method for Doubles.
      @return true if the input double value equals the value of the current object, false otherwize
      **/
    public boolean equals( double d ) {
	return ( value == d);
    }
    
}
