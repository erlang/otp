/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009. All Rights Reserved.
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
package com.ericsson.otp.erlang;

public class OtpErlangExternalFun extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = 6443965570641913886L;

    private final String module;
    private final String function;
    private final int arity;

    public OtpErlangExternalFun(final String module, final String function,
	    final int arity) {
	super();
	this.module = module;
	this.function = function;
	this.arity = arity;
    }

    public OtpErlangExternalFun(final OtpInputStream buf)
	    throws OtpErlangDecodeException {
	final OtpErlangExternalFun f = buf.read_external_fun();
	module = f.module;
	function = f.function;
	arity = f.arity;
    }

    @Override
    public void encode(final OtpOutputStream buf) {
	buf.write_external_fun(module, function, arity);
    }

    @Override
    public boolean equals(final Object o) {
	if (!(o instanceof OtpErlangExternalFun)) {
	    return false;
	}
	final OtpErlangExternalFun f = (OtpErlangExternalFun) o;
	return module.equals(f.module) && function.equals(f.function)
		&& arity == f.arity;
    }

    @Override
    protected int doHashCode() {
	OtpErlangObject.Hash hash = new OtpErlangObject.Hash(14);
	hash.combine(module.hashCode(), function.hashCode());
	hash.combine(arity);
	return hash.valueOf();
    }
    
    @Override
    public String toString() {
	return "#Fun<" + module + "." + function + "." + arity + ">";
    }

}
