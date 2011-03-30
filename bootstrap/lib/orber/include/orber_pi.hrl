%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%%----------------------------------------------------------------------
%% File    : orber_pi.hrl
%% Purpose : 
%%----------------------------------------------------------------------

%%=============== CONSTANTS ==================================
%%-------- PortableInterceptor::Interceptor (local) ----------
%% Reply Status
-define('PortableInterceptor_SUCCESSFUL',                 0).
-define('PortableInterceptor_SYSTEM_EXCEPTION',           1).
-define('PortableInterceptor_USER_EXCEPTION',             2).
-define('PortableInterceptor_LOCATION_FORWARD',           3).
-define('PortableInterceptor_LOCATION_FORWARD_PERMANENT', 4).
-define('PortableInterceptor_TRANSPORT_RETRY',            5).


%%=============== EXCEPTIONS =================================
%%-------- PortableInterceptor::Interceptor (local) ----------
%% forward eq. CORBA::Object, premanent eq. boolean
-record('PortableInterceptor_ForwardRequest', {'OE_ID'="local", forward, permanent}).
-record('PortableInterceptor_InvalidSlot', {'OE_ID'="local"}).

%%--------------- IOP_N::Codec (local) -----------------------
-record('IOP_N_Codec_InvalidTypeForEncoding', {'OE_ID'="local"}).
-record('IOP_N_Codec_FormatMismatch', {'OE_ID'="local"}).
-record('IOP_N_Codec_TypeMismatch', {'OE_ID'="local"}).

%%--------------- IOP_N (Module level) -----------------------
-define('IOP_N_ENCODING_CDR_ENCAPS', 0).

%%--------------- IOP_N::CodecFactory (Module level) ---------
-record('IOP_N_CodecFactory_UnknownEncoding', {'OE_ID'="local"}).

%%--------------- IOP_N::ORBInitInfo (Module level) ----------
%% name eq. string()
-record('PortableInterceptor_ORBInitInfo_DuplicateName', {'OE_ID'="local", name}).
-record('PortableInterceptor_ORBInitInfo_InvalidName', {'OE_ID'="local"}).


%%=============== DATA STRUCTURES ============================
%%--------------- IOP_N (Module level) -----------------------
-record('IOP_N_Encoding', 
	{format,          %% Currently only 'IOP_N_ENCODING_CDR_ENCAPS' allowed.
	 major_version,   %% 1 only
	 minor_version}). %% 0,1 or 2


%%--------------- Dynamic (Module level) ---------------------
%% argument eq. #any{}, 
%% mode eq. CORBA::ParameterMode - PARAM_IN, PARAM_OUT, PARAM_INOUT.
-record('Dynamic_Parameter', 
	{argument, 
	 mode}).

%%--------------- END OF MODULE ------------------------------

