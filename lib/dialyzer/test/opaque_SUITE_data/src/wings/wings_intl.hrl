%%
%%  wings_intl.hrl --
%%
%%     Defines for translations
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_intl.hrl,v 1.1 2009/01/25 18:55:33 kostis Exp $
%%

-define(STR(A,B,Str), wings_lang:str({?MODULE,A,B},Str)).
-define(__(Key,Str), wings_lang:str({?MODULE,Key},Str)).
