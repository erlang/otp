%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_html_util_iso).
-export([entity_to_html/1]).

%% Encodes ISOtech, ISOnum and ISOgrk3.
%%
%% All entities are of the form "[abcdef]". 
%%
entity_to_html(Entity) when is_list(Entity), hd(Entity) =/= $[ ->
    Entity;
entity_to_html(Entity) ->
    case (catch enc(Entity)) of
	{'EXIT', _} ->
	    Entity;
	Enc  ->
	    "<font face=symbol>" ++ Enc ++ "</font>"
    end.

enc("[Delta ]") -> "&#68;";
%% enc("[Dot   ]") -> "&#0;";
%% enc("[DotDot]") -> "&#0;";
enc("[Gamma ]") -> "&#71;";
enc("[Lambda]") -> "&#76;";
enc("[Omega ]") -> "&#87;";
enc("[Phi   ]") -> "&#70;";
enc("[Pi    ]") -> "&#80;";
enc("[Prime ]") -> "&#178;";
enc("[Psi   ]") -> "&#89;";
enc("[Sigma ]") -> "&#83;";
enc("[Theta ]") -> "&#81;";
enc("[Upsi  ]") -> "&#161;";
%% enc("[Verbar]") -> "&#0;";
enc("[Xi    ]") -> "&#88;";

enc("[aleph ]") -> "&#192;";
enc("[alpha ]") -> "&#97;";
enc("[amp   ]") -> "&#38;";
enc("[and   ]") -> "&#217;";
enc("[ang   ]") -> "&#208;";
%% enc("[ang90 ]") -> "&#0;";
%% enc("[angsph]") -> "&#0;";
%% enc("[angst ]") -> "&#0;";
enc("[ap    ]") -> "&#187;";

%% enc("[becaus]") -> "&#0;";
%% enc("[bernou]") -> "&#0;";
enc("[bepsi ]") -> "&#39;";
enc("[beta  ]") -> "&#98;";
enc("[bottom]") -> "&#94;";
enc("[bull  ]") -> "&#183;";

enc("[cap   ]") -> "&#199;";
enc("[chi   ]") -> "&#99;";
enc("[clubs ]") -> "&#167;";
%% enc("[compfn]") -> "&#0;";
enc("[cong  ]") -> "&#64;";
enc("[copy  ]") -> "&#211;";
%% enc("[conint]") -> "&#0;";
enc("[cup   ]") -> "&#200;";

enc("[dArr  ]") -> "&#223;";
enc("[darr  ]") -> "&#175;";
enc("[deg   ]") -> "&#176;";
enc("[delta ]") -> "&#100;";
enc("[diam  ]") -> "&#224;";
enc("[diams ]") -> "&#168;";
enc("[divide]") -> "&#184;";

enc("[empty ]") -> "&#198;";
%% enc("[epsi  ]") -> "&#0;";
%% enc("[epsis ]") -> "&#0;";
enc("[epsiv ]") -> "&#101;";
enc("[equiv ]") -> "&#186;";
enc("[eta   ]") -> "&#104;";
enc("[exist ]") -> "&#36;";

enc("[fnof  ]") -> "&#166;";
enc("[forall]") -> "&#34;";

enc("[gamma ]") -> "&#103;";
%% enc("[gammad]") -> "&#0;";
enc("[ge    ]") -> "&#179;";
enc("[gt    ]") -> "&#62;";

%% enc("[hamilt]") -> "&#0;";
enc("[hArr  ]") -> "&#219;";
enc("[harr  ]") -> "&#171;";
enc("[hearts]") -> "&#169;";
enc("[horbar]") -> "&#190;";

enc("[iff   ]") -> "&#219;";
enc("[image ]") -> "&#193;";
enc("[infin ]") -> "&#165;";
enc("[int   ]") -> "&#242;";
enc("[iota  ]") -> "&#105;";
enc("[isin  ]") -> "&#206;";

enc("[kappa ]") -> "&#107;";
%%enc("[kappav]") -> "&#0;";

enc("[lArr  ]") -> "&#220;";
%% enc("[lagran]") -> "&#0;";
enc("[lambda]") -> "&#108;";
enc("[lang  ]") -> "&#225;";
enc("[larr  ]") -> "&#172;";
enc("[le    ]") -> "&#163;";
%% enc("[lowast]") -> "&#0;";
enc("[lowbar]") -> "&#95;";
enc("[lt    ]") -> "&#60;";

enc("[middot]") -> "&#215;";
enc("[minus ]") -> "&#45;";
%% enc("[mnplus]") -> "&#0;";
enc("[mu    ]") -> "&#109;";

enc("[nabla ]") -> "&#209;";
enc("[ne    ]") -> "&#185;";
enc("[ni    ]") -> "&#39;";
enc("[nsub  ]") -> "&#203;";
enc("[not   ]") -> "&#216;";
enc("[notin ]") -> "&#207;";
enc("[nu    ]") -> "&#110;";

enc("[omega ]") -> "&#119;";
enc("[or    ]") -> "&#218;";
%% enc("[order ]") -> "&#0;";
enc("[oplus ]") -> "&#197;";
enc("[otimes]") -> "&#196;";

%% enc("[par   ]") -> "&#0;";
enc("[part  ]") -> "&#182;";
%% enc("[permil]") -> "&#0;";
enc("[perp  ]") -> "&#94;";			% bottom
enc("[phis  ]") -> "&#102;";
enc("[phiv  ]") -> "&#106;";
%% enc("[phmmat]") -> "&#0;";
enc("[pi    ]") -> "&#112;";
enc("[piv   ]") -> "&#118;";
enc("[plus  ]") -> "&#43;";
enc("[plusmn]") -> "&#177;";
enc("[prime ]") -> "&#162;";
enc("[prod  ]") -> "&#213;";
enc("[prop  ]") -> "&#181;";
enc("[psi   ]") -> "&#121;";

enc("[radic ]") -> "&#214;";
enc("[rang  ]") -> "&#241;";
enc("[rArr  ]") -> "&#222;";
enc("[rarr  ]") -> "&#174;";
enc("[real  ]") -> "&#194;";
enc("[reg   ]") -> "&#210;";
enc("[rho   ]") -> "&#114;";
%% enc("[rhov  ]") -> "&#0;";

enc("[sigma ]") -> "&#115;";
enc("[sigmav]") -> "&#86;";
enc("[sim   ]") -> "&#126;";
%% enc("[sime  ]") -> "&#0;";
%% enc("[square]") -> "&#0;";
enc("[sol   ]") -> "&#164;";
enc("[spades]") -> "&#170;";
enc("[sub   ]") -> "&#204;";
enc("[sube  ]") -> "&#205;";
enc("[sup   ]") -> "&#201;";
enc("[supe  ]") -> "&#202;";
enc("[sum   ]") -> "&#229;";

enc("[tau   ]") -> "&#116;";
enc("[tdot  ]") -> "&#188;";
enc("[there4]") -> "&#92;";
enc("[thetas]") -> "&#113;";
enc("[thetav]") -> "&#74;";
enc("[times ]") -> "&#180;";
%% enc("[tprime]") -> "&#0;";
enc("[trade ]") -> "&#212;";

enc("[uArr  ]") -> "&#221;";
enc("[uarr  ]") -> "&#173;";
enc("[upsi  ]") -> "&#117;";

enc("[verbar]") -> "&#189;";

%% enc("[wedgeq]") -> "&#0;";
enc("[weierp]") -> "&#195;";

enc("[xi    ]") -> "&#120;";

enc("[zeta  ]") -> "&#122;".
