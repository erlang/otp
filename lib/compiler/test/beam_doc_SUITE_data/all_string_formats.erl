-module(all_string_formats).

-export([one/0,two/0,three/0,four/0,five/0,six/0]).

-moduledoc """
  Moduledoc test module
  """.

-doc ~S"Doc test module".
one() -> ok.
-doc ~B"Doc test module".
two() -> ok.
-doc <<"Doc","test","modul",$e>>.
three() -> ok.
-doc <<"Doc test mödule"/utf8>>.
four() -> ok.
-doc <<?MODULE_STRING, "-Doc module">>.
five() -> ok.
-doc ?MODULE_STRING "-" ?MODULE_STRING.
six() -> ok.
