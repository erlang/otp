-module(mkdocs).
-author('mikael.karlsson@creado.com').

-export([run/1]).

run([InFile, OutFile])->
    {A,_}=xmerl_scan:file(InFile,[{fetch_fun, fun(DTDSpec,S) -> {ok,S} end}]),
      B = sdocbook2xhtml:process_xml(A),
      file:write_file(OutFile,[B]).
