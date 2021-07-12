-module(parse_transform).

%%------------------------------------------------------------------
%% Parse Transform Behaviour
%% ------------------------------------------------------------------

-callback parse_transform(Forms, Options) -> Forms
          when Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
               Options :: [compile:option()].
