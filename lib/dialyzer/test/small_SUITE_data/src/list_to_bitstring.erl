%%=====================================================================
%% From: Ken Robinson
%% Date: 28/04/2011, 17:26
%%
%% Program that produced bogus "Function has no local return" warnings
%% due to erlang:list_to_bitstring/1 having erroneous hard coded type
%% information, namely accepting iolist() instead of bitstrlist().
%% Fixed 29/04/2011.
%%=====================================================================

-module(list_to_bitstring).

-export([l2bs/0, l2bs_ok/0]).

%% This function was producing a warning
l2bs() ->
    erlang:list_to_bitstring([<<42>>, <<42:13>>]).

%% while this one was ok.
l2bs_ok() ->
    erlang:list_to_bitstring([<<42>>, <<42,42>>]).
