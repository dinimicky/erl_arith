%% @author Brilliant
%% @doc @todo Add description to perm_comb.


-module(perm_comb).

%% ====================================================================
%% API functions
%% ====================================================================
-export([combination/2, permutation/2]).

permutation(List, Number) when length(List) >= Number-> 
	do_permutation(List, Number, [[]]);
permutation(_List, _Number) ->
	wrong_number.


combination(List, Number) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
do_permutation(_List, 0, Result) ->
	Result;
do_permutation(List, Number, Result) ->
	NewResult = [[Ele|RList]||RList <- Result, Ele <- List, lists:member(Ele, RList) =:= false],
	do_permutation(List, Number - 1, NewResult).

