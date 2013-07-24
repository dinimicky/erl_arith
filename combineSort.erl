%% Author: Administrator
%% Created: 2012-5-10
%% Description: TODO: Add description to combineSort
-module(combineSort).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([combineSort/1, combine2SortedList/2]).
-export([testSeq/1]).
%%
%% API Functions
%%
testSeq(N)->
    lists:seq(9*N, 8*N, -1)++lists:seq(7*N, 8*N)++lists:seq(1, N).

combineSort(List)->
     do_combineSort([[Ele]||Ele<-List], []).

do_combineSort([A,B|T]=_List, Result)->
     do_combineSort(T, [combine2SortedList(A,B)|Result]);
do_combineSort([A]=_List, Result)->
     do_combineSort([A|Result], []);
do_combineSort([]=_List, Result)->
     if
          length(Result)=:=1 ->
               hd(Result);
          length(Result)=:=0 ->
               [];
          true ->
               do_combineSort(Result, [])
     end.
    
combine2SortedList([H1|T1]=List1, [H2|T2]=List2)->
     if
          H1 > H2 ->
               [H2|combine2SortedList(List1, T2)];
          H1 < H2 ->
               [H1|combine2SortedList(T1, List2)];
          true ->
               [H1, H2|combine2SortedList(T1, T2)]
     end;
combine2SortedList([], List2)->
     List2;
combine2SortedList(List1, [])->
     List1.

%%
%% Local Functions
%%


