%% @author ezonghu
%% @doc @todo Add description to ld.


-module(ld).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ld1/2, estimateRuntime/3, ld/2, ld2/2, wf/2]).
ld1(StrS, StrT) ->
  Result = do_ld(StrS, StrT, 0, []),
	lists:min(Result). 
ld2(StrS, StrT) ->
	do_ld2(StrS, StrT, 0, -1).

ld([], StrT) ->
	length(StrT);
ld(StrS, []) ->
	length(StrS);
ld([HeadS|TailS] = StrS, [HeadT|TailT]=StrT) ->
	if 
		HeadS =:= HeadT -> Cost = 0;
		true -> Cost = 1
	end,
	lists:min([ld(TailS, StrT)+1, ld(StrS, TailT) +1, ld(TailS, TailT) +  Cost]).

wf(StrS, StrT) ->
	LenS = length(StrS),
	LenT = length(StrT),
	if
		LenS > LenT -> 
			do_wf(StrT, StrS, lists:seq(0, LenT), 1);
		true -> 
			do_wf(StrS, StrT, lists:seq(0, LenS), 1)
	end.

do_wf(_StrS, [], CurrS, _Index) ->
	lists:last(CurrS);	
do_wf(StrS, [CharT|StrTTail] = _StrT, CurrS, Index) ->
	NewCurrS = do_wf_row(StrS, CharT, CurrS, [Index]),
	do_wf(StrS, StrTTail, NewCurrS, Index + 1).

%% do_wf_row(PrevS,  [null | StrSTail] = _StrS, CharT, CurrS) -> 
%% 	do_wf_row(PrevS, StrSTail, CharT, CurrS);
do_wf_row([], _CharT, _PrevS, CurrS) ->
	lists:reverse(CurrS);
do_wf_row([StrSHead|StrSTail] = _StrS, CharT, [PrevSHead1, PrevSHead2|PrevSTail] = _PrevS, [CurrSHead|_CurrSTail] = CurrS) ->
	Cost = if 
			   StrSHead =:= CharT -> 0;
			   true -> 1
		   end,
	NewCurrSHead = lists:min([PrevSHead1 + Cost, PrevSHead2 + 1, CurrSHead + 1]),
	do_wf_row(StrSTail, CharT, [PrevSHead2|PrevSTail], [NewCurrSHead|CurrS]).

	

estimateRuntime(M, F, A)->
	Start=erlang:localtime(),
	statistics(runtime),
	statistics(wall_clock),
	erlang:apply(M, F, A),
%%     process_file("C-large-practice.in", "C-large-practice.out"),
	{_, Time1} = statistics(runtime),
	{_, Time2} = statistics(wall_clock),
	End=erlang:localtime(),
	U1 = Time1 ,
	U2 = Time2 ,
	io:format("Process spawn time=~p (~p) ms~n", [U1, U2]),
	io:format("start time=~p end time=~p~n", [Start, End]).
%% ====================================================================
%% Internal functions
%% ====================================================================
do_ld([], StrT, Result, ResultList) ->
	Len = length(StrT),
	[Result + Len|ResultList];
do_ld(StrS, [], Result, ResultList) ->
	Len = length(StrS),
	[Result + Len|ResultList];
do_ld([HeadS|TailS] = StrS, [HeadT|TailT] = StrT, Result, ResultList) -> 
	if 
		HeadS =:= HeadT -> Cost = 0;
		true -> Cost = 1
	end,
	
	ResultList1 = do_ld(TailS, StrT, Result + 1, ResultList),
	ResultList2 = do_ld(StrS, TailT, Result + 1, ResultList1),
	do_ld(TailS, TailT, Result + Cost, ResultList2).

ld_min(TmpRes, MinRes) ->
	if 
		MinRes =:= -1 -> TmpRes;
		true -> min(TmpRes, MinRes)
	end.

do_ld2([], StrT, TmpRes, MinRes) ->
	Len = length(StrT),
	ld_min(Len + TmpRes, MinRes);
do_ld2(StrS, [], TmpRes, MinRes) ->
	Len = length(StrS),
	ld_min(Len + TmpRes, MinRes);
do_ld2([HeadS|TailS] = StrS, [HeadT|TailT] = StrT, TmpRes, MinRes) -> 
	if 
		HeadS =:= HeadT -> Cost = 0;
		true -> Cost = 1
	end,
	MinRes1 = do_ld2(TailS, StrT, TmpRes + 1, MinRes),
	MinRes2 = do_ld2(StrS, TailT, TmpRes + 1, MinRes1),
	do_ld2(TailS, TailT, TmpRes + Cost, MinRes2).
		
