%% @author Brilliant
%% @doc @todo Add description to shudu.


-module(shudu).
-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================
exclusion(P, SDList)->
	{X, Y} = position(P),
	Col = column(X, SDList),
	Row = row(Y, SDList),
	Area = area(X,Y,SDList),
	lists:seq(1, 9) -- Col -- Row -- Area.
	

column(C, SDList)->
	[Ele||I<-lists:seq(0, 8), (Ele = lists:nth(I*9+C+1, SDList)) =/=0].
row(R, SDList)->
	[Ele||I<-lists:seq(0, 8), (Ele = lists:nth(R*9+I+1, SDList))=/=0].
area(X,Y, SDList)->
	X1 = X div 3 * 3,
	X2 = (X div 3 + 1) * 3,
	Y1 = Y div 3 * 3,
	Y2 = (Y div 3 + 1) * 3,
	[Ele||I<-lists:seq(X1, X2-1), J<-lists:seq(Y1, Y2-1), (Ele = lists:nth(I+J*9+1, SDList)) =/= 0].

position(I)->
	{I rem 9, I div 9}.


