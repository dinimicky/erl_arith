%% @author Brilliant
%% @doc @todo Add description to shudu.


-module(shudu2).
-compile(export_all).

-define(CROSS(AL,BL),[[A,B]||A<-AL, B<-BL]).
-define(DIGITS, "123456789").
-define(ROWS, "ABCDEFGHI").
-define(COLS, ?DIGITS).
-define(SQUARES, ?CROSS(?ROWS, ?COLS)).
-define(UNITLISTS, [?CROSS(?ROWS, [C])||C<-?COLS]++
					[?CROSS([R], ?COLS)||R<-?ROWS]++
					[?CROSS(RS, CS)||CS<-["123","456","789"], RS<-["ABC", "DEF", "GHI"]]).
-define(UNITS, [{S, [Unit||Unit<-?UNITLISTS, lists:member(S, Unit)]}||S<-?SQUARES]).
-define(PEERS, [{S, lists:umerge(SL)--[S]}||{S,SL}<-?UNITS]).
-define(GRID, "800000000003600000070090200050007000000045700000100030001000068008500010090000400").

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_grid/1, display/1,search/1, solve/1]).
solve(Grid)->
	init_process_global_var(),
	ValueDict = parse_grid(Grid),
	try search(ValueDict) 
	catch
		throw:{_, Result} -> Result
	end.
display(ValuesDict)->
	Width = 1+lists:max([length(dict:fetch(S, ValuesDict))||S<-?SQUARES]),
	Line = string:copies("+"++string:copies("-", 3*Width),3)++"+",
	Center = fun(Elem) ->
					 Head = (Width-length(Elem)) div 2,
					 Tail = Width - length(Elem) - Head,
%% 					 io:format("Elem=~p;Head=~p;Tail=~p~n", [Elem, Head, Tail]),
					 string:chars($ , Head)++Elem++string:chars($ , Tail)	  
			 end,
	StringEle = fun(R, C) ->
						Elem = dict:fetch([R,C], ValuesDict),
%% 						io:format("Elem=~p;Index=~p~n", [Elem, [R,C]]),
						case lists:member(C, "36") of
							true -> Center(Elem)++"|";
							false -> Center(Elem)
						end
				end,
	PrintRow = fun(R) -> 
					   Row = lists:flatten([StringEle(R,C)||C<-?COLS]),
					   case lists:member(R, "CF") of   
						   true -> Row++"\n"++Line++"\n";   
						   false -> Row++"\n"   
					   end
			   end,
%% 	io:format("Width=~p~n", [Width]),
	ValStr = lists:foldl(fun(R, AccIn) -> Row = PrintRow(R), AccIn++Row end, [], ?ROWS),
	lists:foldl(fun(R, ok)-> io:format("~p~n", [R]) end, ok, string:tokens(ValStr, "\n")),
	ValStr.
parse_grid(Grid)->
	Values = [{S, ?DIGITS}||S<-?SQUARES],
	ValuesDict = dict:from_list(Values),
	dict:fold(fun(S,D, AccIn) -> case lists:member(D, ?DIGITS) of
									 true ->  assign(AccIn, S, D);
									 false -> AccIn
								 end
			  end, ValuesDict, grid_values(Grid)).
search(ValuesDict)->
	case lists:all(fun(S)->case dict:fetch(S, ValuesDict) of
							   [_] -> true;
							   _ -> false
						   end
				   end, ?SQUARES) of
%% 	case dict:fold(fun(_S,D,AccIn)-> AccIn and (length(D) =:= 1) end, true, ValuesDict) of
		true -> throw({result, ValuesDict});
		false ->
			
%% 			{_MinL, MinS} = lists:min([{L,S}||S<-?SQUARES, (L=length(dict:fetch(S, ValuesDict)))>1]),
			{MinS, {MinDs, _MinL}} =
			dict:fold(fun(S,D, {_S1, {_OldD, L}}=AccIn)->
							  DL = length(D),
							  if 
								  DL>1, DL<L -> {S, {D,DL}};
								  true -> AccIn
							  end
					  end, {[], {[], 10}}, ValuesDict),
%% 			io:format("search MinS=~p;MinDs=~p~n", [MinS,MinDs]),
			lists:foreach(fun(D)-> try search(assign(ValuesDict, MinS, D)) 
								   catch throw:{error, false}->false 
								   end 
						  end, MinDs)
%% 			some([try search(assign(ValuesDict, MinS, D)) 
%% 						 catch 
%% 							 throw:{error, false}->
%% %% 								 io:format("~p~n", [False]),
%% 								 false;
%% 							 throw:{}
%% 						 end||D<-MinDs])
	end.
	
%% ====================================================================
%% Internal functions
%% ====================================================================
%% some([])->
%% 	false;
%% some([H|T]=_ValuesDicts)->
%% 	case H of
%% 		false -> some(T);
%% 		ValuesDict -> ValuesDict
%% 	end.

init_process_global_var()->
	put(peers, dict:from_list(?PEERS)),
	put(units, dict:from_list(?UNITS)).
grid_values(Grid)->
	Chars = [C||C<-Grid, lists:member(C, ?DIGITS++"0.")],
	dict:from_list(lists:zip(?SQUARES, Chars)).

assign(ValuesDict, S, D)->
	Other_values=lists:delete(D, dict:fetch(S, ValuesDict)),
	lists:foldl(fun(D2, AccIn) -> eliminate(AccIn,S,D2) end, ValuesDict, Other_values).

eliminate(ValuesDict, S, D)->
	S_V = dict:fetch(S, ValuesDict),
	case lists:member(D, S_V) of
		false -> ValuesDict;
		true ->
			NewValue = lists:delete(D, S_V),
			ValuesDict1 = dict:store(S, NewValue, ValuesDict),
			ValuesDict2 = 
			case NewValue of
				[] -> 
%% 					io:format("throw Error for L1~n"),
					throw({error, false});
				[D2] -> lists:foldl(fun(S2, AccIn)->eliminate(AccIn,S2,D2) end, ValuesDict1, dict:fetch(S, get(peers)));
				_ -> ValuesDict1
			end,
			lists:foldl(fun(U, AccIn)-> 
								case [S3||S3<-U,lists:member(D, dict:fetch(S3, AccIn))] of
									[] -> 
%% 										io:format("throw Error for L2~n"),
										throw({error, false});
									[SS3] -> assign(AccIn, SS3, D);
									_ -> AccIn
								end
						end, ValuesDict2, dict:fetch(S, get(units)))
	end.

test()->
	Units = dict:from_list(?UNITS),
	Peers = dict:from_list(?PEERS),
	io:format("~p~n~p~n~p~n~p~n~p~n~p~n", [?SQUARES,?UNITLISTS,Units,Peers,dict:fetch("A1", Units), dict:fetch("A1", Peers)]).
test_solve0()->
	Grid = "600000803040700000000000000000504070300200000106000000020000050000080600000010000",
	solve(Grid).

test_parse_grid()->
	display(parse_grid(?GRID)).
test_solve()->
	solve(?GRID).