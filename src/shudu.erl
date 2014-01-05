%% @author Brilliant
%% @doc @todo Add description to shudu.


-module(shudu).
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
-export([parse_grid/1]).

display(ValuesDict)->
	Width = 1+lists:max([length(dict:fetch(S, ValuesDict))||S<-?SQUARES]),
	Line = string:copies("+"++string:copies("-", 3*Width),3)++"+",
	Center = fun(Elem) ->
					 Head = (Width-length(Elem)) div 2,
					 Tail = Width - length(Elem) - Head,	  
					 string:chars($ , Head)++Elem++string:chars($ , Tail)	  
			 end,
	StringEle = fun(R, C) ->
						Elem = dict:fetch([R,C], get(values)),
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
	ValStr = lists:foldl(fun(R, AccIn) -> Row = PrintRow(R), AccIn++Row end, [], ?ROWS),
	lists:foldl(fun(R, ok)-> io:format("~p~n", [R]) end, ok, string:tokens(ValStr, "\n")),
	ValStr.
parse_grid(Grid)->
	put(peers, dict:from_list(?PEERS)),
	put(units, dict:from_list(?UNITS)),
	Values = [{S, ?DIGITS}||S<-?SQUARES],
	put(values, dict:from_list(Values)),
	dict:map(fun(S,D)->case lists:member(D, ?DIGITS) of
						   true -> assign(S, D);
						   false -> pass
					   end
			 end, grid_values(Grid)),
	get(values).

%% ====================================================================
%% Internal functions
%% ====================================================================
grid_values(Grid)->
	Chars = [C||C<-Grid, lists:member(C, ?DIGITS++"0.")],
	dict:from_list(lists:zip(?SQUARES, Chars)).

assign(S, D)->
	Other_values=lists:delete(D, dict:fetch(S, get(values))),
	[eliminate(S,D2)||D2<-Other_values],
	get(values).

eliminate(S, D)->
	case lists:member(D, dict:fetch(S, get(values))) of
		false -> get(values);
		true ->
			put(values, dict:update(S, fun(VS) -> lists:delete(D, VS) end, get(values))),
			case dict:fetch(S, get(values)) of
				[] -> throw({error, false});
				[D2] -> [eliminate(S2,D2) ||S2<-dict:fetch(S, get(peers))];
				_ -> pass
			end,
			F1 = fun(U,D) ->
						 DPlaces = [SS||SS<-U, lists:member(D, dict:fetch(SS, get(values)))],
						 case DPlaces of 
							 [] -> throw({error, false});
							 [NewS] -> assign(NewS, D);
							 _ -> pass
						 end,
						 ok
				 end,
			[F1(U,D)||U<-dict:fetch(S, get(units))],
			get(values)
	end.

test()->
	Units = dict:from_list(?UNITS),
	Peers = dict:from_list(?PEERS),
	io:format("~p~n~p~n~p~n~p~n~p~n~p~n", [?SQUARES,?UNITLISTS,Units,Peers,dict:fetch("A1", Units), dict:fetch("A1", Peers)]).
test_parse_grid0()->
	Grid = "003020600900305001001806400008102900700000008006708200002609500800203009005010300",
	parse_grid(Grid).

test_parse_grid()->
	parse_grid(?GRID).