%% @author ezonghu
%% @doc @todo Add description to g2010_r1c_pc.


-module(g2010_r1c_pc).
%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_input/1, parse_input/0]).

parse_input()->
	parse_input("C-small-practice.in").
parse_input(InFile)->
	ets:new(squares, [named_table, public]),
	ets:new(bark, [named_table, public]),
	ets:new(res, [named_table, public]),
	{ok, InFileHandler} = file:open(InFile, [read]),
	FirstLine = get_line(InFileHandler),
	Cases = erlang:list_to_integer(FirstLine),
	get_case(InFileHandler, Cases, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_line(FileHandler)->
	string:strip(io:get_line(FileHandler, ""), right, $\n).
get_case(_FileHandler, _Cases, _Cases)->
	ok;
get_case(FileHandler,Cases, CaseId)->
	CaseFirstLine = get_line(FileHandler),
	[Rows, Cols] = [list_to_integer(I)||I<-string:tokens(CaseFirstLine, " ")],
	set_elements(FileHandler, Rows, 0),
	F = fun (R, C) ->
				 if 
					 R == 0 -> ets:insert(squares, {{R,C}, 1});
					 C == 0 -> ets:insert(squares, {{R,C}, 1});
					 true -> ok
				 end
		end,
	[{R,C}||R <- lists:seq(0, Rows - 1), C <- lists:seq(0, Cols - 1)],
	clearTab(squares),
	clearTab(bark),
	get_case(FileHandler, Cases, CaseId+1).

clearTab(Tab)->
	ets:delete(Tab),
	ets:new(Tab, [named_table, public]).

set_elements(_FileHandler, _Rows, _Rows)->
	ok;
set_elements(FileHandler, Rows, RowId)->
	RowStr = get_line(FileHandler),
	Row = parse_caseRow(RowStr),
	insert_element(Row, 0, RowId),
	set_elements(FileHandler, Rows, RowId + 1).

insert_element([], _ColId, _RowId)->
	ok;
insert_element([Element|RowRest]=_Row, ColId, RowId)-> 
	ets:insert(bark, {{RowId, ColId}, Element}),
	ets:insert(squares, {{RowId, ColId}, 0}),
	insert_element(RowRest, ColId + 1, RowId).

parse_caseRow(RowStr)->
	lists:flatmap(fun(HexStr) ->
						  Hex = erlang:list_to_integer([HexStr], 16),
						  lists:map(fun(E) -> 
											case E band Hex of 
												0 -> -1;
												_ -> 1
											end 
									end, [8,4,2,1])
				  end, RowStr).