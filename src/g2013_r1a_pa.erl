%% @author ezonghu
%% @doc @todo Add description to g2013_r1a_pa.


-module(g2013_r1a_pa).
-compile(export_all).
-export([]).
%% ====================================================================
%% solve functions
%% ====================================================================
solve(R,T)->
	N=(math:sqrt(math:pow((2*R-1),2)+8*T)-(2*R-1))/4,
	trunc(N).


%% ====================================================================
%% API functions
%% ====================================================================
process()->
	process("C:/Users/ezonghu/Downloads/A-large-practice").

process(FilePrefix)->
	InputFileName = FilePrefix++".in",
	OutputFileName = FilePrefix++".out",
	process_file(InputFileName, OutputFileName).

process_file(InputFileName, OutputFileName)->
     {ok, I_Fh}=file:open(InputFileName, [read]),
     {ok, O_Fh}=file:open(OutputFileName, [write]),
     {_Cases, InputData}=get_data(I_Fh),
	 OutputData=[solve(R,T)||[R,T]<-InputData],
	 writeOutputData(O_Fh, OutputData),
     file:close(I_Fh),
     file:close(O_Fh).

process_file2(InputFileName, OutputFileName)->
     {ok, I_Fh}=file:open(InputFileName, [read]),
     {ok, O_Fh}=file:open(OutputFileName, [write]),
     {ok, InputData}=get_data(I_Fh),
	 OutputPids=[erlang:spawn(?MODULE, getRangeRecycledNumberCounters, [Start, End, self()])||[End, Start]<-lists:nthtail(1, InputData)],
	 io:format("Output Pids: ~p~n", [OutputPids]),
	 OutputData=gather(OutputPids, []),
     io:format("~w~n", [OutputData]),
	 writeOutputData(O_Fh, OutputData),
     file:close(I_Fh),
     file:close(O_Fh).
%% ====================================================================
%% Internal functions
%% ====================================================================
gather([Pid|TailPids]=_Pids, Results)->
	receive 
		{Pid, Result}->NewResults=[Result|Results],
					   gather(TailPids, NewResults)
	end;
gather([], Results)->
	lists:reverse(Results).

writeOutputData(IoDevice, OutputData)->
	writeOutputData(IoDevice, OutputData, 1).
writeOutputData(IoDevice, [Header|Tail]=_OutputData, Counter)->
	io:fwrite(IoDevice, "Case #~p: ~p~n", [Counter, Header]),
	writeOutputData(IoDevice, Tail, Counter+1);
writeOutputData(_IoDevice, [], _Counter)->
	ok.
get_data(FileHandle)->
	Data = io:get_line(FileHandle, ""),
	{Cases, _} = string:to_integer(Data),
    {Cases, get_data(FileHandle, [])}.
get_data(FileHandle, Datas)->
     case io:get_line(FileHandle,"") of
          eof -> lists:reverse(Datas);
          {error, Reason} -> {error, Reason, Datas};
          Data ->
               {match, Res}=re:run(Data, "[0-9]+", [global, {capture, first, list}]),
               get_data(FileHandle, [multiStr2Int(Res)|Datas])
     end.

multiStr2Int(List)->
     multiStr2Int(List, []).
multiStr2Int([H|T]=_List, RL)->
     {Int, _Rest}=string:to_integer(lists:flatten(H)),
     multiStr2Int(T, [Int|RL]);
multiStr2Int([], RL)->
     lists:reverse(RL).

    
     

