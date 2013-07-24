-module(ip_convert).
-compile(export_all).

ip_str2tuple2(DirtyIpStr) when is_list(DirtyIpStr) ->
	IpStr = re:replace(DirtyIpStr, "\\s", "", [global,{return, list}]),
	IPv4Template = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])",
	IPv4RE = lists:concat(lists:duplicate(3, IPv4Template++"\\."))++IPv4Template,
	case re:run(IpStr, "^"++IPv4RE++"$", [global, {capture, all_but_first, list}]) of
		{match, [[A, B, C, D]]} -> {ipv4, {list_to_integer(A), list_to_integer(B), list_to_integer(C), list_to_integer(D)}};
		{match, _MultiIP} -> {error, multi_ipv4_addresses};
		nomatch -> 
			IPv6noIPv4= convert_ipv4_in_ipv6(IpStr, IPv4RE),
			case covert_compress_in_ipv6(IPv6noIPv4) of
				{error, Reason} -> {error, Reason};
				{ok, IPv6noCompress} -> convert_basic_ipv6(IPv6noCompress)
			end	
	end;
ip_str2tuple2(_IpStr) ->
	{error, input_not_string}.
convert_ipv4_in_ipv6(IpStr, IPv4RE)->
	case re:run(IpStr, ":"++IPv4RE++"$", [{capture, all_but_first}]) of						   
		nomatch -> IpStr;
		{match, [{S1, _L1}|_RestGroups]=Groups} -> 
%% 			io:format("~p~n", [Groups]),
			IPv6Part = string:sub_string(IpStr, 1, S1),
			IPGroups = [erlang:list_to_integer(string:substr(IpStr, S+1, L))||{S,L}<-Groups],
			
			[G1,G2,G3,G4]=IPGroups,
			IPv6Part ++ integer_to_list(G1*256+G2, 16) ++":"++integer_to_list(G3*256+G4,16)
	end.
covert_compress_in_ipv6(IPv6noIPv4)->
	case re:split(IPv6noIPv4, "::", [{return, list}]) of
		[IPv6noIPv4] -> {ok, IPv6noIPv4};
		[IPv6Part1, IPv6Part2] ->
			case IPv6Part1 of 
				[] -> IPv6Part1List = [];
				_ -> IPv6Part1List = re:split(IPv6Part1, ":", [{return, list}])
			end,
			case IPv6Part2 of 
				[] -> IPv6Part2List = [];
				_ -> IPv6Part2List = re:split(IPv6Part2, ":", [{return, list}])
			end,			
			ZeroNum = 8 - length(IPv6Part1List) - length(IPv6Part2List),
			case IPv6Part1List of
				[] ->
%% 					io:format("3:~p~n", [lists:concat(lists:duplicate(ZeroNum, "0:"))++IPv6Part2]),
					case IPv6Part2 of
						[] ->{ok, lists:concat(lists:duplicate(ZeroNum-1, "0:"))++"0"};
						_ -> {ok, lists:concat(lists:duplicate(ZeroNum, "0:"))++IPv6Part2}
					end;
				_ -> 
%% 					io:format("4:~p~n", [IPv6Part1++lists:concat(lists:duplicate(ZeroNum, ":0"))++":"++IPv6Part2]),
					case IPv6Part2List of
						[] -> {ok, IPv6Part1++lists:concat(lists:duplicate(ZeroNum, ":0"))};
						_ -> {ok, IPv6Part1++lists:concat(lists:duplicate(ZeroNum, ":0"))++":"++IPv6Part2}
					end
			end;
		__MultiCompress -> {error, multi_ipv6_compressflag}
	end.

convert_basic_ipv6(IpStr) ->
	IPv6GroupTemplate = "([0-9a-fA-F.]{1,4})",
	IPv6RE = "^"++lists:duplicate(7, IPv6GroupTemplate++":")++IPv6GroupTemplate++"$",
%% 	io:format("~p~n", [IPv6RE]),
	case re:run(IpStr, IPv6RE, [{capture, all_but_first, list}]) of
		{match, IPv6StrList} ->
%% 			io:format("~p~n", [IPv6StrList]),
			IPv6List = [list_to_integer(Number,16)||Number<-IPv6StrList],
			{ipv6, list_to_tuple(IPv6List)};
		nomatch -> {error, wrong_ipv6_format}
	end.
%%============================================================= 
ip_str2tuple(DirtyIpStr) when is_list(DirtyIpStr) ->
  IpStr = re:replace(DirtyIpStr, "\\s", "", [global,{return, list}]),
	IPv4Template = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])",
%% 	(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])
	IPv4RE = lists:concat(lists:duplicate(3, IPv4Template++"\\."))++IPv4Template,
	case re:run(IpStr, "^"++IPv4RE++"$", [global, {capture, all_but_first, list}]) of
		{match, [[A, B, C, D]]} -> {ipv4, {list_to_integer(A), list_to_integer(B), list_to_integer(C), list_to_integer(D)}};
		{match, _MultiIP} -> {error, multi_ipv4_addresses};
		nomatch -> 
			case re:split(IpStr, ":", [{return, list}]) of
				[_WrongFormat] -> {error, wrong_ip_format};
				_IPv6List -> 				
					case re:split(IpStr, "::", [{return, list}]) of
						[IpStr] -> 
							case is_contain_ipv4_in_ipv6(IpStr, IPv4RE) of 
								{false, IpStr} -> 
									case convert_partipv6(IpStr) of 
										{ok, IPList} -> convert_ipv6_list2tuple(IPList);
										{error, Reason} -> {error, Reason}
									end;
								{true, [IPv4String, IpPart1, IpPart2, IpPart3, IpPart4]} -> 
									IPv6Part = re:replace(IpStr, ":{0,2}"++IPv4String, "", [{return, list}]),
									IPv6Last = convert_ipv4_to_ipv6(IpPart1, IpPart2, IpPart3, IpPart4),
									case convert_partipv6(IPv6Part) of
										{ok, IPv6PartList} -> convert_ipv6_list2tuple(IPv6PartList++IPv6Last);
										{error, Reason} -> {error, Reason}
									end;
								{error, Reason} -> {error, Reason}
							end;
						[IpPart1, IpPart2] -> 
							case convert_partipv6(IpPart1) of
								{error, Reason} -> {error, Reason};
								{ok, IpList1} ->
									IpList1Length = length(IpList1),
									case is_contain_ipv4_in_ipv6(IpPart2, IPv4RE) of
										error -> {error, wrong_ipv4_format_in_ipv6};
										{false, IpPart2} ->
											case convert_partipv6(IpPart2) of
												{error, Reason} -> {error, Reason};
												{ok, IpList2} ->
													IpList2Length = length(IpList2),
													ZeroNum = 8 - IpList2Length - IpList1Length,
													IPv6List = IpList1 ++ lists:duplicate(ZeroNum, 0)++IpList2,
													convert_ipv6_list2tuple(IPv6List)
											end;
										{true, [IPv4StringinIPv6, IpPart1inIPv6, IpPart2inIPv6, IpPart3inIPv6, IpPart4inIPv6]} -> 
											IPPart2NoIpv4 = re:replace(IpPart2, ":{0,2}"++IPv4StringinIPv6, "", [{return, list}]),
%% 											io:format("~p~n", [IPPart2NoIpv4]),
											case convert_partipv6(IPPart2NoIpv4) of
												{error, Reason} -> {error, Reason};
												{ok, IpList2} ->
													IpList2Length = length(IpList2),
													ZeroNum = 8 - IpList2Length - IpList1Length - 2,
													IPv6Last = convert_ipv4_to_ipv6(IpPart1inIPv6, IpPart2inIPv6, IpPart3inIPv6, IpPart4inIPv6),
													IPv6List = IpList1 ++ lists:duplicate(ZeroNum, 0)++IpList2 ++ IPv6Last,
													convert_ipv6_list2tuple(IPv6List)
											end
									end
								end;
							_MultiCompress -> {error, multi_ipv6_compressflag}
					end
			end
	end;
ip_str2tuple(_IpStr) ->
	{error, input_not_string}.
%% ==============================
%% Local Functions
%% ==============================
is_contain_ipv4_in_ipv6(IpStr, IPv4RE)->
%%  	io:format("~p~n", [IpStr]),
	case re:run(IpStr, IPv4RE, [global, {capture, first}]) of						   
		nomatch -> {false, IpStr};
		{match, [_IPv4]} -> 
			case re:run(IpStr, IPv4RE++"$", [global, {capture, all, list}]) of
				nomatch -> error;
				{match, [IPv4List]} -> 
					IpStrNoIPv4 = re:replace(IpStr, lists:nth(1, IPv4List), "", [{return, list}]) ,
%%  					io:format("~p~n", [IpStrNoIPv4]),
					if 
						IpStrNoIPv4 =:= [] -> {true, IPv4List};
						true -> 
							case lists:last(IpStrNoIPv4) of
								$: -> {true, IPv4List};
								_ -> error
							end
					end
			end;
		{match, _multiIPv4} -> {error, multi_ipv4_in_ipv6}						   
	end.
convert_ipv4_to_ipv6(IpPart1, IpPart2, IpPart3, IpPart4) ->
	I1 = list_to_integer(IpPart1),
	I2 = list_to_integer(IpPart2),
	I3 = list_to_integer(IpPart3),
	I4 = list_to_integer(IpPart4),
	[I1*256+I2, I3*256+I4].
convert_partipv6([]) ->
	{ok, []};
convert_partipv6(IpStr) ->
	IPv6List = re:split(IpStr, ":", [{return, list}]),
	IPv6GroupTemplate = "^[0-9a-fA-F.]{1,4}$",
	IsIPv6Format = 
	lists:foldl(fun (Group, Result) ->						
						 case Group of 	
							 [] -> Result and true;	
							 Group ->
								 case re:run(Group, IPv6GroupTemplate, [global]) of				
									 {match, _} -> Result and true;
									 nomatch -> Result and false
								 end
						 end
				end, true, IPv6List),
	case IsIPv6Format of
		true -> {ok, [erlang:list_to_integer(Number, 16)||Number<-IPv6List]};
		false -> {error, wrong_ipv6_format}
	end.
convert_ipv6_list2tuple(IPv6List) ->
	if 
		length(IPv6List) > 8 -> {error, wrong_ipv6_format};
		true -> {ipv6, list_to_tuple(IPv6List)}
	end.
