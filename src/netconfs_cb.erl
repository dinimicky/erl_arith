%% @author Brilliant
%% @doc @todo Add description to netconfs_cb.


-module(netconfs_cb).
-behaviour(ssh_daemon_channel).
-record(state, {
	  n,
	  buf_tab
	 }).
-record(buf_idx, {cm, id}).
-record(buf_rec, {idx, ctx}).
-define(Template_List, ["hello.xml", 
						"rpc_ok_reply.xml", 
						"rpc_action_reply.xml",
						"rpc_get_reply.xml"
					   ]).
-define(Template_Dir, "./netconf_template/").
-include_lib("xmerl/include/xmerl.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).
-export([subsystem_spec/1, subsystem_spec/0]).

subsystem_spec()->
	{"netconf", {?MODULE, [?Template_Dir]}}.
subsystem_spec(TemplateDir) ->
    {"netconf", {?MODULE, [TemplateDir]}}.
init([TempDir]) ->
	RestFiles = filelib:fold_files(TempDir, "xml$", false, fun(FileName, AccIn)-> lists:delete(FileName, AccIn) end, ?Template_List),
	case RestFiles of
		[] -> ets:new(netconf, [named_table, protected]),
			  {ok, #state{n = 1, buf_tab=netconf}};
		_ -> {stop, {missing_tempaltes, RestFiles}}
	end.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, #state{buf_tab = Buf_tab} = State) ->
	ets:insert(Buf_tab, #buf_rec{idx = #buf_idx{cm = ConnectionManager, id = ChannelId}, ctx = <<>>}),
    {ok, State}.

handle_ssh_msg({ssh_cm, CM, {data, ChannelId, 0, Data}}, #state{n = N, buf_tab=Buf_tab} = State) ->
	Key = #buf_idx{cm=CM, id=ChannelId},
	[#buf_rec{ctx = OriginData}] = ets:lookup(Buf_tab, Key),
	case check_netconf_finish(<<OriginData/binary, Data/binary>>) of
		{finished, Netconf} -> 
			true = ets:update_element(Buf_tab, Key, {2, <<>>}),
			{RpcElement, _Rest} = xmerl_scan:string(Netconf),
			case xmerl_xpath:string("/rpc/node()", RpcElement) of
				[#xmlElement{name = Method}] -> 
					{ok, Reply}=file:read_file(?Template_Dir++"rpc_"++erlang:atom_to_list(Method)++"_reply.xml"),
					ssh_connection:send(CM, ChannelId, Reply),
					if
						Method =:= 'close-session';Method =:= 'kill-session'  ->
							ssh_connection:send_eof(CM, ChannelId);
						true -> ok
					end;
				[] -> [_Hello] = xmerl_xpath:string("/hello", RpcElement),
					  {ok, Hello}=file:read_file(?Template_Dir++"hello.xml"),
					  ssh_connection:send(CM, ChannelId, Hello)
			end;
		{unfinished, NewData} -> true = ets:update_element(Buf_tab, Key, {2, NewData})
	end,
    M = N - size(Data),
    case M > 0 of
	true ->
	   ssh_connection:send(CM, ChannelId, Data),
	   {ok, State#state{n = M}};
	false ->
	   <<SendData:N/binary, _/binary>> = Data,
           ssh_connection:send(CM, ChannelId, SendData),
           ssh_connection:send_eof(CM, ChannelId),
	   {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, _ConnectionManager,
		{data, _ChannelId, 1, Data}}, State) ->
    error_logger:format(standard_error, " ~p~n", [binary_to_list(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}},
	       State) ->
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State}.

terminate(_Reason, _State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
check_netconf_finish(Data)->
	Len=size(Data),
    Len7 = Len - 7,
    Len6 = Len - 6,
    case Data of
		<<Netconf:Len7/binary, "]]>]]>\n">> ->
			{finished, Netconf};
		<<Netconf:Len6/binary, "]]>]]>">> ->
			{finished, Netconf};
		_ -> 						
			{unfinished, Data}    
	end.




