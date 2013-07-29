%% @author ezonghu
%% @doc @todo Add description to xmerl_pretty_xml.


-module(xmerl_pretty_xml).

%% ====================================================================
%% API functions
%% ====================================================================
-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
   '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [export_text/1,attributes/1]).

-include_lib("xmerl/include/xmerl.hrl").

'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
%io:format("Text=~p~n",[Text]),
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V,Data];
'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>", Data].


%% The '#element#' function is the default handler for XML elements.

'#element#'(Tag, [], Attrs, Parents, _E) ->
%% 	io:format("Parents:~p~n", [Parents]),
%io:format("Empty Tag=~p~n",[Tag]),
    empty_tag(Tag, Attrs, length(Parents));
'#element#'(Tag, Data, Attrs, Parents, _E) ->
%% 	io:format("Parents:~p~n", [Parents]),
%io:format("Tag=~p~n",[Tag]),
    markup(Tag, Attrs, Data, length(Parents)).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% markup(Tag, Data, ParentsNumber) ->
%%     markup(Tag, [], Data, ParentsNumber).

markup(Tag, Attrs, [], ParentsNumber) ->
    empty_tag(Tag, Attrs, ParentsNumber);
markup(Tag, Attrs, [Text]=Data, ParentsNumber) ->
%% 	io:format("xmltext:~p~n", [Data]),
	case re:run(Text,  "<.+>", [global]) of
		{match, _} -> [start_tag(Tag, Attrs, ParentsNumber), Data, ["\n"|end_tag(Tag, ParentsNumber)]];
		nomatch -> [start_tag(Tag, Attrs, ParentsNumber), Data, end_tag(Tag, 0)]
	end;
markup(Tag, Attrs, Data, ParentsNumber) ->
%% 	io:format("xmltext:~p~n", [Data]),
    [start_tag(Tag, Attrs, ParentsNumber), Data, ["\n"|end_tag(Tag, ParentsNumber)]].
%% start_tag(TagStr, ParentsNumber) ->
%%     start_tag(TagStr, [], ParentsNumber).

start_tag(Tag, Attrs, ParentsNumber) when is_atom(Tag) ->
    start_tag(atom_to_list(Tag), Attrs, ParentsNumber);
start_tag(TagStr, [], ParentsNumber) ->
    ["\n", lists:duplicate(ParentsNumber, "\t"), "<", TagStr, ">"];
start_tag(TagStr, Attrs, ParentsNumber) ->
    ["\n", lists:duplicate(ParentsNumber, "\t"), "<", TagStr, attributes(Attrs), ">"].

%% empty_tag(Tag, ParentsNumber) ->
%%     empty_tag(Tag, [], ParentsNumber).

empty_tag(Tag, Attrs, ParentsNumber) when is_atom(Tag) ->
    empty_tag(atom_to_list(Tag), Attrs, ParentsNumber);
empty_tag(TagStr, [], ParentsNumber) ->
    ["\n", lists:duplicate(ParentsNumber, "\t"), "<", TagStr, "/>"];
empty_tag(TagStr, Attrs, ParentsNumber) ->
    ["\n", lists:duplicate(ParentsNumber, "\t"), "<", TagStr, attributes(Attrs), "/>"].

end_tag(Tag, ParentsNumber) when is_atom(Tag) ->
    end_tag(atom_to_list(Tag), ParentsNumber);
end_tag(TagStr, ParentsNumber) ->
    [lists:duplicate(ParentsNumber, "\t"), "</", TagStr, ">"].







