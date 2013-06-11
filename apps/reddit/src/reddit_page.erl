%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 10 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(reddit_page).

-export([
	 title/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

title(MochiHTML) ->
    find_tag(<<"title">>, [MochiHTML]).

find_tag(TagName, []) ->
    {error, notfound};
find_tag(TagName, [El|Rest]) when is_binary(El) ->
    find_tag(TagName, Rest);
find_tag(TagName, [El={TagName, _, _}|_]) ->
    {ok, El};
find_tag(TagName, [{_, _, Children}|Rest]) ->
    case find_tag(TagName, Children) of
	{ok, El} ->
	    {ok, El};
	{error, notfound} ->
	    find_tag(TagName, Rest)
    end.
    



-ifdef(TEST).
find_tag_test() ->
    {error, notfound} = find_tag(<<"title">>, [mochiweb_html:parse("<html>")]),
    {ok, {<<"title">>, [], []}} = find_tag(<<"title">>, [mochiweb_html:parse("<title>")]),
    {ok, {<<"body">>, [], []}} = find_tag(<<"body">>, [mochiweb_html:parse("<html><title>Title<body>")]).

-endif.
