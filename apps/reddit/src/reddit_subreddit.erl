%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 10 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(reddit_subreddit).

-export([
	 fetch/1,
	 children/1,
	 url/1
]).

fetch(Subreddit) ->
    reddit_http:fetch_json(
      "http://www.reddit.com/r/" ++ Subreddit ++ ".json"
    ).

children(Node) ->
    json:get_nested([<<"data">>, <<"children">>], Node).
	    
url(Node) ->
    json:get_nested([<<"data">>, <<"url">>], Node).

%%%===================================================================
%%% Internal
%%%===================================================================

		
