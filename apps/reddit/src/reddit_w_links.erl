%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% A Riak Pipe worker that emits the Urls of a subreddit's posts
%%% @end
%%% Created :  9 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(reddit_w_links).
-behaviour(riak_pipe_vnode_worker).

-export([init/2,
	 process/3,
	 done/1]).


-record(state, {p, fd}).

-include_lib("riak_pipe/include/riak_pipe_log.hrl").

init(Partition, FittingDetails) ->
    {ok, #state{p=Partition, fd=FittingDetails}}.

process(Subreddit, _Last, #state{fd=FD, p=P}=State) ->
    case fetch_children(Subreddit) of
	{ok, Nodes} ->
	    foreach_urls(
	      fun(Url) ->
		      riak_pipe_vnode_worker:send_output(Url, P, FD)
	      end,
	      Nodes);
	{error, _} = E ->
	    ?L(FD, {?MODULE, E})
    end,
    {ok, State}.

done(_State) ->
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================
foreach_urls(F, Nodes) ->
    lists:foreach(
      fun(Node) ->
	      case reddit_subreddit:url(Node) of
		  {ok, Url} ->
		      F(Url);
		  _ ->
		      pass
	      end
      end,
      Nodes
     ).

fetch_children(Subreddit) ->
    error_m:do({ok, Subreddit},
	       [
		fun reddit_subreddit:fetch/1,
		fun reddit_subreddit:children/1
	       ]).
