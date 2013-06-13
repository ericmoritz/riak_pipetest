%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% A Riak Pipe worker that fetches the linked content
%%% @end
%%% Created :  9 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(reddit_w_content).
-behaviour(riak_pipe_vnode_worker).

-export([init/2,
	 process/3,
	 done/1]).


-record(state, {p, fd}).

-include_lib("riak_pipe/include/riak_pipe_log.hrl").

init(Partition, FittingDetails) ->
    {ok, #state{p=Partition, fd=FittingDetails}}.

process(Url, _Last, #state{fd=FD, p=P}=State) ->
    Ret = http:fetch(Url),
    riak_pipe_vnode_worker:send_output({Url, Ret}, P, FD),
    {ok, State}.

done(_State) ->
    ok.

