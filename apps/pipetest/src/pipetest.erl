%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 10 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(pipetest).

-export([start/0]).

start() ->
    apptools:ensure_started(?MODULE, permanent).

