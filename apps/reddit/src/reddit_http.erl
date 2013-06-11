%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% 
%%% @end
%%% Created : 10 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(reddit_http).

-export([
	 fetch_json/1,
	 fetch/1,
	 success_body/1,
	 parse_json/1
]).

fetch_json(Url) ->
    error_m:do(
      {ok, Url},
      [
       fun fetch/1,
       fun success_body/1,
       fun parse_json/1
      ]
     ).

fetch(Url) when is_binary(Url) ->
    fetch(binary_to_list(Url));
fetch(Url) ->
    httpc:request(get, {Url, []}, [], [{body_format, binary}]).
      
success_body({{_, 200, _}, _Hdrs, Body}) ->
    {ok, Body};
success_body(Resp) -> 
    {error, {http, Resp}}.

parse_json(Body) ->
    try jiffy:decode(Body) of
	R ->
	    {ok, R}
    catch
	throw:{error, Error} ->
	    {error, Error}
    end.
