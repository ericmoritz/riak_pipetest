%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% Accessor for json keys
%%% @end
%%% Created : 10 Jun 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(json).

-export([
	 get/2,
	 get/3,
	 get_nested/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type json_container() :: {list()} | list().
-type json_key() :: binary() | integer().

%%--------------------------------------------------------------------
%% @doc
%% Returns the data at Key
%% @end
%%--------------------------------------------------------------------
-spec get(json_key(), json_container()) -> {ok, any()} | {error, notfound}.
get(Index, List) when is_list(List) ->
    case (Index < 1) or (Index > length(List)) of
	true ->
	    {error, notfound};
	false ->
	    {ok, lists:nth(Index, List)}
    end;
get(Key, {Props}) ->
    case proplists:get_value(Key, Props) of
	undefined ->
	    {error, notfound};
	V ->
	    {ok, V}
    end;
get(Key, Val) ->
    {error, {type_error, Key, Val}}.


%%--------------------------------------------------------------------
%% @doc
%% Returns the data at the Key
%% @end
%%--------------------------------------------------------------------
-spec get(json_key(), json_container(), any()) -> {ok, any()}.
get(Key, JSON, Default) ->
    case get(Key, JSON) of
	{error, notfound} ->
	    {ok, Default};
	V ->
	    {ok, V}
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% Accesses a nested value inside a JSON object/list
%% @end
%%--------------------------------------------------------------------
get_nested(Keys, Json) ->
    get_nested_internal(Keys, {ok, Json}).


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_nested_internal([], Val) ->
    Val;
get_nested_internal(_, E={error, _}) ->
    E;
get_nested_internal([Key|Rest], {ok, Json}) ->
    get_nested_internal(Rest, get(Key, Json)).

    
    
-ifdef(TEST).
get_list_test() ->
    {ok, <<"hi">>} = get(1, [<<"hi">>]).

get_list_oob_test() ->
    {error, notfound} = get(0, [<<"hi">>]),
    {error, notfound} = get(2, [<<"hi">>]).


get_obj_test() ->
    {ok, <<"hi">>} = get(<<"greeting">>, {[{<<"greeting">>, <<"hi">>}]}).

get_obj_notfound_test() ->
    {error, notfound} = get(<<"name">>, {[{<<"greeting">>, <<"hi">>}]}).

get_type_error_test() ->
    {error, {type_error, <<"test">>, <<"string">>}} = get(<<"test">>, <<"string">>),
    {error, {type_error, <<"test">>, 1}} = get(<<"test">>, 1),
    {error, {type_error, <<"test">>, true}} = get(<<"test">>, true),
    {error, {type_error, <<"test">>, null}} = get(<<"test">>, null).

get_nested_test() ->
    {ok, <<"eric">>} = get_nested([<<"people">>, 1, <<"first">>],
				  {[
				    {<<"people">>, [
						    {[{<<"first">>, <<"eric">>}]}
						   ]}
				   ]}),

    {error, notfound} = get_nested([<<"people">>, <<"first">>],
				  {[{<<"people">>, {[]}}]}).


-endif.
