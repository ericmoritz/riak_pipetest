-module(social_share).

-export([
	 mapper/3,
	 fetch/3,
	 fetch_twitter_shares/1,
	 fetch_fb_data/1,
	 foreach_fb_count/2
]).

-include_lib("riak_pipe/include/riak_pipe_log.hrl").

mapper(Url, Partition, FittingDetails) ->
    % map the Url to three outputs to be distributed to the cluster
    [
     riak_pipe_vnode_worker:send_output(
       {Type, Url},
       Partition, FittingDetails
      )
     || Type <- [facebook, twitter]
    ],
    ok.

fetch({facebook, Url}, Partition, FittingDetails) ->
    foreach_fb_count(
      Url,
      fun({Type, Value}) ->
    	      send_output(
    		{ok, Value},
    		Type,
    		Url,
    		Partition,
    		FittingDetails
    	       )
      end),
    ok;
fetch({twitter, Url}, Partition, FittingDetails) ->
    send_output(
      fetch_twitter_shares(Url),
      <<"twitter_shares">>,
      Url,
      Partition,
      FittingDetails
     ),
    ok;
fetch(V, _, FittingDetails) ->
    ?L(FittingDetails, {error, {unknown_input, V}}),
    ok.

    

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_output({ok, Value}, Type, Url, Partition, FittingDetails) ->
    riak_pipe_vnode_worker:send_output(
      {Url, {Type, Value}},
      Partition,
      FittingDetails
     );
send_output({error, _}=E, _Type, _Url, _Partition, FittingDetails) ->
    ?L(FittingDetails, E).


fetch_fb_data(Url) ->
    SharesUrl = "http://graph.facebook.com/?id=" ++ Url,
    error_logger:info_msg("~s~n", [SharesUrl]),
    http:fetch_json(SharesUrl).

foreach_fb_count(Url, Fun) ->
    case fetch_fb_data(Url) of
	{ok, Data} ->
	    {ok, Shares} = json:get(<<"shares">>, Data, 0),
	    {ok, Comments} = json:get(<<"comments">>, Data, 0),
	    Fun({<<"facebook_shares">>, Shares}),
	    Fun({<<"facebook_comments">>, Comments}),
	    {ok, Data};
	{error, E}=E ->
	    E
    end.
	    
	    


fetch_twitter_shares(Url) ->
    SharesUrl = "http://urls.api.twitter.com/1/urls/count.json?url=" ++ Url,
    case http:fetch_json(SharesUrl) of
	{ok, Data} ->
	    json:get(<<"count">>, Data, 0);
	{error, _}=E ->
	    E
    end.


