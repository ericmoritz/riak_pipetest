-module(social).

-export([
	 shares/1,
	 shares/2,
	 shares_pipeline/0
]).

-include_lib("riak_pipe/include/riak_pipe.hrl").

shares(Url) ->
    shares(Url, []).

shares(Url, Opts) ->
    {ok, Pipe} = riak_pipe:exec(shares_pipeline(), Opts),
    ok = riak_pipe:queue_work(Pipe, Url),
    riak_pipe:eoi(Pipe),
    riak_pipe:collect_results(Pipe).
		    
shares_pipeline() ->
    [
     #fitting_spec{name=social_mapper,
		   module=riak_pipe_w_xform,
		   arg=fun social_share:mapper/3},

     #fitting_spec{name=social_fetcher,
		   module=riak_pipe_w_xform,
		   arg=fun social_share:fetch/3}
    ].

    
