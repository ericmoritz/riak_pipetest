% -*- erlang -*-
-module(reddit).


-export([
	 subreddit_content/1
]).
    
-include_lib("riak_pipe/include/riak_pipe.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Returns the full content of links for a subreddet
%% @end
%%--------------------------------------------------------------------
subreddit_content(SubReddit) ->
    {ok, Pipe} = riak_pipe:exec(
		   [
                    % fetch a subreddit's page and emit the links
                    #fitting_spec{
		       name=reddit_w_links,
		       module=reddit_w_links
		      },
                    #fitting_spec{
				   name=reddit_w_content,
				   %module=riak_pipe_w_pass
				   module=reddit_w_content
		      }
                   ],
		   [
                    {log, sink}
                   ]),
    ok = riak_pipe:queue_work(Pipe, SubReddit),
    riak_pipe:eoi(Pipe),
    riak_pipe:collect_results(Pipe).
		    
		    
      
