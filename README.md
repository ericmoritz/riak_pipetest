riak_pipetest
=============

An example of a stand alone riak_pipe app

build dev cluster
------------------

    make devrel
    bash devup.sh
    ./dev/dev1/bin/pipetest-admin cluster plan
    ./dev/dev1/bin/pipetest-admin cluster commit

run the example
-----------------

```erlang
$ ./dev/dev1/bin/pipetest attach
    
(dev1@127.0.0.1)1> reddit:subreddit_content("erlang").
{eoi,[{reddit_w_co...}
(dev1@127.0.0.1)1> ^D
```
