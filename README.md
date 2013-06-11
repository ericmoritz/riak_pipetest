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

    ./dev/dev1/bin/pipetest console
    1> reddit:subreddit_content("erlang").
    
