for i in dev/*/bin/pipetest; do $i start; done
for i in dev/dev{2,3,4,5}/bin/pipetest-admin; do $i cluster join dev1@127.0.0.1; done
