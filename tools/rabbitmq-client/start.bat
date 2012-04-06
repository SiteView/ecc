 erl -sname rabbitma_client  +P 500000 +A 200 -pa ebin -pa ../rabbitmq-server/ebin  -boot start_sasl 
  
rem erl -sname siteview  +P 500000 +A 200 -pa ebin   -eval "application:start(siteview)."
    