#!/bin/sh
erlc -o ./ebin/ -I inlcude ./src/erlsoap_lib.erl  
erlc -o ./ebin/ -I inlcude ./src/mod_soap.erl
erlc -o ./ebin/ -I inlcude ./src/soap_server.erl
erlc -o ./ebin/ -I inlcude ./src/monitor_server.erl
erlc -o ./ebin/ -I inlcude ./src/soap_registry_server.erl
erlc -o ./ebin/ -I inlcude ./src/yaws_soap_lib.erl
erlc -o ./ebin/ -I inlcude ./src/make_error.erl
erlc -o ./ebin/ -I inlcude ./src/node_conf.erl