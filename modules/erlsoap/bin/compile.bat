@echo off
erlc -o ../ebin -I include ../src/erlsoap_lib.erl
erlc -o ../ebin -I include ../src/mod_soap.erl
erlc -o ../ebin -I include ../src/monitor_server.erl
erlc -o ../ebin -I include ../src/soap_registry_server.erl
erlc -o ../ebin -I include ../src/soap_server.erl
erlc -o ../ebin -I include ../src/soapclient.erl
erlc -o ../ebin -I include ../src/yaws_soap_lib.erl

pause