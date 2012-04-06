#!/bin/sh
erl -sname wmi -setcookie 3ren -pa ./ebin -pa ./lib -boot start_sasl -s wmi