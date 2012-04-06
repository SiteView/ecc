-module(nmap_util).
-compile(export_all).

random_id()->
    integer_to_list(sv_datetime:now())++integer_to_list(random:uniform(9999999999999999999999999)). 