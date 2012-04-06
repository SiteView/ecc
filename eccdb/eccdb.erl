-module (eccdb).

-include_lib ("amnesia_db_def.hrl").

%%

driver_info () ->
  [{driver, mysql_drv},
   {host, "localhost"},
   {user, "eccdb"},
   {password, "eccdb"},
  {logging, true}].

%%

tables () ->
  [
  subgroup,
  monitor,
  alert_log,
  alert_rule,
  configure,
  counters,
  machine,
  machine_label,
  machine_other,
  monitor_attribute,
  monitor_group,
  monitor_property,
  monitorlog,
  monitorlog_value,
  operationlog,
  schedule,
  user_spl,
  user_info_right,
  weekday
  ].

%%
table (alert_log) ->
  [ 
    {stype, varchar, not_null},
    {name, varchar, not_null},
    refers_to (monitor),
    {receiver, varchar, not_null},
    {title, varchar, not_null},
    {time, date, not_null},
    {result, varchar, not_null},
    {content, varchar, not_null},
    {alert_level, varchar, not_null},
    {measurement, varchar, not_null},
     refers_to (subgroup),
    {responsetime, date, not_null},
    {responder, varchar, not_null},
    {responsecontent, varchar, not_null},
    {cleartime, date, not_null},
    {times, integer, not_null},
    {email, varchar, [not_null, {default, ""}]}
   ];

%%
table (alert_rule) ->
  [ 
    {stype, varchar, not_null},
    {skey, varchar, not_null},
    {svalue, text, not_null}
   ];
table (configure) ->
  [ 
    {name, varchar, not_null},
    {section, varchar, not_null},
    {skey, varchar, not_null},
    {svalue, text, not_null}
   ];

%%
table (counters)->
[
    {name, varchar, not_null},	
    {isdefault, boolean ,not_null} 		
];

%%
table (subgroup)->
[
    {parentid, varchar ,not_null},
    {name, varchar ,not_null},
    {description, varchar ,not_null},
    {user_name, varchar ,not_null}
];

%%
table (machine)->
[
    {name, varchar ,not_null},	
    {host, varchar ,not_null},
    {login, varchar ,not_null},	
    {password, varchar ,not_null},
    {trace, boolean ,not_null},	
    {os, varchar ,not_null},
    {status, varchar ,not_null},
    {method, varchar ,not_null},
    {prompt, varchar ,not_null},
    {loginprom, varchar ,not_null},
    {passwdprom, varchar ,not_null},
    {secondprom, varchar ,not_null},	
    {secondresp, varchar ,not_null},
    {initshell, varchar ,not_null},
    {remoteencoding, varchar ,not_null},
    {sshcommand, varchar ,not_null},
    {sshclient, varchar ,not_null},
    {sshport, integer ,not_null},	
    {disableconncaching,boolean ,not_null},
    {connlimit, integer ,not_null},    
    {version, varchar ,not_null},
    {keyfile, varchar ,not_null},
    {sshauthmethod, varchar ,not_null},
    {label, varchar ,not_null},
    {total , integer ,not_null},
    {stype , varchar ,not_null},
    {pwdmode, varchar ,not_null}
];

%%
table (machine_label)->
[
    {name, varchar ,not_null},
    {stype, varchar ,not_null},
    {idx, integer,not_null},
    {syslabel, boolean ,not_null},
    {ishide, boolean ,not_null},
    {svalue, varchar ,not_null},
    {treeindex, varchar ,not_null},
    refers_to (machine_label)
];


%%
table (machine_other)->
[
    refers_to (machine),
    {name, varchar ,not_null},
    {svalue, text ,not_null}
];

%%
table (monitor)->
[ 
];


%%
table (monitor_attribute)->
[
    refers_to (monitor),
    {name, varchar ,not_null},
    {svalue, text ,not_null},
    {stype, varchar ,not_null}	
];

%%
table (monitor_group)->
[
    refers_to (subgroup),
    refers_to (monitor)  	
];

%%
table (monitor_property)->
[
    refers_to (monitor),
    {name, varchar ,not_null},
    {svalue, text ,not_null},
    {stype, varchar ,not_null}		
];

%%
table (monitorlog)->
[
    {name, varchar ,not_null},
    {time, date ,not_null},
    {category, varchar ,not_null},
    {measurement, varchar ,not_null},
    {class, varchar ,not_null},
    {groupname, varchar ,not_null}    
];

%%
table (monitorlog_value)->
[
    refers_to (monitorlog),
    {svalue, text ,not_null},
    {stype, varchar ,not_null},
    {name, varchar ,not_null},
    {unit, varchar ,not_null}    
];

%%
table (operationlog)->
[
    {create_time, date ,not_null},
    {record_state, varchar ,not_null},
    {ip, varchar ,not_null},
    {operate_time, date ,not_null},
    {operate_type, varchar ,not_null},
    {operate_objname, varchar ,not_null},
    {operate_objinfo, varchar ,not_null},
    {description, varchar ,not_null},
    {user_name, varchar ,not_null} 	
];

%%
table (schedule)->
[
    {name, varchar ,not_null},
    {stype, varchar ,not_null}, 	
    {sunday, varchar ,not_null},
    {monday, varchar ,not_null},
    {tuesday, varchar ,not_null},
    {wednesday, varchar ,not_null},
    {thursday, varchar ,not_null},
    {friday, varchar ,not_null},
    {saturday, varchar ,not_null}
];

%%
table (user_info_right)->
[
    refers_to (user_spl),
    refers_to (subgroup),
    {rights, text ,not_null}	
];

%%
table (user_spl)->
[
    {name , integer ,not_null},
    {password, varchar ,not_null},
    {ldapserver, varchar ,not_null}, 	
    {ldapsecurity, varchar ,not_null},
    {title, varchar ,not_null},
    {description, varchar ,not_null},
    {disabled, boolean ,not_null},
    {cpe, varchar ,not_null},
    {nt, varchar ,not_null},
    {unix, varchar ,not_null}
];

%%
table (weekday)->
[
    {day, integer ,not_null},
    {time_start, date ,not_null},
    {time_end, date ,not_null},
    {enabled, boolean ,not_null}
].























































