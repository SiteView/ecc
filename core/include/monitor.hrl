
-ifndef(_MONITOR_H_).
-define(_MONITOR_H_,1).

-include("log.hrl").

-define(MAX_MONITORS_SKIPS,6).
-define(ID_SEPARATOR,"/").
-define(DEPENDS_PREFIX,"depends ").
-define(MIN_GROUP_INTERVAL,10).
-define(MIN_MONITOR_INTERVAL,15).

-define(RULES,rules).

-define(ID,id).
-define(CLASS,class).
-define(NAME,name).
-define(DESCRIPTION,description).
-define(FREQUENCY,frequency).
-define(ERROR_FREQUENCY,error_frequency).
-define(PARENT,parent).
-define(TITLE,title).

-define(LAST_UPDATE,last).
-define(FORCE_REFRESH,force_refresh).
-define(MONITOR_DELAY_BETWEEN_REFRESH,monitor_delay_between_refresh).
-define(INITIAL_MONITOR_DELAY,initial_monitor_delay).
-define(DISABLED_DESCRIPTION,disabled_description).
-define(DISABLED,disabled).
-define(RUN_OWNRULES,run_ownrules).
-define(NO_DATA,nodata).
-define(CATEGORY,category).
-define(ALERT_LEVEL,alert_level).
-define(LAST_CATEGORY,last_category).
-define(GETHOSTNAME,gethostname).

-define(MEASUREMENT,measurement).
-define(MEASUREMENT_PROPERTY,measurement_property). %%Identifies an attribute is measurement

-define(SAMPLE,sample).
-define(VERFIY_ERROR,verfiy_error).
-define(STATE_STRING,state_string).
-define(DEPENDS_ON,depends_on).
-define(DEPENDS_CONDITION,depends_condition).
-define(OPERATION_ERROR_CODE,operation_error_code).
-define(OPERATION_ERROR_MESSAGE,operation_error_message).
-define(ALERT_DISABLED,alert_disabled).
-define(ALERT_DISABLED_DESCRIPTION,alert_disabled_description).

-define(GOOD_CATEGORY,good).
-define(WARNING_CATEGORY,warning).
-define(ERROR_CATEGORY,error).

-define(TIMED_DISABLE,timed_disable).

-define(NTCounterSummaryMax,3).

-define(MAX_RECENT_MONITOR,20).

-define(APP,app_).

%%templates path
-define(TEMPLATES_PERFMON,"templates.perfmon").


%% browsable monitor 
-define(BROWSE_COUNTER,"browser_counter").
-define(BROWSE_SPLIT,"@").

%  define page parameter name
-define(PAGE_PARAMS,'_PAGE_PARAMS').

-define(DEFAULT_UPAMOUNT,"5").

%%  License
-define(ECCLICFILENAME, "SiteViewECC9.Lic").

%%  Password Management
-define(PWDMNG, pwdManagement).
-define(PWDMNG_NAME,pwdManagement_Name).
-define(PWDMNG_PWD,pwdManagement_Pwd).

-record(pwdSet,{
                                id=[],
                                name=[],
                                pwd=[]
                        }).
                        
%% --------------------- Remote server ------------------------------
-define(PWDMODE_DEFAULT,"other").

-record(machine,{
                 id,
				 name="",
				 host="",
				 login="",
				 passwd="",
				 trace="0",
				 os="nt",
				 status="unknown",
				 method="",    			  %% method="Snmp" Snmp network equipment for more than NT / Unix share options, the middle part of the options for the Unix
				 prompt="#",
				 loginprom="login",
				 passwdprom="password",
				 secondprom="",
				 secondresp="",
				 initshell="",
				 remoteencoding="",
				 sshcommand="",    		  %%The following options for the SSH Advance
				 sshclient="interJavalib",
				 sshport=22,
				 disableconncaching="0",
				 connlimit=3,
				 version="",
				 keyfile="",
				 sshauthmethod="",
                 label="",          %% Label
                 total=0,           %% Device Statistics
                 type="SERVER",     %%  "SWITCH" or "ROUTER_SWITCH" or "ROUTER" or "FIREWALL" or "SERVER" 
                 other=[],          %% Other information Device
                 pwdmode="other"   %% Password mode, so if you do not read the password for the other configuration files, or read password configuration file
				%% connectionmonitor ="false"
				}).

% machine Label
-record(machine_label,{
                                   id,  
                                   name="undefined",     %label name
                                   type="nt",            %Label type, In fact belong to label
                                   index,                %Reference count 
                                   syslabel="false",     %% Does the system label
                                   hide="false",         %% Is hidden
                                   value="nt" ,          %% label value
                                   treeindex="",         %%Total root index of 1, with id to describe the tree
                                   maxchild="",
                                   parentid="",          %%Father id
                                   childrenid=[] 
                                   }).
                                   


%%  --------------------------------------------------

-define(MachLabelGroup, "machine_label").        %% Remote server, unix label group marked
-define(MachUnixLabel, "unix").                  %% Remote server, unix label group marked
-define(MachNTLabel, "nt").                      %% Remote server, nt tag group marked
-define(DefMachNTLabel, "nt_undefined").         %% Remote server, nt tag group marked
-define(DefMachUnixLabel, "unix_undefined").     %% Remote server, unix label group marked
-define(DefMachNTLabelName, "undefined").        %% Remote server, nt tag group marked
-define(DefMachUnixLabelName, "undefined").      %% Remote server, unix label group marked

-record(tr069_device,{
                                    ip,                     %device IP
                                    %deviceid,              %device id                                    
			                        manufacturer,           %
                                    oui,                    %organizationally unique identifier
                                    productclass,           %device type
                                    serialnumber,           %Serial Number
                                    profile = "",           %profile                                      
                                    deviceport = "7547",     
                                    authtype = "Basic",     %Authentication
                                    user = "",               
                                    password = "",     
                                    keyfile = "",           %ssh Certification                          
                                    acsip = "",             %The IP belongs to acs
                                    acsname = "",           %Acs name belongs
                                    timestamp,              %Timestamp
                                    keepalive = "1",        %Whether to enable heartbeat
                                    keepalivetime = "15",   %Heartbeat interval                                    
                                    state = "alive",        %Device Status
                                    label= [],              %Equipment labels
                                    description= "",
                                    total=0                                    
                                    }).
                                    
 
-record(tr069_label,{
                                   name,  %label name
                                   index   %Reference count 
                                   }).
                                   
%% General query data structure (the interface)
-record(query_condition1,{
                                where=[],
                                index=0,
                                count=0,
                                sort=[],
                                sortType="@D"
                        }).
                        
%% General query data structure (intermediate format, and dbecc interface)
-record(query_beam_condition1,{
                                where=[],
                                order=[]
                        }).

%% General query data structure (intermediate format, Where conditions)
-record(query_condition_where,{
                                where=[]        %%([{field,operation,value,relation}|T])
                        }).
                                   
-record(tr069_label_id,{
                                   number %
                                   }).                                   

-record(tr069_paramcaches,{
                                deviceid=[],       %%Own facilities id
                                paramname=[],      %%Parameter name
                                value=[],          %%Parameter value
                                is_paracache_table="true",     %%Whether the parameters of the field cache
                                updatetime=0        %%Cache parameters to modify the time
                        }).

%% Each device corresponding to a collection of all the parameters
-record(tr069_paramcachesdev,{
                                id=[],      %% id
                                params=[]   %% Parameters collection [{paramname,#tr069_paramcaches{}}|T]
                        }).

-record(tr069_download,{
            deviceid, %device id
            commandkey,%The upgrade of the string identifies
            startTime,   %Start time update 
            completeTime,%Upgrade completion time
            state %Upgrade state, respectively complete,upgrading,failure
          }).
          
%%Site
-record(tr069_upgradesite,{
                                devaccessaddr=[],       %%Devices to access address
                                clientaccessaddr=[],      %%User access address
                                username=[],          %%User name
                                password=[],     %%
                                protocol=[]     %%
                        }).

%% Upgrade Set
-record(tr069_upgradeset,{
                                manufacturer=[],       %%
                                productclass=[],      %%
                                version=[],          %%
                                filename=[],     %%
                                is_rightrow="true", %%Is immediately
                                begintime=[],          %%
                                devices=[]             %%Equipment to be upgraded
                        }).

%% upgraded file
-record(tr069_upgradefile,{
                                manufacturer=[],       %%
                                productclass=[],      %%
                                version=[],          %%
                                filename=[],     %%
                                filetype=[],     %%
                                filesize="0",       %%
                                is_upgradefile = "true" %%Whether the upgrade file
                        }).

%% Upgrade Status
-record(tr069_upgradestatus,{
                                commandkey,         %%commandkey 
                                devid = [],         %%device id
                                upgradestatus=[],   %%Upgrade Status
                                version=[],             %%Existing version
                                datetime=[],               %%time
                                begindatetime=[],          %%
                                enddatetime=[],             %%
                                ip=[],  %%ip
                                mac=[],     %% Serial Number
                                group=[],   %% group
                                manufacturer, %manufacturer
                                oui,                   %organizationally unique identifier
                                productclass,   %
                                anothorname,    %% alias
                                total = 0           %% Quantity
                        }).
                        
%% Upgrade equipment at the same time the number of
-record(tr069_upgradeamount,{
                                id=[],
                                amount=?DEFAULT_UPAMOUNT  %% At the same time upgrade the five
                        }).

-record(realtimealarm,{id,manufacturer,oui,productclass,serialnumber,xevent,description="",statu="untreated",notes="",timestamp,number=1,timeofoccurrence,total=0}).

-record(schedule,{id,name,type,days=[]}). %% type="range","absolute"

-record(weekday,{enabled=true,day,time_start,time_end}).

%%browsable monitor counter
-record(counters,{id,name,default=false}).

-record(monitorlog,{id,name,time,category,desc,measurement,class,groupname}).

-record(perf_counter,{object,counterName,instance,objectID,counterID}).

-record(perf_result,{counterType,measurement,baseMeasurement,lastMeasurement,lastBaseMeasurement,measurementFreq,measurementTicks,measurementTime,lastMeasurementTime,lastMeasurementTicks,percent,perSec,precision,value}).

%% ettings
-record(additional_email_settings,
		{name,						% 
		email,						% email address
		disable="false",			% is disable
		template,					%
		schedule					%  shedule [{1,"enabled",{"20:00","24:00"}}]
		}).
		
-record(additional_sms_settings,
		{name,						% 
		email,						% email address
		disable="false",			% is disable
		template,					% 
		schedule					%  shedule [{1,"enabled",{"20:00","24:00"}}]
		}).

-record(additional_snmp_settings,
		{
		name,
		disable,
		snmp_host,
		snmp_objectid,
		snmp_objectid_other,
		snmp_community,
		snmp_generic,
		snmp_specific,
		snmp_trap_version
		}).

%  monitor set template
-record(monitor_set,{title,desc,variable,monitors}).

%  dynamic update
-record(dynamic_update_sets,{id,
							mset,				 % Select the Monitor Set Template used to create new monitors.
							subgroup_name,		 % Monitor group name to be assigned to each set of monitors created using the above template.
							group_name,			 % name for the group to be created to contain all subgroups created using the template set.
							parent_group,		 % an existing group to which the above container group will be added as a subgroup.
							update_frequency,    % Frequency (in seconds) that  will query the MIB or database for new nodes and create monitor sets for new nodes.
							exclude_ip,			 % Enter IP addresses to be excluded from the Update Set, for example, the default gateway IP. 
							title,				 % Optional title for this Dynamic Update Set. The default title is the server address
							snmp_search,		 % SNMP MIB Search (it is record 'snmp_mib_search')
							database_search		 % Database Search (it is record 'database_search')
							}).
							
-record(snmp_mib_search,{server_address,snmp_oid,snmp_oid_other,pool_included,snmp_community}).
-record(database_search,{url,driver,sql,username,password,connect_timeout,query_timeout}).

%% URL Montor
-record(urlresults,{status=-1,totalDuration=0,totalBytes=0,lastModified=0,currentDate=0,body="",head=[],errorMessage="",contentBuffer="",redirectBuffer="",sessionBuffer=""}).

-define(URLok, 200).
-define(kURLok, 200).
-define(kXMLElementNotFoundError, -986).
-define(kXMLFormatError, -987).
-define(kXMLElementMatchError, -988).
-define(kURLContentErrorFound, -989).
-define(kURLContentElementMissing, -990).
-define(kURLNoStatusError, -991).
-define(kMonitorSpecificError, -992).
-define(kURLNoRouteToHostError, -993).
-define(kDLLCrashedError, -994).
-define(kURLContentChangedError, -995).
-define(kURLTimeoutError, -996).
-define(kURLBadHostNameError, -997).
-define(kURLNoConnectionError, -998).
-define(kURLContentMatchError, -999).
-define(kURLUnknownError, -1000).
-define(kDNSIPAddressMismatch, -1001).
-define(kURLRemoteMonitoringError, -1002).
-define(kURLSecurityMismatch, 12157).
-define(kURLCertificateExpired, 12037).
-define(kURLCertificateNameError, 12038).
-define(kURLMissingClientCertificate, 12044).
-define(kURLInvalidCA, 12045).

-define(statusMapping,[
                 {200,"ok"},
                 {201,"created"},
                 {202,"accepted"},
                 {203,"non-authoratative"},
                 {204,"no content"},
                 {205,"reset content"},
                 {206,"partial content"},
                 {301,"document moved"},
                 {302,"document moved"},
                 {303,"document moved"},
                 {307,"document moved"},
                 {400,"bad request"},
                 {401,"unauthorized"},
                 {403,"forbidden"},
                 {404,"not found"},
                 {407,"proxy authentication required"},
                 {500,"server error"},
                 {501,"not implemented"},
                 {502,"proxy received invalid response"},
                 {503,"server busy"},
                 {504,"proxy timed out"},
                 {-992,"error"},
                 {-990,"content element missing"},
                 {-995,"content changed"},
                 {-996,"timed out reading"},
                 {-987,"XML format error"},
                 {-986,"XML element not found"},
                 {-988,"XML element value mismatch"},
                 {-999,"content match error"},
                 {-989,"content error found"},
                 {-998,"unable to connect to server"},
                 {-997,"unknown host name"},
                 {-994,"internal error in WinInet library"},
                 {-993,"unable to reach server"},
                 {-991,"no status in reply from server"},
                 {-1001,"ip address does not match"},
                 {12157,"insufficient encryption, probably needs 128 bit Internet Explorer"},
                 {12037,"secure certificate expired"},
                 {12038,"secure certificate name does not match host name"},
                 {12044,"requires client certificate authentification"},
                 {12045,"certificate authority not registered in Internet Explorer"},
                 {-1002,"unable to connect to remote monitoring server"}
                ]).
				
-record(operation_log,{id,create_time,record_state,user_id,ip,operate_time,operate_type,operate_objname,operate_objinfo,dstr}).

-endif.



