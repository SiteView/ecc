
%% The name of the remote server configuration table
-define(REMOTECONF_SET,'remoteMachine_settings').             %% General configuration
-define(REMOTECONF_RELTAGANDMA,'rel_TagAndMachine').             %% Remote server and the label set of relational tables

%% Remote server configuration table field REMOTECONF_SET table
-define(ISCREATEDDEFAULTTAG, "isCreatedDefaultTag").             %% Type of operating system label
                                   
%% Label group type
-define(SYSTAG_OS,"system_os").             %% Type of operating system label
-define(Device_type,"device_type").           %%Device Type label
-define(SYSTAG_STATUS_ERROR,"system_status_error").     %% Connection status types for the wrong type of label
-define(SYSTAG_METHOD,"system_method").         %% Connection method type label
-define(SYSTAG_ALL,"system_all").             %% Type 'all' the labels
-define(SYSTAG_USERDEFINE,"system_userdefine").      %% User-defined tags
-define(SYSTAG_USER_INDEX,"0:100").  %%User-defined index of the root label
-define(SYSTAG_ROOR,"root").  %%User-defined index of the root label
-define(TreeRootIndex,"0").  %%The index of all the root nodes
-define(TreeRootId,"machine_tag_root").  %%All the id of the root node

%%Device Type
-define(AllDeviceType,[{"SWITCH", "SWITCH"}, {"ROUTE_SWITCH", "ROUTE_SWITCH"},{"ROUTE", "ROUTE"}, {"FIREWALL", "FIREWALL"},{"SERVER","SERVER"}]).
%% Type of operating system


%% All types of operating systems
-define(ALLOSTYPE, [
                    {"Linux", [{"Other Linux","linux"}, 
                            {"Red Hat Enterprise Linux","redhatenterpriselinux"},
                            {"Suse Linux 10","SuseLinux10"},
                            {"Suse Linux 11","SuseLinux11"},
                            {"Ubuntu Linux","ubuntu"}]},
                    {"Hp Unix", [{"HP/UX","hp"},
                         {"HP/UX 64-bit","hp_64bit"},
                         {"Tru64 5.x","tru64"}]},
                    {"FreeBSD", [{"FreeBSD","freebsd"}]},
                    {"AIX", [{"AIX","aix"}]},
                    {"OPENSERVER", [{"OPENSERVER","openserver"}]},
                    {"Sun Solaris", [{"Sun Solaris","sunsolaris"}]},
                    {"Windows", [{"NT","nt"}]}
                    ]).
         
