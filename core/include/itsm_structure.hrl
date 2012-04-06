
%%  ************Data fields predefined fields

%%---- itsm_sys_sign field name

-define(ITSMField_SysSign_CurrNumber,"sys_sign_curr_number").   %%Marking system The current work order number table  id
-define(ITSMField_SysSign_CurrNumber_Title,"Number").   %%Marking system The current work order number field name    title


%% --- choicelists Predefined fields

%% Table 
-define(ITSMChoice_Table_Task,"task").   %%Choice,task
-define(ITSMChoice_Table_Incident,"incident").   %%Choice,Table field: incident
-define(ITSMChoice_Table_Escalation,"Escalation").   %%Choice, Table field: Escalation

%% Element 
-define(ITSMChoice_Element_Impact,"impact").   %%Choice,Elememnt field impact
-define(ITSMChoice_Element_Urgency,"urgency").   %%Choice,Elememnt field urgency
-define(ITSMChoice_Element_Priority,"priority").   %%Choice,Elememnt fieldpriority
-define(ITSMChoice_Element_Category,"category").   %%Choice,Elememnt field category
-define(ITSMChoice_Element_Escalation,"escalation").   %%Choice,Elememnt field escalation

%% Status 
-define(OpenStatus,"Open").   %%Work as a single initial state"1", open state
%% Step
-define(OpenStep,"1").   %%Work as a single initial state"1", open state

%% Filter
%% --Default filter
-define(DefaultFielters,"def_filters").    %%Work as a single initial state"1", open state

%% Accessories storage location
-define(AttachMentPath, "/wwwroot/attachments/").
-define(AttachMentUrl, "/attachments/").

%% **********************Data table structure**********************

%% Drop-down list of table records
-record(choice_lists,{
                                id=[],          
                                table=[],       
                                element=[],      
                                language=[],          
                                label=[],     
                                value=[],       
                                dependent_value=[],     
                                hint=[],           
                                sequence=[],        
                                inactive="false"    
                        }).
                        
%% Work order table records
-record(itsm_incident,{
                                id=[],          %%id
                                number=[],       %%Work Order Number %% This is not the only mark, but still give way to generate a work order number
                                caller=[],      %%Reporter
                                location=[],          %%
                                configuration_item=[],     %%ci Assets
                                impact=[],       %%Value
                                urgency=[],     %%Dependent value
                                priority=[],            %%Hint
                                knowledge=[],        %%Sort Sequence
                                short_description=[],    %%Summary
                                opened=[],    %%Is invalid
                                opened_by=[],    %%Is invalid
                                incident_state=[],    %%Is invalid
                                category=[],    %%Is invalid
                                escalation=[],    %%Is invalid
                                assignment_group=[],    %%Is invalid
                                assigned_to=[],    %%Is invalid
                                additional_comments=[],    %%comments
                                work_notes=[],    %%Description
                                test=[],    %%Is invalid
                                testhoitest=[],    %%Is invalid
                                htmltest=[],    %%Is invalid
                                follow_up=[],    %%Is invalid
                                created=[],    %%Is invalid
                                parent=[],    %%Is invalid
                                child=[],    %%Is invalid
                                status=?OpenStatus,          %% Ticket Status,default Open
                                project=[],      %% Actually refers to the application id
                                step=?OpenStep,      %% Work Order Step
                                
                                %% The following are not saved
                                total=0             %% The number of work orders
                        }).

%% Comments Table records
-record(itsm_sys_comments,{
                                id=[],          %%id
                                user=[],        %%user id
                                is_edit="false",    %% Is edited
                                createdatetime=[],     %% Create time
                                content=[],         %% Note content
                                incident_id=[]      %% The work belongs to 
                        }).

%% Application Form Records
-record(itsm_sys_app_application,{
                                id=[],          %%id
                                title=[],
                                name=[],
                                hint=[],
                                active=[],
                                order=[],
                                category=[],
                                roles=[],
                                device_type=[],
                                workflow=[]         %%Workflow associated with
                        }).

%% Applications and modules relationship table record
%%-record(application_ref_module,{
%%                                application=[], %%Applications id
%%                                module=[]       %%
%%                        }).
 
%% Application modules table records 
-record(itsm_sys_app_module,{
                                id=[],          %%id
                                title=[],
                                table=[],
                                order=[],
                                application=[],
                                hint=[],
                                active="true",
                                image=[],
                                link_type=[],
                                view_name=[],
                                roles=[],
                                filter=[],      %%filter id
                                arguments=[]
                        }).
                        
%% Filter table records
-record(itsm_filters,{
                                id=[],          %%id
                                title=[],
                                table=[],
                                condition=[]
                        }).
                        
%% System table record label
-record(itsm_sys_sign,{
                                id=[],          %% id, example:sys_sign_curr_number
                                title=[],       %% example Number
                                value=[]        %% example 0010145
                        }).
                        
%% ?? ???
-record(itsm_attachment,{
                                id=[],          %% id
                                filename=[],    %% filename
                                filesize=[],    %% filesize
                                filetype=[],    %% filetype
                                updatetime=0,   %% updatetime
                                upuser=[],      %% Uploaded by
                                incident=[],     %% Work Order Number
                                filepath=[],     %% filepath
                                url=[],          %% url
                                type="attachment"   %% "attachment":The default is general accessories,
				                     %% "screenshot": Image Accessories
                        }).
                        
                        
                        
%% ************************System internal data structure, data storage has nothing to do with

%% Accessories Category
-record(attach_group,{
                                commattach=[],  %% general accessories,
                                screenshot=[]   %% Screenshots
                        }).

%%Filter data structure
-record(fielter_condition,{
                                relation=[],          %%Relationship between filter
                                field=[],       %% field
                                operation=[],        %% 
                                value=[]        %%
                        }).
                        
%% General query data structure (the interface)
-record(query_condition,{
                                where=[],
                                index=0,
                                count=0
                        }).
                        
%% General query data structure (intermediate format, and dbecc interface)
-record(query_beam_condition,{
                                where=[],
                                order=[]
                        }).

                        





-record(workflowlist,{
                name,
                description,
                status,
                schemes,
                number_of_steps %int
             }).
 
%
 -record(status,{
                name, % string
                description, % string
                %mode,    %{“active”|“inactive ”} string
                workflows,% list
                icon_url,  %Picture list string,
                index     %Reference count int
             }).
 
-record(workflow,{
                id,        %Workflowid 
                name, %Workflow Name string
                description, %Workflow description string
                %status, % string
                schemes, %Case List list
                steps,     %List of steps list
                index      %Reference count int               
            }).             

-record(step,{ 
                workflowid, %Belongs workflowid
                step_id, %The unique workflow
                name, %step name string
                linked_status, %Connection status
                transitions,    % transition id list {[id]}
                index              %                
            }).  

%
-record(transition,{
                transition_id, %The application of globally unique int
                originating_steps, %Previous state id string                
                name, %transition string
                destination_step, %After a state id string
                screens % tulpe list,[Screen_name],Screen              
            }). 
            
%Distribution of the application of globally unique transition id
-record(transition_id,{
                number  %int        
            }).            
    
            
%Distribution of the application of globally unique workflow id
-record(id,{
                number  %int        
            }).      
    
-record(screen,{
                position,  
                screen_name %string         
            }).  


