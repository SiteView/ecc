-module(app).
%-export([predicateMap/0]).
-export([create/2, get/1, update/2, delete/1,all/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("config.hrl").
-include("dbcs_common.hrl").

create([{version, _}|_], Application) when is_record(Application, application)->
    ?Trace("Application:~p \n",[Application]),
    
    %-record(application, {id, title, published, updated, author, description, 
    %                        viewSourceUrl, privateSource, runOwnAds, 
    %                        xn_premiumService,  xn_tag, xn_category, xn_domain, 
    %                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit}).
    %{
    %         application,
    %         "siteview",
    %         {{type,"text"},"siteview"},
    %         "2008-05-15T16:24:20.255Z",
    %         "2008-08-10T04:02:12.363Z",
    %         {[name, "1j0hpe79jbdh4"]}, 
    %         {{type, "text"},"the social network for siteview."},
    %         "http://www.ning.com/view-source.html?appUrl=siteview", 
    %         "false",
    %         "false",
    %         [
    %            {{type, "premium"}, "false"}
    %         ],
    %         ["tag"],
    %         [
    %            {{term, "term"}, {label, "label"}}
    %         ],
    %         ["siteview.3ren.com"],
    %         "2",
    %         "true",
    %         {{href, "http://api.ning.com/icons/appatar/2108193?default=-1"}, {rel, "icon"}},
    %         2108193,
    %         10000000
    %  }

    NewApplication = fillApplication(Application),
    createApplication(NewApplication).

createApplication(#application{id = null})->
    {error, "application id isn't specified"};
createApplication(#application{author = null})->
    {error, "author isn't specified"};
createApplication(Application)->
    Fun = fun() ->
        ApplicationID = Application#application.id,
        ReadResult = mnesia:read({application, ApplicationID}),
        case ReadResult of
            [] ->
                ProfileID = Application#application.author,
                OID = {profile, ProfileID},
                case mnesia:read(OID) of
                    [Profile] ->
                        MasterKey = Profile#profile.xn_masterkey,
                        NewMasterKey = case MasterKey of
                            null ->
                                [{ApplicationID, null}];
                            _ ->
                                case lists:keysearch(ApplicationID, 1, MasterKey) of
                                    false ->
                                        [{ApplicationID, null}|MasterKey];
                                     _ ->
                                        MasterKey
                                end
                        end,
                        
                        Count = length(NewMasterKey),
                        if
                            Count > ?MaxCreateApplicationCount ->
                                {error, max_create_application_count};
                            true->
                                NewProfile = Profile#profile{xn_masterkey = NewMasterKey},
                                mnesia:write(NewProfile),
                                mnesia:write(Application),
                                {ok, Application}
                        end;
                    [] ->
                        {error, get_profile_empty}
                end;
            [_]->
                {error, existed}
        end
    end,
    case mnesia:transaction(Fun) of
            {atomic, Result}->
                Result;
            {_, Reason}->
                {error, Reason}
    end.

fillApplication(Application)->
                  
%    ID = case ApplicationID of
%            null ->
%                null;
%            [] ->
%                null;
%            _ ->
%                list_to_atom(ApplicationID)
%         end,
    ApplicationID = Application#application.id,
    ViewSourceUrl = case  ApplicationID of
                        null ->
                            null;
                        _ ->
                            list_to_binary(["http://www.3ren.com/view-source.html?appUrl=", atom_to_list(ApplicationID)])
                    end,
                    
    Temp1 = Application#application.privateSource,
    PrivateSource = case Temp1 of
                            null ->
                                false;
                           true ->
                               Temp1
                    end,
    
    Temp2 = Application#application.runOwnAds,
    RunOwnAds = case Temp2 of
                    null->
                        false;
                    _ ->
                       Temp2
                end,
   
    XN_PremiumService = case Application#application.xn_premiumService of
                            null ->
                                [];
                            _ ->
                                Application#application.xn_premiumService
                        end,
                        
    XN_Tag = case Application#application.xn_tag of
                            null ->
                                [];
                            _ ->
                                Application#application.xn_tag
                        end,
    
    XN_Category = case Application#application.xn_category of
                            null ->
                                [];
                            _ ->
                                Application#application.xn_category
                   end,
                   
    XN_Domain = case Application#application.xn_domain of
                            null ->
                                [];
                            _ ->
                                Application#application.xn_domain
                   end,
                   
    ApplicatonSequenceNumber = common:getStrID(),
    DateTime = time:getUTCTime(),
    Href = list_to_binary(["http://api.3ren.com/icons/appatar/", ApplicatonSequenceNumber, "?default=-1"]),

%    -record(application, {id, title, published, updated, author, description,
%                        viewSourceUrl, privateSource, runOwnAds,
%                        xn_premiumService,  xn_tag, xn_category, xn_domain,
%                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit}).
      
      
    Application#application
    {
        %id = ID,
        published = DateTime,
        updated = DateTime,
        viewSourceUrl = ViewSourceUrl,
        privateSource = PrivateSource,
        runOwnAds = RunOwnAds,
        xn_premiumService = XN_PremiumService,
        xn_tag = XN_Tag,
        xn_category = XN_Category,
        xn_domain = XN_Domain,
        xn_active = true,
        link = {Href, <<"icon">>},
        seqenceNumber = list_to_binary(ApplicatonSequenceNumber),
        storeLimit = ?Storage
    }.
    
get([{application, ApplicationQS}|_])->
    ?Trace("ApplicationQS:~p \n",[ApplicationQS]),
    Fun = fun()->
         mnesia:read({application, list_to_atom(ApplicationQS)})
     end,
     case mnesia:transaction(Fun) of
         {atomic, Application}->
              common:extractRecord([], Application);
          {_, Reason}->
              {error, Reason}
     end.
    
update([{application, ApplicationQS}|_], Application)when is_record(Application, application) ->

    ?Trace("ApplicationQS:~p \n",[ApplicationQS]),
    Fun = fun() -> 
        ReadResult = mnesia:read({application, list_to_atom(ApplicationQS)}),
        case ReadResult of
            [OldApplication]->
                NewApplication = updateApplication(Application, OldApplication),
                mnesia:write(NewApplication),
                {ok, NewApplication};
             _ ->
                {error, "get not any application to update"}
        end
    end,
    case mnesia:transaction(Fun) of
           {atomic, TransactionResult} ->
                TransactionResult;
           {_, Reason} ->
                {error, Reason}
   end.

updateApplication(NewApplication, OldApplication)
    when is_record(NewApplication, application), is_record(OldApplication, application)->
    
    %-record(application, {id, title, published, updated, author, description, 
    %                        viewSourceUrl, privateSource, runOwnAds, 
    %                        xn_premiumService,  xn_tag, xn_category, xn_domain, 
    %                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit}).
    %{
    %         application, 
    %         "siteview", 
    %         {{type,"text"},"siteview"}, 
    %         "2008-05-15T16:24:20.255Z", 
    %         "2008-08-10T04:02:12.363Z", 
    %         {[name, "1j0hpe79jbdh4"]}, 
    %         {{type, "text"},"the social network for siteview."}, 
    %         "http://www.ning.com/view-source.html?appUrl=siteview", 
    %         "false",
    %         "false",
    %         [
    %            {{type, "premium"}, "false"}
    %         ],
    %         ["tag"],
    %         [
    %            {{term, "term"}, {label, "label"}}
    %         ],
    %         ["siteview.3ren.com"],
    %         "2",
    %         "true",
    %         {{href, "http://api.ning.com/icons/appatar/2108193?default=-1"}, {rel, "icon"}},
    %         2108193,
    %         10000000
    %}
    
    New_Title = NewApplication#application.title, 
    Title = case New_Title of
        null ->
            OldApplication#application.title;
        _ ->
           New_Title
    end,
   
    Updated = time:getUTCTime(),

    New_Description = NewApplication#application.description, 
    Description = case New_Description of
        null ->
                OldApplication#application.description;
        _ ->
           New_Description
    end,
    
    New_viewSourceUrl = NewApplication#application.viewSourceUrl,
    ViewSourceUrl = case New_viewSourceUrl of
        null ->
            OldApplication#application.viewSourceUrl;
        _ ->
            New_viewSourceUrl
    end,
    
    New_PrivateSource = NewApplication#application.privateSource,
    PrivateSource =  if
        New_PrivateSource =:= true orelse New_PrivateSource =:= false ->
            New_PrivateSource;
        true ->
            OldApplication#application.privateSource
    end,
    
    New_RunOwnAds = NewApplication#application.runOwnAds,
    RunOwnAds =  if
        New_RunOwnAds =:= true orelse New_RunOwnAds =:= false ->
            New_RunOwnAds;
        true ->
            OldApplication#application.runOwnAds
    end,
    
    %xn_premiumService,
    %         [
    %            {{type, "premium"}, "false"}
    %         ],
    New_PremiumService = NewApplication#application.xn_premiumService,
    PremiumService = case New_PremiumService of
        null -> 
            OldApplication#application.xn_premiumService;
        _ ->
            New_PremiumService
    end,
    
    %xn_tag, 
    %         ["tag"],
    New_Tag = NewApplication#application.xn_tag,
    Tag = case New_Tag of
        null -> 
            OldApplication#application.xn_tag;
        _ ->
            New_Tag
    end,
    
    %xn_category, 
    %         [
    %            {{term, "term"}, {label, "label"}}
    %         ],
    New_Category = NewApplication#application.xn_category,
    Category = case New_Category of
        null ->
            OldApplication#application.xn_category;
        _ ->
            New_Category
    end,
    
    %xn_domain,
    %         ["siteview.3ren.com"],
    New_Domain = NewApplication#application.xn_domain,
    Domain = case New_Domain of
        null ->
            OldApplication#application.xn_domain;
        _ ->
            New_Domain
    end,

    %New_XN_Application = NewContent#content.xn_application,
    %New_Link = NewContent#content.link, 
    %                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit}).
    %         "2",
    %         "true",
    %         {{href, "http://api.ning.com/icons/appatar/2108193?default=-1"}, {rel, "icon"}},
    %         2108193,
    %         10000000
    New_XN_LayoutVersion = NewApplication#application.xn_layoutVersion,
    XN_LayoutVersion = case New_XN_LayoutVersion of
        null ->
            OldApplication#application.xn_layoutVersion;
        _ ->
            New_XN_LayoutVersion
    end,
    
    New_XN_Active = NewApplication#application.xn_active,
    XN_Active =  if
        New_XN_Active =:= true orelse New_XN_Active =:= false ->
            New_XN_Active;
        true ->
            OldApplication#application.xn_active
    end,
    
    New_Link = NewApplication#application.link,
    Link = case New_Link of
        null ->
            OldApplication#application.link;
        _ ->
            New_Link
    end,
    
    New_StoreLimit = NewApplication#application.storeLimit,
    StoreLimit = case New_StoreLimit of
        null ->
            OldApplication#application.storeLimit;
        _ ->
            New_StoreLimit
    end,
    
    OldApplication#application
    {
        title = Title, 
        updated = Updated, 
        description = Description,                    
        viewSourceUrl = ViewSourceUrl, 
        privateSource = PrivateSource, 
        runOwnAds = RunOwnAds,                      
        xn_premiumService = PremiumService,  
        xn_tag = Tag, 
        xn_category = Category, 
        xn_domain = Domain,                    
        xn_layoutVersion = XN_LayoutVersion, 
        xn_active = XN_Active, 
        link = Link, 
        storeLimit = StoreLimit
    }.

delete([{application, ApplicationQS}, {profile, ProfileQS}])->
    ?Trace("ApplicationQS:~p \n",[ApplicationQS]),
    ?Trace("ProfileQS:~p \n",[ProfileQS]),
    
    ProfileConditions = condition:parse(ProfileQS), 
    ?Trace("ProfileConditions:~p \n",[ProfileConditions]),
    
    ApplicationID = list_to_atom(ApplicationQS),
    case ProfileConditions of
        [{"id","=", ProfileID}]->
            OID = {profile, list_to_atom(string:strip(ProfileID, both, $'))},
            Fun = fun()->
                case mnesia:read(OID) of
                    [Profile]->
                        MasterKey = Profile#profile.xn_masterkey,
                        NewMasterKey = case MasterKey of
                            null ->
                                [];
                            _ ->
                                lists:keydelete(ApplicationID, 1, MasterKey)
                        end,
                        
                        NewProfile = Profile#profile{xn_masterkey = NewMasterKey},
                        mnesia:write(NewProfile),
                        mnesia:delete({application, ApplicationID}),
                        {ok, application_delete};
                    _ ->
                        {error, get_profile_empty}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result}->
                    Result;
                {_, Reason}->
                    {error, Reason}
            end;
        _ ->
            {error, "profile condition error"}
    end.
all() ->
    case rpc_proxy:call(server_conf:get_db_node(),ets,tab2list,[application]) of
          Apps when is_list(Apps)  ->
               F = fun(X) -> X#application.id end,
               lists:map(F,Apps);
          _  -> []    
    end.