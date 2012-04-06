-module(profile).
%-export([predicateMap/2]).
-export([create/2, get/1, update/2, delete/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("config.hrl").

%predicateMap(Field, Operator)->
%    case Field of
%         "id" ->
%            case Operator of
%                "=" ->
%                    {ok, {3, fun operator:equal/2}};
%                "in" ->
%                    {ok, {3, fun operator:in/2}};
%                _ ->
%                    {error, "profile_" ++ Field ++ "_" ++ Operator}
%            end;
%         "email" ->
%            case Operator of
%                "=" ->
%                    {ok, {17, fun operator:equal/2}};
%                "in" ->
%                    {ok, {17, fun operator:in/2}};
%                _ ->
%                    {error, "profile_" ++ Field ++ "_" ++ Operator}
%            end;
%         "password"->
%             case Operator of
%                "=" ->
%                    {ok, {18, fun operator:equal/2}};
%                _ ->
%                    {error, "profile_" ++ Field ++ "_" ++ Operator}
%            end;
%         _ ->
%            {error, "profile_" ++ Field ++ "_" ++ Operator} 
%      end.

create([{version, _}], Profile) when is_record(Profile, profile)->
    ?Trace("Profile:~p \n",[Profile]),
    NewProfile = fillProfile(Profile),
    createProfile(NewProfile);
    
create(_, Profile) when is_record(Profile, profile)->
    ?Trace("Profile:~p \n",[Profile]),
     
    Fun = fun() ->
        case mnesia:read({profile,Profile#profile.id}) of
            [] ->
                mnesia:write(Profile),
                {ok, Profile};
            [_] ->
                {error, exsited}
         end
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, Result}->
            Result;
        {_, Reason}->
            {error, Reason}
    end.
    
fillProfile(Profile)->

    XN_Email = list_to_binary(condition:to_lower(binary_to_list(Profile#profile.xn_email))),
            
    EMailVerified = Profile#profile.emailVerified,
    if
        EMailVerified =:= true; EMailVerified =:= false ->
            Profile#profile{xn_email = XN_Email};
        true ->
            Profile#profile{emailVerified = false, xn_email = XN_Email}
    end.

getNameFromEmail(<<$@, _/binary>>, Acc)->
    lists:reverse(Acc);
getNameFromEmail(<<C, Rest/binary>>, Acc)->
    getNameFromEmail(Rest, [C|Acc]);
getNameFromEmail(<<>>, Acc)->
    lists:reverse(Acc).

createProfile(#profile{xn_birthdate = null})->
    {error, "birthdate isn't specified"};
createProfile(#profile{xn_email = null})->
    {error, "email isn't specified"};
createProfile(#profile{xn_password = null})->
    {error, "password isn't specified"};
createProfile(Profile)->
    Fun = fun()->
        %Pattern = #profile{xn_email = Profile#profile.xn_email, _ = '_'},
        EMail = Profile#profile.xn_email,
        case mnesia:index_read(profile, EMail, #profile.xn_email) of
            [] ->
                ID = list_to_atom(string:to_lower(erlang:integer_to_list(common:getIntID(), 36))),
                DateTime = time:getUTCTime(),
                Author = ?Server,
                Size = mnesia:table_info(profile, size),

                Name = getNameFromEmail(EMail, []),
                Title = case Profile#profile.title of
                    null ->
                        list_to_binary(Name);
                    <<>> ->
                        list_to_binary(Name);
                    _ ->
                        Profile#profile.title
                end,
                %Title = list_to_binary(getNameFromEmail(EMail, [])),
                
                %<link rel="icon" href="http://api.ning.com/icons/profile/9985105?default=-1&crop=1%3A1" /> 
                Href = list_to_binary(["http://api.3ren.com/icons/profile/", erlang:integer_to_list(Size + 1), "?default=-1&crop=1:1"]),
                Link = {Href, <<"icon">>},

                New_Profile = Profile#profile{
                                                id = ID,
                                                title = Title,
                                                published = DateTime,
                                                updated = DateTime,
                                                author = Author,
                                                link = Link,
                                                xn_masterkey = []
                                            },
                mnesia:write(New_Profile),
                {ok, New_Profile};
               _ ->
                {error, "email exsited"}
           end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            Result;
        {_, Reason}->
            {error, Reason}
    end.
    
getProfile([H|R], Acc)->
    case mnesia:dirty_read({profile, H}) of
        [Profile]->
            getProfile(R, [Profile|Acc]);
        _ ->
            getProfile(R, Acc)
   end;
getProfile([], Acc)->
    lists:reverse(Acc).

getProfileByEMail([H|R], Acc)->
    case mnesia:dirty_index_read(profile, H, #profile.xn_email) of
        [Profile]->
            getProfileByEMail(R, [Profile|Acc]);
        _ ->
            getProfileByEMail(R, Acc)
   end;
getProfileByEMail([], Acc)->
    lists:reverse(Acc).

get([{application, ApplicationQS}, {profile, ProfileQS}|_])->
    ?Trace("ApplicationQS:~p \n",[ApplicationQS]),
    ?Trace("ProfileQS:~p \n",[ProfileQS]),

    Conditions = condition:parse(ProfileQS), 
    ?Trace("Conditions:~p \n",[Conditions]),
    
    Fun = fun()->
        case Conditions of
            [{"email", "=", EMailValue}|OtherConditions]->
                NewValue = condition:to_lower(string:strip(EMailValue, both, $')),
		EMail = list_to_binary(NewValue),
		Profiles = mnesia:dirty_index_read(profile, EMail, #profile.xn_email),
		case OtherConditions of
		    [{"password", "=", Password}]->
			case Profiles of
			    [Profile]->
				case Profile#profile.xn_password =:= list_to_binary(Password) of
				    true ->
					{ok,[Profile]};
				    _ ->
					{ok, []}
				 end;
			     _ ->
				 {ok, []}
			end;
		    _ ->
			{ok, Profiles}
	       end;
            [{"email", "in", EMailListString}]->
                EMailList = condition:parseList(binary, condition:to_lower(EMailListString)),
                {ok, getProfileByEMail(EMailList, [])};
            [{"id", Operator, IDValue}] ->
               case Operator of
                   "in" ->
                       ProfileIDS = condition:parseList(atom, IDValue),
                       {ok, getProfile(ProfileIDS, [])};
                    "=" ->
                        {ok, mnesia:dirty_read({profile, list_to_atom(string:strip(IDValue, both, $'))})};
                    _ ->
                        {error, id_condition_error}
                end;
             _ ->
                {error, get_condition_error}
       end
    end,
                    
    case mnesia:transaction(Fun) of
        {atomic, {ok, Profiles}}->
            common:extractRecord([], Profiles);
        {atomic, Error}->
            Error;
        {abort, Reason} ->
            {error, Reason}
    end.

updateProfile(NewProfile, OldProfile)-> 

   New_Title = NewProfile#profile.title,
   Title = case New_Title of
	null->
            OldProfile#profile.title;
	_ ->
	   New_Title
   end,

   New_Summary = NewProfile#profile.summary,
   Summary = case New_Summary of
	null->
	     OldProfile#profile.summary;
	_ ->
	   New_Summary
   end,
    
    New_EMailVerified = NewProfile#profile.emailVerified,
    
    EMailVerified = if
        New_EMailVerified =:= true; New_EMailVerified =:= false ->
            New_EMailVerified;
        true ->
            OldProfile#profile.emailVerified
    end,
    
    New_XN_Email = NewProfile#profile.xn_email,
    XN_Email = if
        New_XN_Email =/= null ->
            list_to_binary(condition:to_lower(binary_to_list(New_XN_Email)));
            %New_XN_Email;
        true ->
            OldProfile#profile.xn_email
    end,
    
    New_XN_Password = NewProfile#profile.xn_password,
    XN_Password = if
        New_XN_Password =/= null ->
            New_XN_Password;
        true->
            OldProfile#profile.xn_password
    end,

%    New_XN_MasterKey = NewProfile#profile.xn_masterkey,
%    XN_MasterKey = if
%        New_XN_MasterKey =/= null ->
%            New_XN_MasterKey;
%        true->
%            OldProfile#profile.xn_masterkey
%    end,
    
    %<link rel="icon" href="http://api.ning.com/icons/profile/9985105?default=-1&crop=1%3A1" /> 
    New_Link = NewProfile#profile.link,
    Link = case New_Link of
        null ->
            OldProfile#profile.link;
        _ ->
            New_Link
    end,
   
   OldProfile#profile{
                        title = Title,
                        summary = Summary,
                        updated = time:getUTCTime(),
                        emailVerified = EMailVerified,
                        link = Link,
                        xn_zipcode  =   NewProfile#profile.xn_zipcode,
                        xn_relationship=  NewProfile#profile.xn_relationship,
                        xn_gender=  NewProfile#profile.xn_gender,
                        xn_location=  NewProfile#profile.xn_location,
                        xn_country=  NewProfile#profile.xn_country,
                        xn_birthdate=  NewProfile#profile.xn_birthdate,
                        xn_email  = XN_Email,
                        xn_password =  XN_Password
                        %xn_masterkey  = XN_MasterKey
                        }.

update([{application, ApplicationQS}, {profile, ProfileQS}|_], Profile)->

   ?Trace("ApplicationQS:~p \n",[ApplicationQS]),
   ?Trace("ProfileQS:~p \n",[ProfileQS]),

    Conditions = condition:parse(ProfileQS),
    
    ?Trace("Condition:~p \n",[Conditions]),

    case Conditions of
        [{"id", "=", Value}] ->
            ID = list_to_atom(string:strip(Value, both, $')),
%            Reader = fun()->
%                Q = qlc:q ([X || X <- mnesia:table(profile), X#profile.xn_id =:= ID]),
%                qlc:e(Q) 
%            end,
            Fun = fun()->
                case mnesia:read({profile, ID}) of
                    [ProfileOld] ->
                        ProfileNew = updateProfile(Profile, ProfileOld),
                        OldEMail = ProfileOld#profile.xn_email,
                        NewEMail = ProfileNew#profile.xn_email,
                        case OldEMail =:= NewEMail of
                            false ->
                                case mnesia:dirty_index_read({profile, NewEMail, #profile.xn_email}) of
                                    [] ->
                                         mnesia:write(ProfileNew),
                                         {ok, ProfileNew};
                                     _ ->
                                         {error, profile_email_used}
                                end;
                            _ ->
                                mnesia:write(ProfileNew),
                                {ok, ProfileNew}
                        end;
                    _ ->
                        {error, profile_empty}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} ->
                    Result;
                {_, Reason} ->
                    {error, Reason}
            end;
          _ ->
            {error, "condition error"}
    end.

delete([{application, ApplicationQS}, {profile, ProfileQS}|_])->
    
    ?Trace("ApplicationQS:~p \n",[ApplicationQS]),
    ?Trace("ProfileQS:~p \n",[ProfileQS]),

    Conditions = condition:parse(ProfileQS),

    ?Trace("Condition:~p \n",[Conditions]),
    
    case Conditions of
        [{"id", "=", Value}] ->
            ID = list_to_atom(string:strip(Value, both, $')),
            OID = {profile, ID},
            ?Trace("Delete OID:~p \n",[OID]),
            F = fun() ->
                mnesia:delete(OID)
            end,
            case mnesia:transaction(F) of
                {atomic, ok}->
                    {ok, deleted};
                {_, Reason}->
                    {error, Reason}
            end;
        _ ->
            {error, "condition error"}
    end.
