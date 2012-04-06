%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Auth.
%% 
%% Description: Tools for URL and URL series monitor.
%% Sofar it surpport basic and digest authorization
-module(url_auth).
-compile(export_all).
-export([basic_auth/2,digest_auth/5]).

%% @spec basic_auth(User,Passwd) -> Header
%% where
%% User = string()
%% Passwd = string()
%% Header = [{Key,Value}]
%% Key = string()
%% Value = string()
%% @doc create basic authorization head.
basic_auth("",_)->[];
basic_auth(_,"")->[];
basic_auth(User,Passwd) ->
    UserPasswd = base64:encode_to_string(User ++ ":" ++ Passwd),
    [{"Authorization","Basic " ++ UserPasswd}].

%% @spec digest_auth(User,Passwd,ResponseHeaders,URL,Method) -> Header
%% where
%% User = string()
%% Passwd = string()
%% ResponseHeaders = [{Key,Value}]
%% URL = string()
%% Method = string()
%% Header = [{Key,Value}]
%% Key = string()
%% Value = string()
%% @doc create digest authorization head using response http header.
digest_auth("",_,_,_,_)->[];
digest_auth(_,"",_,_,_)->[];
digest_auth(User,Passwd,ResponseHeaders,URL,Method) ->
    %%Parse challenge response
    HeaderDict = dict:from_list(ResponseHeaders),
    {ok, AuthenticateHeader} = dict:find("www-authenticate", HeaderDict),
    %%Strip the authorization type from the string
    AdjustedAuthHeader = string:substr(AuthenticateHeader, 8, string:len(AuthenticateHeader)-7),
    AuthFields = string:tokens(AdjustedAuthHeader, ","),
    Fun = fun(AuthField) ->
        [Key, Value] = string:tokens(AuthField, "="),
        case Value of
            [$\"|_] ->
                {string:strip(Key,both), string:strip(string:strip(Value,both, $\"), both)};
            _->
                {string:strip(Key,both), string:strip(Value, both)}
        end
    end,
    AuthPairs = [Fun(AuthField) || AuthField <- AuthFields],
    AuthDict = dict:from_list(AuthPairs),
    {ok, Nonce} = dict:find("nonce", AuthDict),
    {ok, Realm} = dict:find("realm", AuthDict),
    {ok, Algorithm} = dict:find("algorithm", AuthDict),
    {ok, Qop} = dict:find("qop", AuthDict),
    {ok, Opaque} = dict:find("opaque", AuthDict),
    %%Calculate H(A1)
    Cnonce = get_cnonce(),
    NonceCount = "00000001",
    HA1 = case Algorithm of
        "MD5" ->
            hexdigest([User , ":" , Realm , ":" , Passwd]);
        "MD5-sess" ->
            hexdigest([hexdigest([User , ":" , Realm , ":" , Passwd]),":",Nonce,":",Cnonce]);
        _->
            hexdigest([User , ":" , Realm , ":" , Passwd])
    end,
    %%Calculate H(A2)
    {_, _, _, _, Path, Query} = http_uri:parse(URL),
    HA2 = hexdigest([string:to_upper(atom_to_list(Method)),":",Path++Query]),
    %%Calculate the digest response
    ChallengeResponse = hexdigest([HA1,":",Nonce,":",NonceCount,":",Cnonce,":",Qop,":",HA2]),
    %%Build the Authorization header value
    AuthValue = "Digest username=\"" ++ User ++ "\", realm=\"" ++ Realm ++ "\", nonce=\"" ++ Nonce ++ "\", uri=\"" ++ Path ++ Query ++ "\", cnonce=\"" ++ Cnonce ++ "\", nc=" ++ NonceCount ++ ", qop=\"" ++ Qop ++ "\", response=\"" ++ ChallengeResponse ++ "\", opaque=\"" ++ Opaque ++ "\", algorithm=\"" ++ Algorithm ++ "\"",
    %%Send request again with challenge response included
    [{"Authorization",AuthValue}].
    
hexdigest(String) ->
    lists:flatten(lists:map(fun(V) ->
        string:to_lower(integer_to_hexlist(V)) end,
            binary_to_list(erlang:md5(String))
    )).

integer_to_hexlist(X) when  X < 16 ->
    "0" ++ httpd_util:integer_to_hexlist(X);
integer_to_hexlist(X) ->
    httpd_util:integer_to_hexlist(X).
    
get_cnonce() ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    base64:encode_to_string(generate_cnonce(8)).
    
generate_cnonce({0, Cnonce}) ->
    [random:uniform(255) | Cnonce];
generate_cnonce({Len, Cnonce}) ->
    generate_cnonce({Len-1,[random:uniform(255) | Cnonce]});
generate_cnonce(Len) ->
    generate_cnonce({Len-1,[random:uniform(255)]}).
        
        
    