-module(web_mochiweb).
-compile(export_all).

start(DocRoot)->
	Options1 = [{ip,"127.0.0.1"}, {port,8000}],	
	Loop = fun(Req) ->                  
				   ?MODULE:loop(Req, DocRoot) %%DocRootÎªwwwµÄpath
			end,
	mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).
	
stop() ->
    mochiweb_http:stop(?MODULE).
	
loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    io:format("get path: ~p\n",[Path]), 
	case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "auth" ->  
					Req:respond({401, [{"WWW-Authenticate", "Basic realm=\"validate\""},{"Content-type", "text/html"}], <<"<html><head>
									<title>login</title>
									</head>
									<body>test<body><html>">>}),%401 not Authorized
					H = Req:get(headers),
					{_, {"host", _, {"accept", _, _, {"accept-language", _, _, {"connection", _, {"cache-control", _, {"authorization", {'Authorization',"Basic "++Auth}, _, _}, _}, _}}}, _}} = H,
					AuthStr = binary_to_list(base64:decode(Auth)),
					io:format("Ret: ~p~n", [AuthStr]);
                _ ->
                    Req:serve_file(Path, DocRoot) 
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()   			
            end;
        _ ->
            Req:respond({501, [], []})			
    end.	