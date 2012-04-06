-module(linkfinder, [UrlString, Proxy, ProxyUserName, ProxyPassword, PostData, UserName, Password, Timeout, Pause, MaxLinks, MaxSearchDepth, LogPath, Monitor, ContentMax, SummaryMax]).
-author('yan.zhang@dragonflow.com').
-compile(export_all).
-define(TARGET_TAGS, ["a", "img", "input", "body", "base", "meta", "frame", "iframe", "area"]).
-define(CurrentStatusInfo, currentstatus).

new(A, B, C, D, E, F, G, H, I, J, K, L, M) ->
	case ets:info(?CurrentStatusInfo) of
		undefined ->
			%io:format("create ets: currentstatus~n"),
			ets:new(?CurrentStatusInfo, [named_table]);
		_ ->
			ok
	end,
	ets:insert(?CurrentStatusInfo, {totalBroken, 0}),
	ets:insert(?CurrentStatusInfo, {totalLeft, 0}),
	ets:insert(?CurrentStatusInfo, {totalPages, 0}),
	ets:insert(?CurrentStatusInfo, {totalGraphics, 0}),
	ets:insert(?CurrentStatusInfo, {totalTime, 0}),
	ets:insert(?CurrentStatusInfo, {brokenSummary, ""}),
	ets:insert(?CurrentStatusInfo, {brokenLinks, ""}),
	ets:insert(?CurrentStatusInfo, {currentDepth, 0}),	
	_ContentMax = 50000,
	_SummaryMax = 500,
	{?MODULE,A, B, C, D, E, F, G, H, I, J, K, L, M, _ContentMax, _SummaryMax}.
	

search(Flag) ->
	inets:start(),
	URLInfo = [{url, UrlString},
				{source, "-"},
				{contentType, "unknown"},
				{size, -1},
				{duration, -1},
				{status, -1},
				{count, 0},
				{visited, true},
				{externalLink, false},
				{sortKey, 1}],
	%ets:insert(?CurrentStatusInfo, {remaining, [URLInfo]}), %add start urlinfo, remaining similiar to vector
	ets:insert(?CurrentStatusInfo, {urlstrings, [UrlString]}),
	ets:insert(?CurrentStatusInfo, {UrlString, URLInfo}),
	search(Flag, [{UrlString, false}]).
	
search(_Flag, []) ->
	ok;
search(Flag, Remaining) ->
	[{_, CurrentDepth}] = ets:lookup(?CurrentStatusInfo, currentDepth),	
	case CurrentDepth =< MaxSearchDepth of
		false ->%do nothing
			ok;
		true ->	
			ets:insert(?CurrentStatusInfo, {currentDepth, CurrentDepth+1}),%CurrentDepth++
			%io:format("currentDepth:~p remaining size:~p ~n", [currentDepth, length(Remaining)]),
			searchCurrentDepth(Flag, Remaining),
			
			[{_, T_pages}] = ets:lookup(?CurrentStatusInfo, totalPages),	
			case T_pages < MaxLinks of
				true ->
					%io:format("find New Links ~n"),
					NewLinks = findNewLinks(),
					NewRemaining = getPartInfo(NewLinks, [url, externalLink], []),
					search(Flag, NewRemaining);
				_ ->
					ok
			end
	end.
	
%remaining.elements	
searchCurrentDepth(_Flag, []) ->
	ok;
searchCurrentDepth(Flag, [{Url, ExternalLink}|Remaining]) ->
	[{_, T_pages}] = ets:lookup(?CurrentStatusInfo, totalPages),	
	case T_pages < MaxLinks of
		true ->%do
			findLinks(unescapeHTML(Url), ExternalLink, Flag), %flag tag is extern or no
			searchCurrentDepth(Flag, Remaining);
		_ ->%do nothing
			ok
	end.	
	
	
findNewLinks() ->
	[{_, UrlStringList}] = ets:lookup(?CurrentStatusInfo, urlstrings),
	%io:format(" UrlStringList:~p ~n", [UrlStringList]),	
	getUrlInfoFromETS(UrlStringList, []).

getUrlInfoFromETS([], Acc) ->
	Acc;
getUrlInfoFromETS([Url|Rest], Acc) when is_list(Url) ->	
	%io:format("UrlString:~p UrlInfo:~p~n", [UrlString, ets:lookup(?CurrentStatusInfo, UrlString)]),
	%io:format(" Rest:~p ~n", [Rest]),
	[{_, UrlInfo}] = ets:lookup(?CurrentStatusInfo, Url),
	io:format(" Visited:~p ~n", [proplists:get_value(visited, UrlInfo)]),
	case proplists:get_value(visited, UrlInfo) of
		true ->
			NewAcc = Acc;
		_ ->
			[{_, TotalLeft}] = ets:lookup(?CurrentStatusInfo, totalLeft),
			ets:insert(?CurrentStatusInfo, {totalLeft, TotalLeft+1}),
			
			UrlInfo1 = case getHost(Url) /= string:to_lower(getHost(UrlString)) of
				true ->
					proplists:delete(externalLink, UrlInfo)++[{externalLink, true}];
				_ ->
					UrlInfo
			end,
			UrlInfo2 = proplists:delete(visited, UrlInfo1)++[{visited, true}],
			NewAcc = Acc++[UrlInfo2]
	end,
	getUrlInfoFromETS(Rest, NewAcc).


getPartInfo([], _KeyList, Acc) ->
	Acc;
getPartInfo([H|R], KeyList, Acc) when is_list(H) ->
	NewAcc = case [proplists:get_value(Key, H) || Key <- KeyList] of
		[] ->
			Acc;
		L ->	
			Info = list_to_tuple(L),
			Acc++[Info]
	end,
	getPartInfo(R, KeyList, NewAcc).
	

findLinks(Url, ExternalLink, Flag) ->
	%io:format("findLinks--url:~p~n", [Url]), 
	Time1 = now(),
	case Monitor /= null of
		true ->
			[{_, T_broken}] = ets:lookup(?CurrentStatusInfo, totalBroken),			
			[{_, T_pages}] = ets:lookup(?CurrentStatusInfo, totalPages),
			[{_, T_grap}] = ets:lookup(?CurrentStatusInfo, totalGraphics),
			[{_, T_time}] = ets:lookup(?CurrentStatusInfo, totalTime),
			[{_, T_left}] = ets:lookup(?CurrentStatusInfo, totalLeft),
			[{_, B_summary}] = ets:lookup(?CurrentStatusInfo, brokenSummary),
			[{_, B_links}] = ets:lookup(?CurrentStatusInfo, brokenLinks),
			Monitor:updateProperties(Url, T_broken, T_pages, T_grap, T_time, T_left, B_summary, B_links);%url, totalBroken, totalPages, totalGraphics, totalTime, totalLeft, brokenSummary, brokenLinks
		_ ->
			ok
	end,
	
	Flag1 = (string:right(Url, 4)==".map") orelse (string:right(Url, 4)==".bin") orelse (string:right(Url, 3)==".gz") orelse 
		 (string:right(Url, 4)==".tar") orelse (string:right(Url, 4)==".exe") orelse (string:right(Url, 4)==".mov") orelse
		 (string:right(Url, 4)==".zip") orelse (string:right(Url, 4)==".wav") orelse (string:right(Url, 4)==".hqx"),
	P = getProtocol(Url),
	case (P /= "http")  andalso (P /= "https") of
		true ->
			Flag2 = Flag1,
			A1 = -1,
			A2 = 0,
			A3 = 0,
			Headers = "",
			Body = "";
		false -> %"http(s)"
			case Flag1 of
				true ->
					%do another thing...
					Array = PostData ++ ["Method: HEAD"],					
					[A1_tmp, A2_tmp, A3_tmp, Headers_tmp, Body_tmp] = checkOne(Url, Array),
					case getContentType(Headers_tmp) of
						undefined ->
							Flag2 = Flag1,
							A1 = A1_tmp,
							A2 = A2_tmp,
							A3 = A3_tmp,							
							Headers = Headers_tmp,
							Body = Body_tmp;
						Content ->
							case string:str(Content, "text" ) /= 0 of
								true ->
									%change Flag1  status false
									Flag2 = false,
									%change A1_tmp, A2_tmp,...value
									[A1, A2, A3, Headers, Body] = checkOne(Url, PostData);
								_ ->
									Flag2 = Flag1,
									A1 = A1_tmp,
									A2 = A2_tmp,
									A3 = A3_tmp,									
									Headers = Headers_tmp,
									Body = Body_tmp
							end
					end;
				false ->					
					Flag2 = Flag1,
					[A1, A2, A3, Headers, Body] = checkOne(Url, PostData)%%get new URL and HTML
			end,
			
			sleep(Pause)
	end,
	%ÈÃPostData = null	
	
	case A1 == 204 of
		true ->
			Status = 200;
		_ ->
			Status = A1
	end,
	case Flag2 of
		true -> 
			Duration = -1;
		_ ->
			case A2 ==0 of
				true ->
					Duration = timer:now_diff(now(), Time1);
				_ ->
					Duration = A2
			end
	end,
	_Size = A3,
	ContentType = getContentType(Headers),
	%io:format("findLinks--url:~p ContentType:~p~n", [Url, ContentType]),
	
	case ContentType of		
		"image"++_ ->
			[{_, TG}] = ets:lookup(?CurrentStatusInfo, totalGraphics),
			ets:insert(?CurrentStatusInfo, {totalGraphics, TG+1});
		_ 	->
			ok
	end,
	[{_, TP}] = ets:lookup(?CurrentStatusInfo, totalPages),
	ets:insert(?CurrentStatusInfo, {totalPages, TP+1}),
	[{_, TL}] = ets:lookup(?CurrentStatusInfo, totalLeft),
	ets:insert(?CurrentStatusInfo, {totalLeft, TL-1}),
	
	case (Status /= 200) andalso (Status /= -1) of
		true ->
			[{_, TB}] = ets:lookup(?CurrentStatusInfo, totalBroken),
			ets:insert(?CurrentStatusInfo, {totalBroken, TB+1}),
			[{_, BrokenLinks}] = ets:lookup(?CurrentStatusInfo, brokenLinks),			
			NBrokenLinks = case length(BrokenLinks) > 0 of
				true ->
					BrokenLinks ++ ", " ++ Url;
				_ ->
					Url
			end,
			ets:insert(?CurrentStatusInfo, {brokenLinks, NBrokenLinks}),
			
			[{_, BrokenSummary}] = ets:lookup(?CurrentStatusInfo, brokenSummary),			
			NBrokenSummary = case string:right(BrokenSummary, 3) /= "..." of
				true ->
					case length(BrokenSummary) of
						0 ->
							Url;
						Len ->
							case Len > SummaryMax of
								true ->
									BrokenSummary ++ "...";
								_ ->
									BrokenSummary ++ ", " ++ Url
							end
					end;
				_ ->
					BrokenSummary
			end,
			ets:insert(?CurrentStatusInfo, {brokenSummary, NBrokenSummary});
		false ->
			[{_, TotalTime}] = ets:lookup(?CurrentStatusInfo, totalTime),
			ets:insert(?CurrentStatusInfo, {totalTime, TotalTime + Duration})
	end,
	
	%io:format("findLinks--Status:~p ContentType:~p ExternalLink:~p Flag:~p~n", [Status, ContentType, ExternalLink, Flag]),
	case (Status == 200)  andalso (ContentType == "text/html") andalso ((not ExternalLink) orelse Flag) of
		true ->
			%html content parse
			Data = parseBody(Body),
			
			%get base url
			BaseTagInfo = getcontents(Data, "base", ["href"]),
			case BaseTagInfo of	
				[] ->
					S6 = "";
				_ ->					
					S6 = proplists:get_value("href", BaseTagInfo),
					io:format("LinkFinder base URL: ~p~n", [S6])
			end,
			
			%get body backgroud image
			BodyTagInfo = getcontents(Data, "body", ["background"]),
			case BodyTagInfo of	
				[] ->
					%S7 = [];
					ok;
				_ ->					
					S7 = proplists:get_all_values("background", BodyTagInfo),%addURL(urlinfo1, s7, s6, true)					
					io:format("LinkFinder body tag image URL: ~p~n", [S7]),
					addURL(Url, S7, S6)
			end,
				
			%get meta auto refreshed url
			MetaTagInfo = getcontents(Data, "meta", ["url"]),
			case MetaTagInfo of	
				[] ->
					%S8 = [];
					ok;
				_ ->					
					S8 = proplists:get_all_values("url", MetaTagInfo),%addURL(urlinfo1, s8, s6)
					io:format("LinkFinder meta tag URL: ~p~n", [S8]),
					addURL(Url, S8, S6)
			end,	
			
			%get img src
			ImgTagInfo = getcontents(Data, "img", ["src"]),
			case ImgTagInfo of	
				[] ->
					%S9 = [];
					ok;
				_ ->					
					S9 = proplists:get_all_values("src", ImgTagInfo),%list addURL(urlinfo1, s9, s6, true)
					io:format("LinkFinder img tag src URL: ~p~n", [S9]),
					addURL(Url, S9, S6)
			end,

			%get input image src
			InputTagInfo = getcontents(Data, "input", ["src"]),
			case InputTagInfo of	
				[] ->
					%S10 = [];
					ok;
				_ ->					
					S10 = proplists:get_all_values("src", InputTagInfo),%list addURL(urlinfo1, s10, s6, true)
					io:format("LinkFinder input tag image src URL: ~p~n", [S10]),
					addURL(Url, S10, S6)
			end,

			%get a href
			ATagInfo = getcontents(Data, "a", ["href"]),
			case ATagInfo of	
				[] ->
					%S11 = [];
					ok;
				_ ->					
					S11 = proplists:get_all_values("href", ATagInfo),%list addURL(urlinfo1, s11, s6)
					io:format("LinkFinder A tag href link URL: ~p~n", [S11]),
					addURL(Url, S11, S6)
			end,
			
			%get frame src
			FrameTagInfo = getcontents(Data, "frame", ["src"]),
			case FrameTagInfo of	
				[] ->
					%S12 = [];
					ok;
				_ ->					
					S12 = proplists:get_all_values("src", FrameTagInfo),%list addURL(urlinfo1, s12, s6)
					io:format("LinkFinder frame tag src link URL: ~p~n", [S12]),
					addURL(Url, S12, S6)
			end,
			
			%get iframe src
			IframeTagInfo = getcontents(Data, "iframe", ["src"]),
			case IframeTagInfo of	
				[] ->
					%S13 = [];
					ok;
				_ ->					
					S13 = proplists:get_all_values("src", IframeTagInfo),%list addURL(urlinfo1, s13, s6)
					io:format("LinkFinder iframe tag src link URL: ~p~n", [S13]),
					addURL(Url, S13, S6)
			end,
			
			%get area href
			AreaTagInfo = getcontents(Data, "area", ["href"]),
			case AreaTagInfo of	
				[] ->
					%S14 = [];
					ok;
				_ ->					
					S14 = proplists:get_all_values("href", AreaTagInfo),%list addURL(urlinfo1, s14, s6)
					io:format("LinkFinder area tag href link URL: ~p~n", [S14]),
					addURL(Url, S14, S6)
			end;
		_ ->
			ok
	end.

addURL(_CurURL, [], _BaseURL) ->
	ok;
addURL(CurURL, [NewURL|RestNewURL], BaseURL) ->
	string:strip(NewURL),%Remove leading and trailing spaces NewURL
	case NewURL of
		"#"++_ -> %If NewURL as "#" at the beginning, then give up the URL
			ok;
		_ ->
			case (string:left(NewURL, 11) /= "javascript:") andalso 
				  (string:left(NewURL, 7) /= "mailto:") andalso 
				  (string:left(NewURL, 5) /= "news:") andalso 
				  (string:left(NewURL, 6) /= "ftp://") of
				true -> %Head for the relative path with the path
					Url_tmp = resolveURL(NewURL, CurURL, BaseURL),
					Url = case string:rstr(Url_tmp, "#") of
						0 ->
							Url_tmp;
						I ->
							string:substr(Url_tmp, 1, I-1)
					end;
				_ ->
					Url = NewURL
			end,
			%Url to determine whether the in memory table, if exists, update its count, or add
			case ets:lookup(?CurrentStatusInfo, Url) of
				[{_, Info}] ->
					Count = proplists:get_value(count, Info)+1,
					UrlInfo1 = proplists:delete(count, Info)++[{count, Count}];
				_ ->
					UrlInfo1 = [{url, Url},
								{source, CurURL},
								{contentType, "unknown"},
								{size, -1},
								{duration, -1},
								{status, -1},
								{count, 1},
								{visited, false},
								{externalLink, false},
								{sortKey, 1}],
					[{_, UrlStrings}] = ets:lookup(?CurrentStatusInfo, urlstrings),
					ets:insert(?CurrentStatusInfo, {urlstrings, UrlStrings++[Url]})		
			end,
			%io:format("ets:insert--url:~p UrlInfo:~p~n", [Url, UrlInfo1]),
			ets:insert(?CurrentStatusInfo, {Url, UrlInfo1})
	end,
	addURL(CurURL, RestNewURL, BaseURL).
	

%TARGET_TAGS = {"A", "IMG", "INPUT", "BODY", "BASE", "META", "FRAME", "IFRAME", "AREA"}
parseBody(Body) ->	
	%io:format("parseBody--~n"),
	%First weed out all of the comments
	NewBody = clearNote(Body),	
	[retrieveTag(NewBody, Tag, []) || Tag <- ?TARGET_TAGS].
	

retrieveTag(String, Tag, Acc) ->	
	case string:str(String, "<"++Tag++" ") of
		0 ->
			{Tag, Acc};
		Pos_s ->
			RestStr = string:substr(String, Pos_s),
			TagContent = case string:str(RestStr, ">") of
							0 ->
								NewString = string:substr(String, 1, Pos_s-1),
								RestStr;
							Pos_e ->
								NewString = string:substr(String, 1, Pos_s-1)++string:substr(RestStr, Pos_e+1),
								string:substr(RestStr, 1, Pos_e)
						end,
			retrieveTag(NewString, Tag, Acc++[TagContent])			
	end.
	

clearNote(Body) ->
	%T1 = now(),
	case string:str(Body, "<!--") of
		0 ->
			string:to_lower(Body);
		Pos_s ->
			NewBody = case string:str(Body, "-->") of
						0 ->
							string:substr(Body, 1, Pos_s-1);
						Pos_e ->
							string:substr(Body, 1, Pos_s-1)++string:substr(Body, Pos_e+3)
					end,
			clearNote(NewBody)
	end.
	%timer:now_diff(now(), T1).
	
%"<meta http-equiv='refresh' content='0;url=/web/login' />"	
%"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=gb2312\" >"
%return:[{Attribute, Value}] | []
parseAttribute(String, Tag) ->		
	case string:left(String, length(Tag)+1) == "<"++Tag of
		true ->
			Str = string:substr(String, length(Tag)+2),
			readAttribute(Str, Str, 1, 1, []);
		false ->
			[]
	end.			

readAttribute(_C, [], _I, _J, A) ->
	A;
readAttribute(C, [H|S], I, J, A) ->
	case H of
		$\s ->
			readAttribute(C, S, I+1, J+1, A);
		$= ->
			case J-I > 0 of
				true ->
					Prop = string:substr(C, I, J-I),
					Str1 = string:substr(C, J+1),
					[Quote|Str2] = Str1,
					{V1, V2} = case (Quote==$') or (Quote==$") of
									true ->								
										readValue(Str2, Str2, 1, Quote);
									_ ->
										readValue(Str1, Str1, 1, $\s)
								end,
					readAttribute(V2, V2, 1, 1, A++[{Prop, V1}]);
				_ ->
					readAttribute(C, S, I+1, J+1, A)
			end;		
		_ ->
			readAttribute(C, S, I, J+1, A)
	end.		

readValue(_Str1, [], _J, _TermChar) ->
	{"", ""};
readValue(Str1, [H|S], J, TermChar) ->
	case H  of
		TermChar ->
			Value = string:substr(Str1, 1, J-1),
			Rest = string:substr(Str1, J+1),
			{Value, Rest};		
		$> ->
			case J > 1 of
				true ->
					Value = string:substr(Str1, 1, J-1),
					Value1 = case string:right(Value, 1) == "/" of
								true ->
									string:substr(Value, 1, length(Value)-1);
								_ ->
									Value
							end,
					{Value1, ""};
				_ ->
					{"", ""}
			end;
		_ ->
			readValue(Str1, S, J+1, TermChar)
	end.

%return:[StatusCode, Duration, length(Body), Head, Body]
checkOne(Url, Post) ->	
	HOpts = case ProxyUserName /= "" of
				true ->
					[{timeout, Timeout}, {proxy_auth, {ProxyUserName, ProxyPassword}}];
				_ ->
					[{timeout, Timeout}]
			end,
	checkURL(Url, [], HOpts, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% URLMonitor API %%%%%%%%
%Remove the String in the ".." (Url of the string ".." said the parent directory)
stripDotDot(String) ->
	I = string:rstr(String, "?"),
	J = string:str(String, ".."),
	case (J == 0) orelse ((I > 0) andalso (J > I)) of
		true ->
			String;
		_ ->
			%String to find the ".." before the last "/" Index	
			case string:rstr(string:substr(String, 1, J-2), "/") of
				0 ->
					String;
				K ->
					NewString = case string:right(string:substr(String, 1, K-1), 2) == ":/" of
						true ->
							string:substr(String, 1, J-1) ++ string:substr(String, J+3);
						_ ->
							string:substr(String, 1, K-1) ++ string:substr(String, J+2)
					end,
					stripDotDot(NewString)
			end			
	end.	

%String remove the ". /" (Url of string. "/" Indicates the current directory)
stripDotSlash(String) ->	
	case string:str(String, "//") of
		0 ->
			String;
		I ->
			case string:str(string:substr(String, I+2), "/") of
				0 ->
					String;
				J ->
					stripDotSlash(String, J+I+1)
			end
	end.

stripDotSlash(String, J) ->
	K = string:rstr(String, "?"),
	L = case string:str(string:substr(String, J+1), "./") of
		0 ->
			0;
		I ->
			I+J
	end,
	case (L > 0) andalso ((K == 0) orelse (L =< k)) of
		true ->
			NewString = string:substr(String, 1, L-1) ++ string:substr(String, L+2),
			stripDotSlash(NewString, J);
		_ ->
			String
	end.
	
resolveURL(NewURL, CurURL, BaseURL) ->
	%NewURL1 = removeChars(NewURL, "\n\r"),
	case getPort(CurURL) of
		"" ->
			CurPort = "";
		P ->
			CurPort = ":"++P
	end,
	
	case string:chr(NewURL, $:) of
		0 ->%NNewURL the URL for the relative
			case string:left(NewURL, 2) == "//" of
				true ->%Added before the NewURL protocol headers such as "http:"
					Flag = false,
					NewURL1 = getProtocol(CurURL) ++ ":" ++ NewURL;
				_ ->
					Flag = true,
					NewURL1 = NewURL
			end;
		I ->
			Str = string:substr(NewURL, 1, I-1),%Take ":" before the string
			F = not (onlyChars(NewURL, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")),
			case (Str == "http") orelse (Str == "https") orelse (string:left(NewURL, length(Str)+3) == Str++"://") of
				true ->
					Flag = true;
				_ ->
					Flag = F
			end,
			NewURL1 = NewURL
	end,
	
	case Flag of
		false ->
			NewURL3 = NewURL1;
		true ->%That NewURL absolute URL to "http" or "https" or "XXX: / /" at the beginning, or NewURL relative URL to a "/" at the beginning or not to "/" at the beginning
			NewURL2 = case (string:left(NewURL1, 5) == "http:") orelse (string:left(NewURL1, 6) == "https:") of
				true ->
					Index = string:str(NewURL1, ":"),
					string:substr(NewURL1, Index+1);
				_ ->
					NewURL1
			end,
			%To NewURL2 with header, host and port information
			case string:left(NewURL2, 1) == "/" of
				true ->%NewURL2 with "/" at the beginning
					case BaseURL of
						"" ->
							NewURL3 = getProtocol(CurURL) ++ "://" ++ getHost(CurURL) ++ CurPort ++ NewURL2;
						_ ->
							%BaseURL end with "/"
							BaseURL1 = case string:right(BaseURL, 1) /= "/" of
								true ->
									BaseURL++"/";
								_ ->
									BaseURL
							end,
							case getPort(BaseURL1) of
								"" ->
									BasePort = "";
								P_b ->
									BasePort = ":" ++ P_b
							end,
							NewURL3 = getProtocol(BaseURL1) ++ "://" ++ getHost(BaseURL1) ++ BasePort ++ NewURL2							
					end;
				_ ->%NewURL2 not to "/" at the beginning
					case BaseURL of
						"" ->
							S1 = getFile(CurURL),
							J = string:rstr(S1, "/"),
							L = string:str(S1, "?"),
							K = case (L > 0) andalso (J > L) of
								true ->
									string:rstr(string:substr(S1, 1, L), "/");
								_ ->
									J
							end,
							S2 = case (K > 0) andalso (K < length(S1)-1) of
								true ->
									string:substr(S1, 1, K);
								_ ->
									S1
							end,
							S3 = case string:right(S2, 1) /= "/" of
								true ->
									S2++"/";
								_ ->
									S2
							end,								
							NewURL3 = getProtocol(CurURL) ++ "://" ++ getHost(CurURL) ++ CurPort ++ S3 ++ NewURL2;
						_ ->
							%BaseURL end with "/"
							BaseURL1 = case string:right(BaseURL, 1) /= "/" of
								true ->
									BaseURL++"/";
								_ ->
									BaseURL
							end,							
							NewURL3 = BaseURL1 ++ NewURL2							
					end
			end
	end,
	NewURL4 = stripDotDot(NewURL3),
	NewURL5 = stripDotSlash(NewURL4),
	NewURL6 = removeChars(NewURL5, "\n\r"),
	%NewURL7 = string:strip(NewURL6),
	case http_uri:parse(NewURL6) of
		{error, _} ->
			io:format("The url is not complete for the redirect: ~s~n", [NewURL6]),%throw exception
			NewURL6;
		_ ->
			%If NewURL6 Port information is not included, then fill in the Port Port of CurURL		
			case (length(CurPort) > 0) andalso (getPort(NewURL6) == "") of
				true ->
					Host = getHost(NewURL6),
					J1 = string:str(NewURL6, Host) + length(Host),
					S6 = string:substr(NewURL6, 1, J1-1),
					S7 = string:substr(NewURL6, J1),
					S6 ++ CurPort ++ S7;
				_ ->
					NewURL6
			end
	end.	

checkURL(Url, Headers, HTTPOptions, Post) ->
	T1 = now(),
	case Post /= "" of
		true ->
			Method = post,
			Request = {Url, Headers, "text/html", Post};%Post=string()
		_ ->
			Method = get,
			Request = {Url, Headers}
	end,		
	case http:request(Method, Request, HTTPOptions, []) of
		{ok, {{_HttpVersion, StatusCode, _ReasonPhrase}, Head, Body}} ->
			case StatusCode of
				401 ->
					%UserName, Password
					Hrds = [{"Authorization", ""}],%auth header
					checkURL(Url, Hrds, HTTPOptions, Post);
				_ ->					
					Duration = timer:now_diff(now(), T1),%microseconds					
					%NewUrls = "",%meta refresh url
					NBody = case length(Body) > ContentMax of
						true ->%truncate ...
							string:substr(Body, 1, ContentMax);
						_ ->
							Body
					end,
					[StatusCode, Duration, length(Body), Head, NBody]
			end;
		{error, _Reason}	 ->%term
			[-1, 0, 0, "", ""]
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% URLInfo API %%%%%%%%%%%%
%%get  Url   protocol info	
getProtocol(Url) ->
	case string:chr(Url, $:) of
		0 ->
			S = "";
		I ->
			S = string:substr(Url, 1, I-1)
	end,
	string:to_lower(S).	

%%To obtain the Url of the host information (which may include the port number)
getRawHost(Url) ->
	case string:str(Url, "://") of
		0 ->			
			"";			
		I ->
			S1 = string:substr(Url, I+3),
			case string:str(S1, "/") of
				0 ->
					case string:str(S1, "?") of
						0 ->
							S1;
						K ->
							string:substr(S1, 1, K-1)
					end;
				J ->
					string:substr(S1, 1, J-1)
			end
	end.

%%To obtain the Url of the host information does not include port number
getHost(Url) ->
	RawHost = getRawHost(Url),
	case string:str(RawHost, ":") of
		0 ->
			RawHost;
		I ->
			string:substr(RawHost, 1, I-1)
	end.
	
%%The port information obtained Url
getPort(Url) ->
	RawHost = getRawHost(Url),
	case string:str(RawHost, ":") of
		0 ->
			"";
		I ->
			string:substr(RawHost, I+1)
	end.

%%return  Integer
getConnectPort(Url) ->
	case getPort(Url) of
		"" ->
			case getProtocol(Url) == "https" of
				true ->
					443;
				_ ->
					80
			end;
		P ->
			list_to_integer(P)
	end.

%%To obtain the Url of the file and directory information
getFile(Url) ->
	case string:str(Url, ":") of
		0 ->
			Url;
		I ->
			case string:substr(Url, I+1, 2) == "//" of
				true ->
					J = 3;
				_ ->
					J = 1
			end,
			S = string:substr(Url, I+J),
			case J==3 of
				false ->
					S;
				_ ->
					case string:str(S, "/") of
						0 ->
							"/";
						K ->
							string:substr(S, K)
					end
			end
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% TextUtils API %%%%%%
%2 contains the parameters of the characters deleted from argument 1
removeChars(Source, []) ->
	Source;
removeChars(Source, [H|R]) ->
	Ret = lists:filter(fun(Char) ->Char /= H end, Source),
	removeChars(Ret, R).	
	
%Determine the parameters of a character is contained in all the characters in the parameter 2, if all, it returns true, otherwise returns false
onlyChars("", _Str2) ->
	true;
onlyChars([H|Str], Str2) ->
	case lists:member(H, Str2) of
		true ->
			onlyChars(Str, Str2);
		_ ->
			false
	end.

%html special character conversion
unescapeHTML(String) ->%String all lowercase
	%Str1 = replaceString(String, "&amp;", "&"),
	%replaceString(Str1, "&AMP;", "&").
	replaceString(String, "&amp;", "&").
	
%Str contains Str1 the substring with the replacement Str2	
replaceString(Str, Str1, Str2) ->
	case Str1 /= Str2 of
		true ->
			replaceStringLoop(Str, Str1, Str2);
		_ ->
			Str
	end.

replaceStringLoop(Str, Str1, Str2) ->
	case string:str(Str, Str1) of
		0 ->
			Str;
		Index ->
			NewStr = string:substr(Str, 1, Index-1) ++ Str2 ++ string:substr(Str, Index+length(Str1)),
			replaceStringLoop(NewStr, Str1, Str2)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getcontents(Data, Tag, Attributes) ->
	Contents = proplists:get_value(Tag, Data),	
	lists:flatten([getvalue(Content, Attributes, []) || Content <- Contents]).

%"<meta http-equiv='refresh' content='0; url=/web/login' />"
%Acc:[{Attr1,Value1},{Attr2,Value2},...]
getvalue(_Content, [], Acc) ->
	Acc;
getvalue(Content, [Attr|RestAttr], Acc) ->
	case string:str(Content, Attr++"=") of
		0 ->
			getvalue(Content, RestAttr, Acc);
		I ->
			Str1 = string:substr(Content, I+length(Attr)+1),
			[Quote|Str2] = Str1,
			Value = case (Quote==$') or (Quote==$") of
						true ->					
							substr(Str2, Quote);
						_ ->
							S = substr(Str1, $\s),
							case (string:right(S, 1) == "'") orelse (string:right(S, 1) == "\"") of
								true ->
									string:substr(S, 1, length(S)-1);
								_ ->
									S
							end
					end,			
			getvalue(Content, RestAttr, Acc++[{Attr, Value}])
	end.

%Take string String in the string: that is, all the characters before the character Quote
substr(String, Quote) ->
	case string:chr(String, Quote) of
		0 ->
			"";
		J ->
			string:substr(String, 1, J-1)
	end.

%return:string() | undefined
getContentType(Headers) ->
	proplists:get_value("content-type", Headers).
	
sleep(T) ->
	receive
	after T -> true
	end.	
	
getTotalBroken() ->	
	[{_, TotalBroken}] = ets:lookup(?CurrentStatusInfo, totalBroken),
	TotalBroken.
	
getTotalPages() ->	
	[{_, TotalPages}] = ets:lookup(?CurrentStatusInfo, totalPages),
	TotalPages.
	
getTotalGraphics() ->	
	[{_, TotalGraphics}] = ets:lookup(?CurrentStatusInfo, totalGraphics),
	TotalGraphics.

%microseconds
getTotalTime() ->	
	[{_, TotalTime}] = ets:lookup(?CurrentStatusInfo, totalTime),
	TotalTime.
	
getBrokenSummary() ->	
	[{_, BrokenSummary}] = ets:lookup(?CurrentStatusInfo, brokenSummary),
	BrokenSummary.
getBrokenLinks() ->	
	[{_, BrokenLinks}] = ets:lookup(?CurrentStatusInfo, brokenLinks),
	BrokenLinks.