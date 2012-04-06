-module(egd_chart).
-compile(export_all).

-export([
	graph/1, 
	graph/2, 
	bar2d/1,
	bar2d/2
	]).

-export([
	init/0,
	gbkToUnicode/1,		 
	hsl2rgb/1, 
	rgb2hsl/1
	]).

-export([smart_ticksize/3]).

-record(chart, {
	type = png,
	render_engine = opaque,
	margin = 30,			% margin
	bbx = {{30,30}, {130,130}},	% Graph boundingbox (internal)
	ibbx = undefined,
	ranges = {{0,0}, {100,100}},    % Data boundingbox
	width = 160,			% Total chart width
	height = 160,			% Total chart height
	dxdy = {1.0,1.0},
	ticksize = {10,10},
	precision = {2,2},

	% colors
	bg_rgba     = {255, 255, 255, 255},
	margin_rgba = {255, 255, 255, 255},
	graph_rgba  = [],		% ordered color convention , non-implemented

	% graph specific
	x_label = "X",
	y_label = "Y",
	graph_name_yo = 10,		% Name offset from top
	graph_name_yh = 10,		% Name drop offset
	graph_name_xo = 10,		% Name offset from RHS

	% bar2d specific
	bar_width = 40,
	column_width = 40,
	
	% datetime will translate {{2009,1,29},{10,20,57}} to a number between 1 to 10
	% number is a absolute x-axis position
	% topN_text for topN report
	x_data_type = datetime,	% or number, topN_text
	
	% day       : 10:23  , 22:42
	% week      : Monday , Tuesday
	% month     : 12     , 29
	% undefined : 2009-10-9 17:12:10
	contrast_type = undefined, 
	
	week_texts = [],
			
	trends_line = undefined,   
	threshold = undefined,	
	bar_color_changing = undefined, % or over_threshold
	data_count = 0,
	title = []			   		   
			   
	}).

-define(float_error, 0.0000000000000001).
-define(title_height, 45).


get_uni_list(L)->
	get_uni_list(L,[]).

get_uni_list([],U) -> 
	lists:reverse(U);
get_uni_list([A,B|Z],U) -> 
   	H = B*256 + A,
   	get_uni_list(Z,[H|U]).


listTextGbkToUnicode([])->
	[];
listTextGbkToUnicode(Texts)->
	lists:map(
		fun	([])-> [];
			(E)-> case catch gbkToUnicode( string(E,2) ) of
					{'EXIT', _} -> [];
					C -> C
				  end
		end, Texts).

gbkToUnicode(T1, T2)->
	L1= gbkToUnicode(T1), 
	L2= gbkToUnicode(T2),
	[L1,L2].

gbkToUnicode([])->
	[];
gbkToUnicode( Text )->
    L= iconv:convert("utf-8", "UCS-2-INTERNAL", Text),
    case platform:getOs() of
        1 ->
            get_uni_list(L);
        _ ->
            L
    end.

init()->
	loadFont(),
	loadTitleFont(),
	loadExampleFont(),
	ok.

loadFont()->
	egd_font:load("6x11_latin1.wingsfont").	

loadTitleFont()->
	egd_font:load("big15.wingsfont").	

loadExampleFont()->
	egd_font:load("big11.wingsfont").

%% color conversions
%% H, hue has the range of [0, 360]
%% S, saturation has the range of [0,1]
%% L, lightness has the range of [0,1]

hsl2rgb({H,S,L}) -> hsl2rgb({H,S,L,255});
hsl2rgb({H,S,L,A}) ->
    Q  = if
	L < 0.5 -> L * (1 + S);
	true    -> L + S - (L * S)
    end,
    P  = 2 * L - Q,
    Hk = H/360,
    Rt = Hk + 1/3,
    Gt = Hk,
    Bt = Hk - 1/3,

    Cts = lists:map(fun
	(Tc) when Tc < 0.0 -> Tc + 1.0;
	(Tc) when Tc > 1.0 -> Tc - 1.0;
	(Tc) ->  Tc
    end, [Rt, Gt, Bt]),
    [R,G,B] = lists:map(fun
	(Tc) when Tc < 1/6 -> P + ((Q - P) * 6 * Tc);
	(Tc) when Tc < 1/2, Tc >= 1/6 -> Q;
	(Tc) when Tc < 2/3, Tc >= 1/2 -> P + ((Q - P) * 6 * (2/3 - Tc));
	(_ ) -> P
    end, Cts),
    {trunc(R*255),trunc(G*255),trunc(B*255),A}.

rgb2hsl({R,G,B}) -> rgb2hsl({R,G,B,255});
rgb2hsl({R,G,B,A}) ->
    Rf  = R/255,
    Gf  = G/255,
    Bf  = B/255,
    Max = lists:max([Rf,Gf,Bf]),
    Min = lists:min([Rf,Gf,Bf]),
    H   = if
	    abs(Max - Min) < ?float_error ->
		0;
	    abs(Max - Rf)  < ?float_error ->
		D  = 60 * (Gf - Bf)/(Max - Min),
		Dt = trunc(D),
	        Dt rem 360;
	    abs(Max - Gf) < ?float_error ->
		60 * (Bf - Rf)/(Max - Min) + 120;
	    abs(Max - Bf) < ?float_error ->
		60 * (Rf - Gf)/(Max - Min) + 240;
	    true -> 
	    0
	end,
    L   = (Max + Min)/2,
    S   = if
	    abs(Max - Min) < ?float_error ->
		0;
	    L > 0.5 ->
		(Max - Min)/(2 - (Max + Min));
	    true ->
		(Max - Min)/(Max + Min)
	end,
    {H, S, L, A}.

%% graph/1 and graph/2
%% In:
%%	Data :: [{Graphname :: atom() | string(), [{X,Y}]}]
%%	Options :: [
%%		% Metric options
%%		{ width,    integer() }, (300)
%%		{ height,   integer() }, (300)
%%		{ margin,   integer() }, (30)
%%		{ ticksize, { integer(), integer() },
%%		{ x_range,  { float(), float() },
%%		{ y_range,  { float(), float() },
%%		
%%		% Naming options
%%		{ x_label,  string() | atom() },
%%		{ y_label,  string() | atom() },
%%
%%		% Color options
%%		{bg_rgba,    {byte(), byte(), byte()}}		
%%	]
graph(Data) -> graph(Data, [{width, 300}, {height, 300}]).

graph(Data0, Options) ->
	XDataType= proplists:get_value(x_data_type, Options, datetime),
	ContrastType= proplists:get_value(contrast_type, Options, undefined),
	{ ZeroDate, Scale, Data }= prepare_contrast_data(Options, Data0, ContrastType, XDataType),
	
    TempChart = graph_chart(Options,Data, ZeroDate, Scale),
	Chart= TempChart#chart{data_count= length(Data)},
    Im = egd:create(Chart#chart.width, Chart#chart.height),
    LightBlue = egd:color(Chart#chart.bg_rgba),
    {Pt1, Pt2} = Chart#chart.bbx,
    egd:filledRectangle(Im, Pt1, Pt2, LightBlue), % background

    % Fonts? Check for text enabling
	Font = case Chart#chart.contrast_type=:=week andalso Chart#chart.week_texts=/=[] of
			true -> loadExampleFont();
			_ -> loadFont()
		  end,
 
    TenXData0= draw_graphs(Data, Chart, Im),
	TenXData= case Chart#chart.contrast_type =/= undefined of
				  true-> setTenXLocationForContrast(ZeroDate, Scale);
				  _->TenXData0
			  end,
	draw_perf_threshold(Im, Chart),
	
    % estetic crop, necessary?
    {{X0,Y0}, {X1,Y1}} = Chart#chart.bbx,
    W = Chart#chart.width,
    H = Chart#chart.height,
    White = egd:color(Chart#chart.margin_rgba),
    egd:filledRectangle(Im, {0,0}, {X0-1,H}, White),
    egd:filledRectangle(Im, {X1+1,0}, {W,H}, White),
    egd:filledRectangle(Im, {0,0}, {W,Y0-1}, White),
    egd:filledRectangle(Im, {0,Y1+1}, {W,H}, White),

%% 	egd:text(Im, {500,0}, Font, "Title 123456789", egd:color({0,0,0})),
    draw_ticks(Chart, Im, Font, TenXData),
    draw_origo_lines(Chart, Im), 	% draw origo crosshair
	draw_graphs(Data, Chart, Im),

	%%     draw_graph_names(Data, Chart, Font, Im),
	GraphC	 = proplists:get_value(graph_rgba, Options, []),
    ColorMap = plot2d_convert_data(Chart, GraphC, Data),	
	draw_bar2d_set_colormap(Im, Chart, loadExampleFont(), ColorMap),
	draw_perf_titile(Im, Chart),
	
%%     draw_xlabel(Chart, Im, Font),
%%     draw_ylabel(Chart, Im, Font),

    Png = egd:render(Im, Chart#chart.type),
    egd:destroy(Im),
    try erlang:exit(Im, normal) catch _:_ -> ok end,
    Png.

% [{Dataset, [{Key, Value}]}] -> [{Key, [{Dataset, Value}]}]
plot2d_convert_data(Chart, GraphC, Data) -> plot2d_convert_data(Chart, GraphC, Data, 0, []).
plot2d_convert_data(Chart, GraphC, [], _, ColorMap ) -> lists:reverse(ColorMap);
plot2d_convert_data(Chart, GraphC, [{Set, KVs}|Data], ColorIndex, ColorMap) ->
    Color = color_scheme(ColorIndex, GraphC, plot2d, Chart#chart.data_count),
    plot2d_convert_data(Chart, GraphC, Data, ColorIndex + 1, [{Set,Color}|ColorMap]).


graph_chart(Opts, Data, ZeroDate, Scale) ->

	{{T_X0,T_Y0},{T_X1,T_Y1}}= proplists:get_value(ranges, Opts, ranges(Data)),
    {{X0,Y0},{X1,Y1}} = case ZeroDate=:=0 andalso Scale=:=0 of
							true->{{T_X0,T_Y0},{T_X1,T_Y1}};
							_->   {{ZeroDate,T_Y0},{ZeroDate+Scale,T_Y1}}
						end,	
    Type      = proplists:get_value(type,         Opts, png),
    Width     = proplists:get_value(width,        Opts, 600),
    Height    = proplists:get_value(height,       Opts, 600),
    Xlabel    = proplists:get_value(x_label,      Opts, "X"),
    Ylabel    = proplists:get_value(y_label,      Opts, "Y"),
    XrangeMax = proplists:get_value(x_range_max,  Opts, X1),
    XrangeMin = proplists:get_value(x_range_min,  Opts, X0),
    YrangeMax = proplists:get_value(y_range_max,  Opts, Y1),
    YrangeMin = proplists:get_value(y_range_min,  Opts, Y0),
    Ranges    = {{XrangeMin, YrangeMin}, {XrangeMax,YrangeMax}},
    Precision = precision_level(Ranges, 10),
    {TsX,TsY} = smart_ticksize(Ranges, 10),
    XTicksize = proplists:get_value(x_ticksize,   Opts, TsX),
    YTicksize = proplists:get_value(y_ticksize,   Opts, TsY),
    Ticksize  = proplists:get_value(ticksize,     Opts, {XTicksize, YTicksize}),
    Margin    = proplists:get_value(margin,       Opts, 30),
    BGC       = proplists:get_value(bg_rgba,      Opts, {255, 255, 255, 255}),
	GraphC	  = proplists:get_value(graph_rgba,   Opts, []),
	XDataType = proplists:get_value(x_data_type,  Opts, datetime),	
	ContrastType= proplists:get_value(contrast_type,Opts, undefined),
	WeekText  = proplists:get_value(week_texts,   Opts, []),	
	Threshold = proplists:get_value(threshold,    Opts, undefined),
	TrendsLine= proplists:get_value(trends_line,  Opts, undefined),
	Title1	  = proplists:get_value(title1,    	  Opts, []),
	Title2	  = proplists:get_value(title2,    	  Opts, []),	
	
    BBX       = {{Margin, Margin+?title_height}, {Width - Margin, Height - Margin}},
    DxDy      = update_dxdy(Ranges,BBX),
    
    
    #chart{
	type      = Type,
	width     = Width,
	height    = Height,
	graph_rgba= GraphC,
	x_label   = Xlabel,
	y_label   = Ylabel,
	ranges    = Ranges,
	precision = Precision,
	ticksize  = Ticksize,
	margin    = Margin,
	bbx       = BBX,
	dxdy      = DxDy,
	bg_rgba   = BGC,
	x_data_type=XDataType, 	 
	contrast_type=ContrastType,
	week_texts= listTextGbkToUnicode(WeekText),
	threshold = Threshold,
	trends_line=TrendsLine,
	title	  = gbkToUnicode(Title1,Title2)
    }.

draw_ylabel(Chart, Im, Font) ->
    Label = string(Chart#chart.y_label, 2),
    N = length(Label),
    {Fw,_Fh} = egd_font:size(Font),
    Width = N*Fw,
    {{Xbbx,Ybbx}, {_,_}} = Chart#chart.bbx,
    Pt = {Xbbx - trunc(Width/2), Ybbx - 20},
    egd:text(Im, Pt, Font, Label, egd:color({0,0,0})).

draw_xlabel(Chart, Im, Font) ->
    Label = string(Chart#chart.x_label, 2),
    N = length(Label),
    {Fw,_Fh} = egd_font:size(Font),
    Width = N*Fw,
    {{Xbbxl,_}, {Xbbxr,Ybbx}} = Chart#chart.bbx,
    Xc = trunc((Xbbxr - Xbbxl)/2) + Chart#chart.margin,
    Y  = Ybbx + 20,
    Pt = {Xc - trunc(Width/2), Y},
    egd:text(Im, Pt, Font, Label, egd:color({0,0,0})).


%% not implement self-defined color: GraphC
color_scheme(Index, GraphC, Type, DataCount)->
	color_scheme(Index, GraphC, Type, DataCount, false).
  
color_scheme(Index, GraphC, Type, DataCount, IsThin) when GraphC =/= [] orelse DataCount<14 ->
	L= case IsThin of
		false -> 0.6;
		middle -> 0.81;
		_ -> 0.88
	end,
    egd:color(hsl2rgb({Index*55 rem 360, 0.7, L, 120})); 
color_scheme(Index, GraphC, Type, DataCount, IsThin) ->
	I = case Type of
			plot2d -> round(Index+20);
			_ -> Index
		end,
	Plus= case I rem 10 of
			  8 -> 0.1;
			  9 -> -0.1;
		  	  _ -> 0.3
	end,
	TmepL = 0.5+((I+1) rem 4)/10, 
	L= case IsThin of
		false -> TmepL;
		middle -> case TmepL >= 0.7 of
				 true -> 0.9;
				 _ -> 0.81
			end;			   
		_ -> case TmepL >= 0.7 of
				 true -> 0.93;
				 _ -> 0.88
			end		 
	end,	
    egd:color(hsl2rgb({ trunc(I*15.3) rem 360, 0.5+Plus, L, 120})).  % + (I rem 4)/10


draw_graphs(Datas, Chart, Im) ->
    draw_graphs(Datas, 0, Chart, Im, []).
draw_graphs([],_,_,_, TenXLocation) -> TenXLocation;
draw_graphs([{_, Data}|Datas], ColorIndex, Chart, Im, TenXLocation) ->
    Color = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count),
	ThinColor = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count, true),
	MiddleThinColor = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count, middle),
	{{Xmin,_}, {Xmax,_}} = Chart#chart.ranges,
	WillTrends= ColorIndex=:=0 andalso Chart#chart.trends_line =:= true, 
	WillGetTenXL=  TenXLocation=:=[],
    {NewTenXL,TrendsData}= draw_graph(Data, Chart, Color, {MiddleThinColor,ThinColor}, Im, TenXLocation, WillGetTenXL, (Xmax-Xmin)/9.5 + ColorIndex*Xmax , WillTrends, {0,0,0,0,0}),
	draw_trends_line(Im, Chart, 10, TrendsData, WillTrends),
	case WillTrends of
		true->
			draw_graph(Data, Chart, Color, {MiddleThinColor,ThinColor}, Im, TenXLocation, WillGetTenXL, (Xmax-Xmin)/9.5 + ColorIndex*Xmax , WillTrends, {0,0,0,0,0});
		_->
			ok
	end,
    draw_graphs(Datas, ColorIndex + 1, Chart, Im, NewTenXL).
   
draw_graph([], _, _,_,_, TenXLocation, WillGetTenXL, Xgap, WillTrends, TrendsData) -> {TenXLocation, TrendsData};
draw_graph([E1,E2|Data], Chart, Color, ThinColor, Im, TenXLocation, WillGetTenXL, Xgap, WillTrends, TrendsData) ->
    P1 = xy2chart(E1, Chart),
    P2 = xy2chart(E2, Chart),
    %% ÅÐ¶ÏÊÇ·ñÒç³ö×Ý×ø±ê
    {{X0,Y0}, {X1,Y1}} = Chart#chart.bbx,
    {X, Y} = P1,
    YValue = 
    if
        Y < Y0 ->
            Y0;
        true ->
            Y
    end,
    P11 = {X, YValue},
    {X2, Y2} = P2,
    YValue2 = 
    if
        Y2 < Y0 ->
            Y0;
        true ->
            Y2
    end,
    P22 = {X2, YValue2},
    %%
    {TenXL, NewTrendsData}= draw_graph_dot(P1, Color, Im, TenXLocation, Xgap, E1, Chart#chart.x_data_type, WillTrends, TrendsData),
	NewTenXL= case Chart#chart.x_data_type =/= datetime of
		true -> 
			[];
		_ ->
			if WillGetTenXL andalso Data=:=[] ->
					[Last|T]= TenXL,
					{XData,_}= E2,
					[XData|T];
			   true ->
				   	TenXL
			end
	end,	
    egd:thickline(Im, P11, P22, Color, ThinColor),
    draw_graph([E2|Data], Chart, Color, ThinColor, Im, NewTenXL, WillGetTenXL, Xgap, WillTrends, NewTrendsData);
draw_graph([E|Data], Chart, Color, ThinColor, Im, TenXLocation, WillGetTenXL, Xgap, WillTrends, TrendsData) ->
    Pt = xy2chart(E, Chart),
    %% ÅÐ¶ÏÊÇ·ñÒç³ö×Ý×ø±ê
    %%{{X0,Y0}, {X1,Y1}} = Chart#chart.bbx,
    %%{X, Y} = Pt,
    %%YValue = 
    %%if
    %%    Y < Y0 ->
    %%        Y0;
    %%    true ->
    %%        Y
    %%end,
    %%P11 = {X, YValue},
    %%
	{NewTenXL,NewTrendsData}= draw_graph_dot(Pt, Color, Im, TenXLocation, Xgap, E, Chart#chart.x_data_type, WillTrends, TrendsData),
    draw_graph(Data, Chart, Color, ThinColor, Im, NewTenXL, WillGetTenXL, Xgap, WillTrends, NewTrendsData).

draw_graph_dot({X,Y}, Color, Im, TenXLocation, Xgap, {XData,YData}, XDataType, WillTrends, TrendsData) ->
	Trends_data= cal_trends_data({XData,YData}, XDataType, WillTrends, TrendsData),
    egd:filledEllipse(Im, {X - 2, Y - 2}, {X + 2, Y + 2}, Color),
	{case XDataType =/= datetime of
		true -> 
			[];
		_ ->	
			case TenXLocation of 
				[] ->
					[XData];
				[H|T] ->
					case abs(XData - H) >= Xgap of 
						true -> [XData|TenXLocation];
						_ -> TenXLocation
					end
			end
	end,Trends_data}.


draw_trends_line(Im, Chart, ColorIndex, {Exy,Ex,Ey,Ex_square,N}, true)->
	catch begin
    Color = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count),
	ThinColor = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count, true),
	MiddleThinColor = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count, middle),	
	A= (N*Exy-Ex*Ey)/(N*Ex_square-Ex*Ex),
	B= (Ey-A*Ex)/N,
	{{Xmin, YMin}, {Xmax, YMax}}= Chart#chart.ranges,
	{{X1,Y1},{X2,Y2}}= get_proper_trends_line({{Xmin, YMin}, {Xmax, YMax}},A,B),
    P1 = xy2chart({X1,Y1}, Chart),
    P2 = xy2chart({X2,Y2}, Chart),
    egd:thickline(Im, P1, P2, Color, {MiddleThinColor,ThinColor}),
	ok
	end;
draw_trends_line(Im, Chart, _, _, _)->
	ok.


get_proper_trends_line({{Xmin, YMin}, {Xmax, YMax}},A,B)->
	Y1= A*Xmin+B,
	Y2= A*Xmax+B,
	NY1= case Y1>YMax of
		true -> YMax;
		_ ->
			case Y1<YMin of
				true-> YMin;
				_-> Y1
			end
		 end,
	NY2= case Y2>YMax of
		true -> YMax;
		_ ->
			case Y2<YMin of
				true-> YMin;
				_-> Y2
			end
		 end,	
	NX1= (NY1-B)/A,
	NX2= (NY2-B)/A,				
	{{NX1, NY1}, {NX2, NY2}}.			
    

cal_trends_data({XData,YData},datetime,true, {Exy,Ex,Ey,Ex_square,N})->
	{Exy+XData*YData, Ex+XData, Ey+YData, Ex_square+XData*XData, N+1};
cal_trends_data({XData,YData},_,_,TrendsData)->
	TrendsData.
	

%% name and color information


draw_graph_names(Datas, Chart, Font, Im) ->
    draw_graph_names(Datas, 0, Chart, Font, Im, 0, Chart#chart.graph_name_yh).
draw_graph_names([],_,_,_,_,_,_) -> ok;
draw_graph_names([{Name, _}|Datas], ColorIndex, Chart, Font, Im, Yo, Yh) ->
    Color = color_scheme(ColorIndex, Chart#chart.graph_rgba, plot2d, Chart#chart.data_count),
    draw_graph_name_color(Chart, Im, Font, Name, Color, Yo),
    draw_graph_names(Datas, ColorIndex + 1, Chart, Font, Im, Yo + Yh, Yh).

draw_graph_name_color(Chart, Im, Font, Name, Color, Yh) ->
    {{_X0,Y0}, {X1,_Y1}} = Chart#chart.bbx,
    Xo   = Chart#chart.graph_name_xo,
    Yo   = Chart#chart.graph_name_yo,
    Xl   = 50,
    LPt1 = {X1 - Xo - Xl, Y0 + Yo + Yh},
    LPt2 = {X1 - Xo, Y0 + Yo + Yh},

    {Fw,Fh} = egd_font:size(Font),
    Str     = string(Name,2),
    N       = length(Str),
    TPt     = {X1 - 2*Xo - Xl - Fw*N, Y0 + Yo + Yh - trunc(Fh/2) - 3},

    egd:filledRectangle(Im, LPt1, LPt2, Color),
    egd:text(Im, TPt, Font, Str, egd:color({0,0,0})).

%% origo crosshair

draw_origo_lines(Chart, Im) ->
    Black  = egd:color({20,20,20}),
    Black1 = egd:color({50,50,50}),
    {{X0,Y0},{X1,Y1}} = Chart#chart.bbx,
    {X,Y} = xy2chart({0,0}, Chart), 
    if
	X > X0, X < X1, Y > Y0, Y < Y1 -> 
	    egd:filledRectangle(Im, {X0,Y}, {X1,Y}, Black1),
	    egd:filledRectangle(Im, {X,Y0}, {X,Y1}, Black1);
	true -> 
	    ok
    end,
    egd:rectangle(Im, {X0,Y0}, {X1,Y1}, Black),
    ok.

% new ticks

draw_ticks(Chart, Im, Font, TenXData) ->
    {Xts, Yts} = Chart#chart.ticksize,
    {{Xmin,Ymin}, {Xmax,Ymax}} = Chart#chart.ranges,
    Ys = case Ymin of 
	Ymin when Ymin < 0 -> trunc(Ymin/Yts) * Yts;
	_ -> (trunc(Ymin/Yts) + 1) * Yts
    end,
    Xs = case Xmin of
	Xmin when Xmin < 0 -> trunc(Xmin/Xts) * Xts;
	_ -> (trunc(Xmin/Xts) + 1) * Xts
    end,
	draw_single_y_lp(Im, Chart, Ymin, Font, Ymin, Ymax),
    draw_yticks_lp(Im, Chart, Ys, Yts, Ymax, Font),
	case TenXData of
		[] ->
			draw_xticks_lp(Im, Chart, Xs, Xts, Xmax, Font);
		_ ->
			draw_xticks_lp_datetime( Im, Chart, Font, lists:reverse(TenXData) )
	end.

draw_single_y_lp(Im, Chart, Yi, Font, _Ymin, _Ymin)->
    {_,Y}          = xy2chart({0,Yi}, Chart),
    {{X,_}, _}     = Chart#chart.bbx,
    {_, Precision} = Chart#chart.precision,
    tick_text(Im, Font, Yi, {X,Y}, Precision, left);
draw_single_y_lp(_,_,_,_,_,_)->
	ok.


draw_xticks_lp_datetime(Im, Chart, Font, [Xi|TenXData]) ->
    {X,_}           = xy2chart({Xi,0}, Chart),
    {_, {_,Y}}      = Chart#chart.bbx,
    { Precision, _} = Chart#chart.precision,
    draw_perf_xbar(Im, Chart, X),
    egd:filledRectangle(Im, {X,Y-2}, {X,Y+2}, egd:color({0,0,0})),
	{XiText_1,XiText_2}= xi_text(Chart#chart.x_data_type, Chart#chart.contrast_type, Xi, Chart#chart.week_texts),
	if Chart#chart.contrast_type=:=undefined ->
		   	tick_text(Im, Font, XiText_2, {X,Y}, Precision, below),
			tick_text(Im, Font, XiText_1, {X,Y+10}, Precision, below);
	   true->
		   ok
	end,
	
	if Chart#chart.contrast_type=:=week  ->
		    bar_tick_text_egd(Im,XiText_2,Font,X,Y),
			bar_tick_text_egd(Im,XiText_1,Font,X,Y);
	   Chart#chart.contrast_type=:=month andalso TenXData=/=[] ->
			tick_text(Im, Font, XiText_2, {X,Y}, Precision, below),
			tick_text(Im, Font, XiText_1, {X,Y+10}, Precision, below);
	   Chart#chart.contrast_type=:=day andalso TenXData=/=[] ->
		   	tick_text(Im, Font, XiText_2, {X,Y}, Precision, below),
			tick_text(Im, Font, XiText_1, {X,Y+10}, Precision, below);
	   true->
		   ok
	end,

	{XT_1,XT_2}= xi_text(Chart#chart.x_data_type, undefined, Xi, []),
	if Chart#chart.contrast_type=:=week andalso TenXData=:=[] ->
			bar_tick_text_egd(Im, XT_1, Font, X, Y+13);
	   Chart#chart.contrast_type=:=month andalso TenXData=:=[] ->
			bar_tick_text_egd(Im, XT_1, Font, X, Y),
			bar_tick_text_egd(Im, XT_2, Font, X, Y+10);	   
	   Chart#chart.contrast_type=:=day andalso TenXData=:=[] ->
			bar_tick_text_egd(Im, XT_2, Font, X, Y),
			bar_tick_text_egd(Im, XT_1, Font, X, Y+10);	   
		true ->
			ok
	end,
    draw_xticks_lp_datetime(Im, Chart, Font, TenXData);
draw_xticks_lp_datetime(_,_,_, []) -> ok.

draw_yticks_lp(Im, Chart, Yi, Yts, Ymax, Font) when Yi < Ymax ->
    {_,Y}          = xy2chart({0,Yi}, Chart),
    {{X,_}, _}     = Chart#chart.bbx,
    {_, Precision} = Chart#chart.precision,
    draw_perf_ybar(Im, Chart, Y),
    egd:filledRectangle(Im, {X-2,Y}, {X+2,Y}, egd:color({0,0,0})),
    tick_text(Im, Font, Yi, {X,Y}, Precision, left),
    draw_yticks_lp(Im, Chart, Yi + Yts, Yts, Ymax, Font);
draw_yticks_lp(_,_,_,_,_,_) -> ok.

draw_xticks_lp(Im, Chart, Xi, Xts, Xmax, Font) when Xi < Xmax ->
    {X,_}           = xy2chart({Xi,0}, Chart),
    {_, {_,Y}}      = Chart#chart.bbx,
    { Precision, _} = Chart#chart.precision,
    draw_perf_xbar(Im, Chart, X),
    egd:filledRectangle(Im, {X,Y-2}, {X,Y+2}, egd:color({0,0,0})),
	{XiText_1,XiText_2}= xi_text(Chart#chart.x_data_type, Chart#chart.contrast_type, Xi, Chart#chart.week_texts),
	tick_text(Im, Font, XiText_2, {X,Y}, Precision, below),
	tick_text(Im, Font, XiText_1, {X,Y+10}, Precision, below),
    draw_xticks_lp(Im, Chart, Xi + Xts, Xts, Xmax, Font);
draw_xticks_lp(_,_,_,_,_,_) -> ok.

xi_text(XDataType, _ContrastType, Xi, _WeekTexts) when XDataType =:= topN_text ->
	fun	(X) when is_list(X) -> X; 
		(X) when is_atom(X)	-> atom_to_list(X); 
	   	(_X) -> "Text should be a lists!"			 
	end(Xi);
xi_text(XDataType, ContrastType, Xi, WeekTexts) when XDataType =:= datetime ->
	case catch calendar:gregorian_seconds_to_datetime(trunc(Xi)) of
		{'EXIT', _} -> 
			Xi;
		{{A,B,C},{D,E,F}} ->
			fun	(ContrastType2) when ContrastType2=:=day ->
					{"", integer_to_list(D)++" :"++integer_to_list(E)++" :"++integer_to_list(F)};
			   	(ContrastType2) when ContrastType2=:=week ->
					W= calendar:day_of_the_week(A,B,C),
					{"", int_to_week_text(W, WeekTexts)};
			   	(ContrastType2) when ContrastType2=:=month ->
					{"", integer_to_list(C)};
				(_ContrastType2) ->					
					{integer_to_list(A)++"-"++integer_to_list(B)++"-"++integer_to_list(C) , integer_to_list(D)++" :"++integer_to_list(E)++" :"++integer_to_list(F)}			   
			end(ContrastType)
    end;
xi_text(_XDataType, _ContrastType, Xi, _WeekTexts) ->
	Xi.

int_to_week_text(N, [])->
	int_to_week_text(N);
int_to_week_text(N, WeekTexts)->
	case catch lists:nth(N, WeekTexts) of
		{'EXIT', _} -> 
			int_to_week_text(N);
		T ->
			T
	end.

int_to_week_text(1)-> "Monday";
int_to_week_text(2)-> "Tuesday";	
int_to_week_text(3)-> "Wednesday";
int_to_week_text(4)-> "Thursday";
int_to_week_text(5)-> "Friday";
int_to_week_text(6)-> "Saturday";
int_to_week_text(7)-> "Sunday";
int_to_week_text(_)-> "week".

tick_text(Im, Font, Tick, {X,Y}, Precision, Orientation) ->
    String = string(Tick, Precision),
    L = length(String),
    {Xl,Yl} = egd_font:size(Font),
    PxL = L*Xl,
    {Xo,Yo} = case Orientation of
	above -> {-round(PxL/2), -Yl - 3};
	below -> {-round(PxL/2), 3};
	left  -> {round(-PxL - 4),-round(Yl/2) - 1};
	right -> {3, -round(Yl/2)};
	_ -> throw(tick_text_error)
    end,
    egd:text(Im, {X + Xo,Y + Yo}, Font, String, egd:color({0,0,0})).

draw_perf_threshold(Im, Chart) when is_integer(Chart#chart.threshold) orelse is_float(Chart#chart.threshold)  ->
	{{X0,Y0},{X1,Y1}} = Chart#chart.bbx,
	T= Chart#chart.threshold,
	{_,Y}= xy2chart({0,T}, Chart),
	case Y>=Y0 andalso Y=<Y1 of
		true ->
		    [Xl,Xr] = lists:sort([X0,X1]),
		    Color = egd:color({190,190,190}),
			egd:filledRectangle(Im, {Xl,Y}, {Xr,Y}, Color);
		_ ->
			ok
	end;
draw_perf_threshold(Im, Chart) ->
	ok.


draw_perf_titile(Im, Chart) ->
	Font= loadTitleFont(),	
	{W,H} = egd_font:size(Font),
	draw_perf_titile(Im, Chart, Chart#chart.title, Font, 0, W, H).
	
draw_perf_titile(Im, Chart, [], Font, Index, Width, Height) ->
	ok;
draw_perf_titile(Im, Chart, [H|Titles], Font, Index, Width, Height) ->
	X= round( (Chart#chart.width - textLength(H, Width, Width/2))/2 ),
	Y= round( 10 + Height*Index ),
	egd:text(Im, {X,Y}, Font, H, egd:color({0,0,0})),
	draw_perf_titile(Im, Chart, Titles, Font, Index+1, Width, Height).


textLength(Text, Width, Width2)->
	textLength(Text, Width, Width2, 0).

textLength([], Width,  Width2, Ret)->
	Ret;
textLength([H|T], Width, Width2, Ret) when H<256 ->
	textLength(T, Width, Width2, Ret+Width2);
textLength([H|T], Width,  Width2, Ret) ->
	textLength(T, Width,  Width2, Ret+Width).

	
% background tick bars, should be drawn with background
    
draw_perf_ybar(Im, Chart, Yi) ->
    Pw = 5,
    Lw = 10,
    {{X0,_},{X1,_}} = Chart#chart.bbx,
    [Xl,Xr] = lists:sort([X0,X1]),
    Color = egd:color({180,180,190}),
    lists:foreach(
	fun(X) ->
    	    egd:filledRectangle(Im, {X,Yi}, {X+Pw, Yi}, Color)
	end, lists:seq(Xl,Xr,Lw)),
    ok.

draw_perf_xbar(Im, Chart, Xi) ->
    Pw = 5,
    Lw = 10,
    {{_,Y0},{_,Y1}} = Chart#chart.bbx,
    [Yu,Yl] = lists:sort([Y0,Y1]),
    Color = egd:color({130,130,130}),
    lists:foreach(
	fun(Y) ->
    	    egd:filledRectangle(Im, {Xi,Y}, {Xi, Y+Pw}, Color)
	end, lists:seq(Yu,Yl,Lw)),
    ok.


%% bar2d/1 and bar2d/2
%% In:
%%	Data :: [{ Datasetname :: string(), [{Keyname :: atom() | string(), number() :: Value}]}]
%%		Datasetname = Name of this dataset (the color name)
%%		Keyname = The name of each grouping
%%	Options :: [{Key, Value}]
%%		Key = bar_width
%%		Key = column_width
%%		Colors?
%% Abstract:
%%	The graph is devided into column where each column have
%%	one or more bars.
%%	Each column is associated with a name.
%%	Each bar may have a secondary name (a key).

bar2d(Data) -> bar2d(Data, [{width, 600}, {height, 600}]).

bar2d(Data0, Options) ->
    %%io:format("Data   !!!!!!!!!!!!!~n~p~n", [Data0]),
    %%io:format("Options   !!!!!!!!!!!!!~n~p~n", [Options]),
    os:cmd("pause"),
	GraphC	 = proplists:get_value(graph_rgba, Options, []),
	XDataType	 = proplists:get_value(x_data_type, Options, datetime),
    {ColorMap, Data1} = bar2d_convert_data(GraphC, Data0, XDataType),	
	Data= case XDataType of
			topN_text -> lists:reverse(Data1);
			_ -> lists:sort(Data1)
		  end,
    TempChart = bar2d_chart(Options, Data),
	DataCount = length(Data0),
	case (is_integer(TempChart#chart.threshold) orelse is_float(TempChart#chart.threshold)) andalso DataCount =:= 1 of
		true ->
			Chart= TempChart#chart{bar_color_changing= over_threshold, data_count= DataCount};
		_ ->
			Chart= TempChart#chart{data_count= DataCount}
	end,	
    Im = egd:create(Chart#chart.width, Chart#chart.height),
    LightBlue = egd:color(Chart#chart.bg_rgba),
    {Pt1, Pt2} = Chart#chart.bbx,
    egd:filledRectangle(Im, Pt1, Pt2, LightBlue), % background

    draw_perf_threshold(Im, Chart),
	draw_perf_titile(Im, Chart),	

	% Fonts? Check for text enabling
	Font = case XDataType=:=topN_text orelse (Chart#chart.contrast_type=:=week andalso Chart#chart.week_texts=/=[]) of
			true -> loadExampleFont();
			_ -> loadFont()
		  end,

    draw_bar2d_ytick(Im, Chart, Font),

    % Color map texts for sets
    draw_bar2d_set_colormap(Im, Chart, loadExampleFont(), ColorMap),

    % Draw bars
    draw_bar2d_data(Data, Chart, Font, Im),
	
%% 	draw_xlabel(Chart, Im, Font),
%%     draw_ylabel(Chart, Im, Font),
  
    egd:rectangle(Im, Pt1, Pt2, egd:color({0,0,0})),
    Png = egd:render(Im, Chart#chart.type, [{render_engine, Chart#chart.render_engine}]),
    egd:destroy(Im),
    try erlang:exit(Im, normal) catch _:_ -> ok end,
    Png.
    
% [{Dataset, [{Key, Value}]}] -> [{Key, [{Dataset, Value}]}]
bar2d_convert_data(GraphC, Data, XDataType) -> bar2d_convert_data(GraphC, Data, 0,{[], []}, XDataType).
bar2d_convert_data(GraphC, [], _, {ColorMap, Out}, XDataType) -> {lists:reverse(ColorMap), Out};
bar2d_convert_data(GraphC, [{Set, KVs}|Data], ColorIndex, {ColorMap, Out}, XDataType) ->
    Color = color_scheme(ColorIndex, GraphC, bar2d, 50),
    bar2d_convert_data(GraphC, Data, ColorIndex + 1, {[{Set,Color}|ColorMap], bar2d_convert_data_kvs(KVs, Set, Color, Out, XDataType)}, XDataType).

bar2d_convert_data_kvs([], _,_, Out, XDataType) -> Out;
bar2d_convert_data_kvs([{Key, Value}|KVs], Set, Color, Out, XDataType) when XDataType=:=topN_text ->
	bar2d_convert_data_kvs(KVs, Set, Color, [{Key, [{{Color, Set}, Value}]}|Out], XDataType);
bar2d_convert_data_kvs([{Key, Value}|KVs], Set, Color, Out, XDataType) ->
    case proplists:get_value(Key, Out) of 
	undefined ->
	    bar2d_convert_data_kvs(KVs, Set, Color, [{Key, [{{Color, Set}, Value}]}|Out], XDataType);
	DVs ->
	    bar2d_convert_data_kvs(KVs, Set, Color, [{Key, [{{Color, Set}, Value} | DVs]} | proplists:delete(Key, Out)], XDataType)
    end.

    
% beta color map, static allocated

bar2d_chart(Opts, Data) ->
    Values = lists:foldl(fun
	({_, DVs}, Out) ->
	    Vs = [V || {_,V} <- DVs],
	    Out ++ Vs
	end, [], Data),
    Type     = proplists:get_value(type,          Opts, png),
    Margin   = proplists:get_value(margin,        Opts, 30),
    Width    = proplists:get_value(width,         Opts, 600),
    Height   = proplists:get_value(height,        Opts, 600),
    Xlabel    = proplists:get_value(x_label,      Opts, "X"),
    Ylabel    = proplists:get_value(y_label,      Opts, "Y"),	
    Xrange   = proplists:get_value(y_range,       Opts, 0),
    YrangeMax = proplists:get_value(y_range_max,  Opts, lists:max([Xrange|Values])),
    Ranges   = proplists:get_value(ranges,        Opts, {{0,0}, {length(Data), YrangeMax}}),
    Ticksize = proplists:get_value(ticksize,      Opts, smart_ticksize(Ranges, 10)),
    Cw       = proplists:get_value(column_width,  Opts, {ratio, 0.8}),
    Bw       = proplists:get_value(bar_width,     Opts, {ratio, 1.0}),
    InfoW    = proplists:get_value(info_box,      Opts, 0),
    Renderer = proplists:get_value(render_engine, Opts, opaque),
    % colors
    BGC      = proplists:get_value(bg_rgba,       Opts, {255, 255, 255, 255}),
    MGC      = proplists:get_value(margin_rgba,   Opts, {255, 255, 255, 255}),
	GraphC	 = proplists:get_value(graph_rgba,    Opts, []),	
	XDataType= proplists:get_value(x_data_type,   Opts, datetime),	
	ContrastType= proplists:get_value(contrast_type,Opts, undefined),	
	WeekText = proplists:get_value(week_texts,   Opts, []),	
	Threshold= proplists:get_value(threshold,     Opts, undefined),		
	Title1	 = proplists:get_value(title1,    	  Opts, []),
	Title2	 = proplists:get_value(title2,    	  Opts, []),	
	
    % bounding box
    IBBX     = {{Width - Margin - InfoW, Margin}, {Width - Margin, Height - Margin}},
    BBX      = {{Margin, Margin+?title_height}, {Width - Margin - InfoW - 10, Height - Margin}},
    DxDy     = update_dxdy(Ranges, BBX),

    #chart{
	type        = Type,
	margin	 	= Margin,
	width	 	= Width,
	height	 	= Height,
	x_label	 	= Xlabel,
	y_label	 	= Ylabel,
	ranges   	= Ranges,
	ticksize 	= Ticksize,
	bbx	 		= BBX,
	ibbx        = IBBX,
	dxdy 	 	= DxDy,
	column_width= Cw,
	bar_width 	= Bw,
	margin_rgba = MGC,
	bg_rgba     = BGC,
	graph_rgba	= GraphC,
	render_engine= Renderer,
	x_data_type	=XDataType,
	contrast_type=ContrastType,	
	week_texts= listTextGbkToUnicode(WeekText),
	threshold   = Threshold,
	title	  = gbkToUnicode(Title1,Title2)
    }.

draw_bar2d_set_colormap(Im, Chart, Font, ColorMap) when Chart#chart.bar_color_changing =:= over_threshold ->
	ok;
draw_bar2d_set_colormap(Im, Chart, Font, ColorMap) ->
    Margin = Chart#chart.margin,
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap, {Margin, ?title_height}, Margin, 0).

draw_bar2d_set_colormap(_, _, _, [], _, _,_) -> ok;
draw_bar2d_set_colormap(Im, Chart, Font, [{Set, Color}|ColorMap], {X, Y}, Margin, MaxLen) ->
    String = gbkToUnicode(string(Set, 2)),
	{Width,_} = egd_font:size(Font),
	Len= textLength(String, Width, Width/2),
	NewMaxLen= case Len>MaxLen of
		true -> Len;
		_ ->  MaxLen
	end,					
    egd:text(Im, {X + 10, Y+14}, Font, String, egd:color({0,0,0})),
    egd:filledRectangle(Im, {X,Y+17}, {X+5, Y+22}, Color),
    draw_bar2d_set_colormap_step(Im, Chart, Font, ColorMap, {X,Y}, Margin, NewMaxLen).

draw_bar2d_set_colormap_step(Im, Chart, Font, ColorMap, {X,Y}, Margin, MaxLen) when (Y + 35) < Margin+?title_height ->
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap, {X, Y + 12}, Margin, MaxLen);
draw_bar2d_set_colormap_step(Im, Chart, Font, ColorMap, {X,Y}, Margin, MaxLen) ->
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap, {round(X+MaxLen+30), ?title_height}, Margin, 0).

draw_bar2d_ytick(Im, Chart, Font) ->
    {_, Yts}               = Chart#chart.ticksize,
    {{_, _}, {_, Ymax}} = Chart#chart.ranges,
    draw_bar2d_yticks_up(Im, Chart, Yts, Yts, Ymax, Font).   %% UPPER tick points
    
draw_bar2d_yticks_up(Im, Chart, Yi, Yts, Ymax, Font) when Yi < Ymax ->
    {X, Y}         = xy2chart({0,Yi}, Chart),
    {_, Precision} = Chart#chart.precision,
    draw_bar2d_ybar(Im, Chart, Y),
    egd:filledRectangle(Im, {X-2,Y}, {X+2,Y}, egd:color({0,0,0})),
    tick_text(Im, Font, Yi, {X,Y}, Precision, left),
    draw_bar2d_yticks_up(Im, Chart, Yi + Yts, Yts, Ymax, Font);
draw_bar2d_yticks_up(_,_,_,_,_,_) -> ok.

draw_bar2d_ybar(Im, Chart, Yi) ->
    Pw = 5,
    Lw = 10,
    {{X0,_},{X1,_}} = Chart#chart.bbx,
    [Xl,Xr] = lists:sort([X0,X1]),
    Color = egd:color({180,180,190}),
    lists:foreach(
	fun(X) ->
    	    egd:filledRectangle(Im, {X-Pw,Yi}, {X, Yi}, Color)
	end, lists:seq(Xl+Pw,Xr,Lw)),
    ok.

draw_bar2d_data(Columns, Chart, Font, Im) ->
    {{Xl,_}, {Xr,_}} = Chart#chart.bbx,
    Cn = length(Columns),	% number of columns
    Co = (Xr - Xl)/(Cn),	% column offset within chart
    Cx = Xl + Co/2,		% start x of column
	ShowIndex = case Chart#chart.x_data_type =:= topN_text of
			   		true -> round(Cn/18);
					_ -> round(Cn/9.5)
				end,
	{Fw, Fh} = egd_font:size(Font),
    draw_bar2d_data_columns(Columns, Chart, Font, Im, Cx, Co, 0, ShowIndex, Cn-1, ((Xr - Xl)/(Cn))-Fh).

draw_bar2d_data_columns([], _, _, _, _, _, _, _, _, _) -> ok;
draw_bar2d_data_columns([{Name, Bars} | Columns], Chart, Font, Im, Cx, Co, Index, ShowIndex, LastIndex, ColumnWidth) ->
    {{X0,Y0}, {X1,Y1}} = Chart#chart.bbx,

    Cwb = case Chart#chart.column_width of
	default -> Co;
	{ratio, P} when is_number(P) -> P*Co;
	Cw when is_number(Cw) -> lists:min([Cw,Co])
    end,

    %% draw column text
	case ( ShowIndex =:= 0 orelse Index =:= LastIndex orelse (Index rem ShowIndex =:= 0 andalso (LastIndex-Index)>=ShowIndex )) of
		true ->
			X= trunc(Cx),			
			egd:filledRectangle(Im, {X,Y1}, {X,Y1+3}, egd:color({0,0,0})),
			XDataType= Chart#chart.x_data_type,
			XiText= xi_text(XDataType, Chart#chart.contrast_type, Name, Chart#chart.week_texts),
			case  XDataType of
				datetime ->
					{XiText_1,XiText_2}= XiText, 
					bar_tick_text(Im, Font, XDataType, Chart#chart.contrast_type, XiText_2, X, Y1, ColumnWidth),
					bar_tick_text(Im, Font, XDataType, Chart#chart.contrast_type, XiText_1, X, Y1+10, ColumnWidth);
				_ ->
					bar_tick_text(Im, Font, XDataType, Chart#chart.contrast_type, XiText, X, Y1, ColumnWidth)
			end;
		_ ->
			ok
	end,

    Bn = length(Bars),		% number of bars
    Bo = Cwb/Bn,	        % bar offset within column
    Bx = Cx - Cwb/2 + Bo/2,	% starting x of bar

    CS = 43,
    draw_bar2d_data_bars(Bars, Chart, Font, Im, Bx, Bo, CS, Index, length(Bars)),
    draw_bar2d_data_columns(Columns, Chart, Font, Im, Cx + Co, Co, Index+1, ShowIndex, LastIndex, ColumnWidth).

bar_tick_text(Im, Font, XDataType, _ContrastType, Name, Cx, Y1, ColumnWidth) when  XDataType=:=topN_text ->
	String = gbkToUnicode(string(Name, 2)),
	{Fw, Fh} = egd_font:size(Font),
	{S1,S2}= get_proper_text(length(String),String,Font,ColumnWidth, Fw, Fw/2),
	bar_tick_text_egd(Im,S1,Font,Cx,Y1),
	{S3,S4}= get_proper_text(length(S2),S2,Font,ColumnWidth, Fw, Fw/2),
	bar_tick_text_egd(Im,S3,Font,Cx,Y1+Fh),
	{S5,S6}= get_proper_text(length(S4),S4,Font,ColumnWidth, Fw, Fw/2),
	bar_tick_text_egd(Im,S5,Font,Cx,Y1+Fh+Fh);
bar_tick_text(Im, Font, XDataType, ContrastType, Name, Cx, Y1, ColumnWidth) when ContrastType=:=week ->
	bar_tick_text_egd(Im,Name,Font,Cx,Y1);
bar_tick_text(Im, Font, XDataType, _ContrastType, Name, Cx, Y1, ColumnWidth) ->
	bar_tick_text_egd(Im,string(Name, 2),Font,Cx,Y1).

bar_tick_text_egd(Im,String,Font,Cx,Y1)->
	{Fw, Fh} = egd_font:size(Font),
	Fw2= case Font of
			'6x11_latin1' -> Fw;
			_ -> Fw/2
		 end,	
    L = textLength(String, Fw, Fw2), %%Fw*Ns,
    Tpt = {trunc(Cx - L/2 + 2), Y1 + 3},
    egd:text(Im, Tpt, Font, String, egd:color({0,0,0})).


get_proper_text(0,String,_,_,_,_)->
	{String,""};
get_proper_text(N,String,Font,ColumnWidth, Fw, Fw_2)->
	{S1,S2}= lists:split(N, String),
	L = textLength(S1, Fw, Fw_2),
	case L>ColumnWidth of
		true->
			get_proper_text(N-1,String,Font,ColumnWidth,Fw, Fw/2);
		_->
			{S1,S2}
	end.
  

draw_bar2d_data_bars([], _, _, _, _, _, _, _, _) -> ok;
draw_bar2d_data_bars([{{TempColor,Set}, Value}|Bars], Chart, Font, Im, Bx, Bo,CS, Index, BarCount) ->
    {{X0,Y0}, {X1,Y1}} = Chart#chart.bbx,
    {_, Precision}     = Chart#chart.precision,
    {_, Y}             = xy2chart({0, Value}, Chart),

    Bwb2 = case Chart#chart.bar_width of
	default -> Bo;
	{ratio, P} when is_number(P) -> P*Bo;
	Bw when is_number(Bw) -> lists:min([Bw,Bo])
    end,
	
	FitWidth= round(Chart#chart.width/22),
	Bwb= case Bwb2>FitWidth of
			 true->
				 FitWidth;
			 _->
				Bwb2
		 end,
    Black = egd:color({0,0,0}),

    % draw bar text
    String = string(Value, Precision),
    Ns = length(String),
    {Fw, Fh} = egd_font:size(Font),
	Fw2= case Font of
			'6x11_latin1' -> Fw;
			_ -> Fw/2
		 end,	
    L = textLength(String, Fw, Fw2), %%Fw*Ns,	
    Tpt = {trunc(Bx - L/2 + 1), Y - Fh - 5},
%%    egd:text(Im, Tpt, Font, String, Black),

   	Color= case Chart#chart.x_data_type =:= topN_text of
			   	true -> color_scheme(Index,[], bar2d, BarCount);
				_ -> get_bar_changing_color(TempColor, Index, Chart#chart.bar_color_changing, Value, Chart#chart.threshold)
		   end,
    %% ÅÐ¶ÏÊÇ·ñÒç³ö×Ý×ø±ê
    YValue = 
    if
        Y < Y0 ->
            Y0;
        true ->
            Y
    end,
    
    Pt1 = {trunc(Bx - Bwb/2), YValue},
    Pt2 = {trunc(Bx + Bwb/2), Y1},
    egd:filledRectangle(Im, Pt1, Pt2, Color),
    egd:rectangle(Im, Pt1, Pt2, Black),
    draw_bar2d_data_bars(Bars, Chart, Font, Im, Bx + Bo, Bo, CS + CS, Index, BarCount).

get_bar_changing_color(Color,Index,Changing, Value, Threshold)when Changing =/= over_threshold->
	Color;
get_bar_changing_color(Color,Index,Changing, Value, Threshold)->
%%  	color_scheme(Index,[], bar2d, 10). %% show all 20 colors
	I= case Value=<Threshold of
		   true -> 9;
		   _ -> 4
	   end,
	color_scheme(I,[], bar2d, 50).
   

%%==========================================================================
%%
%%              Aux functions     
%%
%%==========================================================================

%% convert X,Y value to coordinate
xy2chart({X,Y}, Chart) ->
    {{Rx0,Ry0}, {_Rx1,_Ry1}} = Chart#chart.ranges,
    {{Bx0,By0}, {Bx1,By1}} = Chart#chart.bbx,
    {Dx, Dy} = Chart#chart.dxdy,
    {round(X*Dx + Bx0 - Rx0*Dx), round(By1 - (Y*Dy + By0 - Ry0*Dy - Chart#chart.margin - ?title_height))}. 


ranges([{_Name, Es}|Data]) when is_list(Es) ->
    Ranges = xy_minmax(Es),
    ranges(Data, Ranges).

ranges([], Ranges) -> Ranges;
ranges([{_Name, Es}|Data], CoRanges) when is_list(Es) ->
    Ranges = xy_minmax(Es),
    ranges(Data, xy_resulting_ranges(Ranges, CoRanges)).

    
smart_ticksize({{X0, Y0}, {X1, Y1}}, N) ->
    { smart_ticksize(X0,X1,N), smart_ticksize(Y0,Y1,round(N/2))}.


smart_ticksize(S, E, N) when is_number(S), is_number(E), is_number(N) ->
    % Calculate stepsize then 'humanize' the value to a human pleasing format.
    R = abs((E - S))/N,
    if 
	abs(R) < ?float_error -> 2.0;
	true ->
	    % get the ratio on the form of 2-3 significant digits.
	    %V =  2 - math:log10(R),
	    %P = trunc(V + 0.5),
	    P = precision_level(S, E, N),
	    M = math:pow(10, P),
	    Vsig = R*M,
	    %% do magic    
	    Rsig = Vsig/50,
	    Hsig = 50 * trunc(Rsig + 0.5),
	    %% fin magic
	    Hsig/M
    end;
smart_ticksize(_, _, _) -> 2.0.

precision_level({{X0, Y0}, {X1, Y1}}, N) ->
     { precision_level(X0,X1,N), precision_level(Y0,Y1,N)}.

precision_level(S, E, N) when is_number(S), is_number(E) ->
    % Calculate stepsize then 'humanize' the value to a human pleasing format.
    R = abs((E - S))/N,
    if 
	abs(R) < ?float_error -> 2;
	true ->
	    % get the ratio on the form of 2-3 significant digits.
	    V =  2 - math:log10(R),
	    P = trunc(V + 0.5)
    end;
precision_level(_, _, _) -> 2.

% on form [{X,Y}]
xy_minmax(Elements) ->
    Xs = [ X || {X,_} <- Elements ],
    Ys = [ Y || {_,Y} <- Elements ],
    {{lists:min(Xs),lists:min(Ys)}, {lists:max(Xs), lists:max(Ys)}}.

xy_resulting_ranges({{X0,Y0},{X1,Y1}},{{X2,Y2},{X3,Y3}}) ->
    { 	
	{lists:min([X0,X1,X2,X3]),
	 lists:min([Y0,Y1,Y2,Y3])},
	{lists:max([X0,X1,X2,X3]),
	 lists:max([Y0,Y1,Y2,Y3])}
    }.

update_dxdy({{Rx0, Ry0}, {Rx1, Ry1}} = A, {{Bx0,By0},{Bx1,By1}} = B) ->
   Dx = divide((Bx1 - Bx0),(Rx1 - Rx0)),
   Dy = divide((By1 - By0),(Ry1 - Ry0)),
   {Dx,Dy}.

divide(T,N) when abs(N) < ?float_error -> 0.0;
%divide(T,N) when abs(N) < ?float_error -> exit({bad_divide, {T,N}});
divide(T,N) -> T/N.

print_info_chart(Chart) ->
    io:format("Chart ->~n"),
    io:format("    type:     ~p~n", [Chart#chart.type]),
    io:format("    margin:   ~p~n", [Chart#chart.margin]),
    io:format("    bbx:      ~p~n", [Chart#chart.bbx]),
    io:format("    ticksize: ~p~n", [Chart#chart.ticksize]),
    io:format("    ranges:   ~p~n", [Chart#chart.ranges]),
    io:format("    width:    ~p~n", [Chart#chart.width]),
    io:format("    height:   ~p~n", [Chart#chart.height]),
    io:format("    dxdy:     ~p~n", [Chart#chart.dxdy]),
    ok.

string(E, P) when is_atom(E)    -> atom_to_list(E);
string(E, P) when is_float(E)   -> float_to_maybe_integer_to_string(E, P); 
string(E, P) when is_integer(E) -> s("~w", [E]);
string(E, P) when is_binary(E)  -> lists:flatten(binary_to_list(E));
string(E, P) when is_list(E)    -> s("~s", [E]).

float_to_maybe_integer_to_string(F, P) ->
    I = trunc(F),
    A = abs(I - F),
    if 
	% integer
	A < ?float_error -> s("~w", [I]);

	true ->
	% float
	    Format = s("~~.~wf", [P]),
	    s(Format, [F])
    end.

s(Format, Terms) ->
	Text= 
	case catch begin
				  For= io_lib:format(Format, Terms),
				  {ok, For} 
			   end of
		{ok,F}->
			F;
		_->
			""
    end,
	lists:flatten(Text).


prepare_contrast_data(_Opts, Data, undefined, _XDataType)->
	{0,0,Data};
prepare_contrast_data(_Opts, Data, _ContrastType, XDataType) when XDataType =/= datetime andalso XDataType =/= undefined ->
	{0,0,Data};
prepare_contrast_data(_Opts, Data, ContrastType, _XDataType) ->
	{Name, Es}= lists:nth(1,Data),
	{ZeroDate,Scale}= getZeroDate(Es, ContrastType),
	io:format("contrast report  ZeroDate:~p   Scale:~p~n",[calendar:gregorian_seconds_to_datetime(ZeroDate),Scale]),
	NewData= lists:foldl(
			fun ({Name, Es}, Last) ->
				{ZeroDate2,_}= getZeroDate(Es, ContrastType),
				Move=  ZeroDate2-ZeroDate,
				NewEs= lists:foldl(
				  		fun( {Time,V},Acc )->
								Diff= Time-ZeroDate2,
								case Diff>=0 andalso Diff<Scale of
									true-> [{Time-Move, V}|Acc];
									_-> Acc
								end
						end, [], Es),
				case NewEs=:=[] of
					true-> Last;
					_->[{Name, lists:reverse(NewEs)}|Last]
				end
			end, [], Data),
	{ ZeroDate, Scale, lists:reverse(NewData) }.

getZeroDate(Es, ContrastType)->
	{Xmin,_} = lists:min(Es),
	{{A,B,C},{_,_,_}}= calendar:gregorian_seconds_to_datetime(trunc(Xmin)),
	{_ZeroDate,_Scale}=
		case ContrastType of
			day-> 	
				{calendar:datetime_to_gregorian_seconds( {{A,B,C},{0,0,0}} ),
				 86400};
			month->	
				Z= calendar:datetime_to_gregorian_seconds( {{A,B,1},{0,0,0}} ), 
				{Z,getNextMothZeroSecond(A,B)-Z };			  
			week->  
				W= case calendar:day_of_the_week(A,B,C) of
					   7-> 0;
					   N-> N				   
				   end,
				{calendar:datetime_to_gregorian_seconds( {{A,B,C},{0,0,0}} ) - W*86400,
				 604800}
		end.	


setTenXLocationForContrast(ZeroDate, Scale)->
	{N,Gap,Type}= if
					Scale=:=86400  -> {12,7200,day};
		   			Scale=:=604800 -> {7,86400,week};
	   	   			Scale >=2505600-> {10,259200,month};
				  	true 		   -> {9,259200,month}
	   		  	  end,
	setTenXLocationForContrast(ZeroDate, Gap, N, N, [], Type).

setTenXLocationForContrast(_ZeroDate, _Gap, -1, _Count, Ret, Type)->
	lists:reverse(Ret);
setTenXLocationForContrast(ZeroDate, Gap, Count, Count, Ret, Type)->
	fun(Type) when Type=:=day ->
			setTenXLocationForContrast(ZeroDate, Gap, Count-1, Count, [(ZeroDate+Gap*Count-1)|Ret], Type);
	   (month)->
			{{A,B,_},{_,_,_}}= calendar:gregorian_seconds_to_datetime(ZeroDate),
			Second= getNextMothZeroSecond(A,B) -1,
			setTenXLocationForContrast(ZeroDate, Gap, Count-1, Count, [Second|Ret], Type);
	   (_)->
			setTenXLocationForContrast(ZeroDate, Gap, Count-1, Count, Ret, Type)
	end(Type);
setTenXLocationForContrast(ZeroDate, Gap, Index, Count, Ret, Type)->
	setTenXLocationForContrast(ZeroDate, Gap, Index-1, Count, [(ZeroDate+Gap*Index)|Ret], Type).
															   

getNextMothZeroSecond(Year,CurrentMonth)->
  	{Y,M}= 	case CurrentMonth of
				12-> {Year+1,1};
				_->  {Year,CurrentMonth+1}
			end,
	calendar:datetime_to_gregorian_seconds( {{Y,M,1},{0,0,0}} ).


	
	

