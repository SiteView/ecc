%% Author: Administrator
%% Created: 2010-1-7
%% Description: TODO: Add description to samplestatisticsproc
-module(samplestatisticsproc,[BASE]).

%%
%% Include files
%%
-extends(siteview_object).
-compile(export_all).
-include("monitor.hrl").


%%
%% API Functions
%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
new() ->
	Obj = siteview_object:new(),
	Obj:set_property(worstCategory, nodata),
	Obj:set_property(lastCategory, nodata),
	Obj:set_property(sampleCount, 0),
	Obj:set_property(minimum, 0.0),
	Obj:set_property(maximum, 0.0),
	Obj:set_property(total, 0.0),
	Obj:set_property(totalSquared, 0.0),
	Obj:set_property(totalSamples, 0.0),
	Obj:set_property(totalValue, ""),
	{?MODULE,Obj}.
	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getSampleCount() ->	
	{ok,{_,SampleCount}} = THIS:get_property(sampleCount),	
	SampleCount.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotal() -> 
	{ok,{_,Total}} = THIS:get_property(total),
	Total.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalSquared() ->
	{ok,{_,TotalSquared}} = THIS:get_property(totalSquared),
	TotalSquared.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalSamples() ->
	{ok,{_,TotalSamples}} = THIS:get_property(totalSamples),
	TotalSamples.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalValue() ->
	{ok,{_,TotalValue}} = THIS:get_property(totalValue),
	TotalValue.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getAverage() ->
	{ok,{_,TotalSamples}} = THIS:get_property(totalSamples),
	{ok,{_,Total}} = THIS:get_property(total),	
    if
		(TotalSamples == 0) or not(is_float(Total))->
            float(0.0);
        true ->
            Total/float(TotalSamples)
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getStandardDeviation() ->
	{ok,{_,TotalSamples}} = THIS:get_property(totalValue),
	{ok,{_,Total}} = THIS:get_property(total),	
	{ok,{_,TotalSquared}} = THIS:get_property(totalSquared),	
	TotalSquared.


%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMaximum() ->
	{ok,{_,Maximum}} = THIS:get_property(maximum),	
	Maximum.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMinimum() ->
	{ok,{_,Minimum}} = THIS:get_property(minimum),	
	Minimum.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getWorstCategory() ->
	{ok,{_,WorstCategory}} = THIS:get_property(worstCategory),	
	WorstCategory.
