%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(ssTreeObject,[BASE]).
-extends(siteview_object).
-compile(export_all).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new()->
	Obj = siteview_object:new(),

	Obj:set_attribute(id, ""),
	Obj:set_attribute(name, ""),	
	Obj:set_attribute(properties, ""),
	Obj:set_attribute(permissionsObj, ""),
	
	Obj:set_attribute(defualtComparator, []),
	Obj:set_attribute(treeObject, []),
	Obj:set_attribute(children, []),
	Obj:set_attribute(parent,[]),
	Obj:set_attribute(useParentHierarchicalEquality, false),
	Obj:set_attribute(useChildrenHierarchicalEquality, false),
	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
