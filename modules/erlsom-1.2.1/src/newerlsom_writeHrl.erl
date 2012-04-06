-module(newerlsom_writeHrl).
-export([writeHrl/1]).
-export([writeHrlFile/3]).
-export([writeXsdHrlFile/2]).
%% internal exports
-export([writeType/2]).
-export([writeAttribute/2]).
-compile(export_all).
-include("erlsom_parse.hrl").
-include("erlsom.hrl").

%% debug(Text) -> io:format("writeHrl: ~p~n", [Text]).

%% debug(Text1, Text2) ->
  %% io:format("~p ~p~n", [Text1, Text2]).

writeHrl(#model{tps = Types}) ->
  Acc = "",
  writeTypes(Types, Acc).

writeHrlFile(Xsd, Prefix, Namespaces) ->
%% compile file
  Result = erlsom:compile(Xsd, Prefix, Namespaces),
  case Result of
    {ok, Model} -> 
      writeHrl(Model);
    {error, Error} -> 
      io:format("Error while compiling file: ~p~n", [Error])
  end.

writeXsdHrlFile(Xsd, Options) ->
%% compile file
  Result = erlsom:compile_xsd(Xsd, Options),
  case Result of
    {ok, Model} -> 
      writeHrl(Model);
    {error, Error} -> 
      throw({error, Error})
  end.

writeTypes(Types, Acc) ->
  Acc ++ lists:foldl({erlsom_writeHrl, writeType}, [], Types).

writeType(#type{nm = '_document'}, Acc) ->
  Acc;
%% writeType(Type, []) -> writeType2(Type);
writeType(Type, Acc) ->
  Acc ++ writeType2(Type).

writeType2(#type{nm = Name, els = Elements, atts = Attributes}) ->
  "-record('" ++ atom_to_list(Name)
  ++ joinStrings("', {anyAttribs",
                 joinStrings(writeAttributes(Attributes), 
		             writeElements(Elements)))
  ++ "}).\n".

writeElements(Elements) ->
  writeElements(Elements, [], 0).
writeElements([], String, _) ->
  String;
writeElements([Element], Acc, CountChoices) ->
  {String, _} = writeElement(Element, CountChoices),
  Acc ++ String;
writeElements([Element | Tail], Acc, CountChoices) ->
  {String, CountChoices2} = writeElement(Element, CountChoices),
  writeElements(Tail, Acc  ++ String ++ ", ", CountChoices2).

writeElement(#el{alts = Alternatives}, CountChoices) ->
  writeAlternatives(Alternatives, CountChoices).

%% easy case: 1 alternative (not a choice), 'real' element (not a group)
writeAlternatives([], CountChoices) ->
  {"any_strict_but_none_defined", CountChoices};
writeAlternatives([#alt{tag = '#any'}], CountChoices) ->
  {"any", CountChoices};
writeAlternatives([#alt{tag = Tag, rl = true}], CountChoices) ->
  {"'" ++ erlsom_lib:nameWithoutPrefix(atom_to_list(Tag)) ++ "'", CountChoices};
writeAlternatives([#alt{tag = Tag, rl = false, tp = {_,_}}], CountChoices) ->
  {"'" ++ erlsom_lib:nameWithoutPrefix(atom_to_list(Tag)) ++ "'", CountChoices};
writeAlternatives([#alt{rl = false, tp=Tp}], CountChoices) ->
  {"'" ++ erlsom_lib:nameWithoutPrefix(atom_to_list(Tp)) ++ "'", CountChoices};
%% more than 1 alternative: a choice
writeAlternatives([#alt{} | _Tail], CountChoices) ->
  Acc = case CountChoices of
         0 ->
           "choice";
         _ -> 
           "choice" ++ integer_to_list(CountChoices)
       end,
  {Acc, CountChoices +1}.
      

writeAttributes(Attributes) ->
  lists:foldl({erlsom_writeHrl, writeAttribute}, [], Attributes).

writeAttribute(#att{nm = Name}, []) -> "'" ++ atom_to_list(Name) ++ "'";
writeAttribute(#att{nm = Name}, Acc) -> Acc  ++ ", '" ++ atom_to_list(Name) ++ "'".

joinStrings([], StringB) ->
  StringB;
joinStrings(StringA, []) ->
  StringA;
joinStrings(StringA, StringB) ->
  StringA ++ ", " ++ StringB.
	
    
getparams(#model{tps = Types}) ->
  Acc = [],
  getparams1(Types, Acc).
  

getparams1([],Acc)->Acc;

getparams1([#type{nm = '_document'}|R], Acc) ->
    getparams1(R,Acc);
    
getparams1([#type{nm = Name, els = Elements}|R], Acc) ->
    getparams1(R,[{erlsom_lib:nameWithoutPrefix(atom_to_list(Name)),getparams2(Elements)}]++Acc);

getparams1([_|R], Acc) ->
    getparams1(R,Acc).
	
getparams2([])->[];
getparams2([#el{alts = Alternatives}|R])->
    getparams3(Alternatives)++getparams2(R).
    
getparams3([])->[];
getparams3([#alt{tag = Tag, tp = {_,Tp}}|R]) ->
    [{erlsom_lib:nameWithoutPrefix(atom_to_list(Tag)),Tp}]++getparams3(R);
getparams3([#alt{tag = Tag, tp = Tp}|R]) ->
    [{erlsom_lib:nameWithoutPrefix(atom_to_list(Tag)),Tp}]++getparams3(R);
getparams3([F|R]) ->
    io:format("skip element is:~p~n",[F]),
    getparams3(R).
	