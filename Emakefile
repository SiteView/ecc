
{"./*", [{outdir,"./"}]}.

{"core/eldap/*", [{ i, "core/include" }, {outdir, "core/ebin"}]}.
{"core/src/*", [debug_info,{ i, "core/include" }, {outdir, "core/ebin"}]}.
{"core/utils/*", [debug_info,{i, "core/include" }, {outdir, "core/ebin"}]}.

{"web/src/*", [{d,debug},{ i, "web/include" },{outdir, "web/ebin"}]}.
{"plugin/src/*", [
	{ i, "plugin/include" },
	{ i, "./nitrogen/include" },
	{ i, "additionmod/erlang-gettext/include" },
	{ i, "core/src"},
	{outdir, "plugin/ebin"}
]}.

% Compile Nitrogen Files
{ "nitrogen/src/*", [
	{ i, "nitrogen/include" },
	{ i, "additionmod/erlang-gettext/include" },
	{ i, "core/include"},
	{ outdir, "nitrogen/ebin" }
]}.
{ "nitrogen/src/pages/*", [
	{ i, "nitrogen/include" },
	{ i, "additionmod/erlang-gettext/include" },
	{ i, "core/include"},
	{ outdir, "nitrogen/ebin" }
]}.
{ "nitrogen/src/reference/*", [
	{ i, "nitrogen/include" },
	{ i, "additionmod/erlang-gettext/include" },
	{ i, "core/include"},
	{ outdir, "nitrogen/ebin" }
]}.

{"additionmod/erlang-gettext/src/*", [{ i, "additionmod/erlang-gettext/include" },{outdir, "additionmod/erlang-gettext/ebin"}]}.

{"modules/df_snmp/src/*",[{outdir,"modules/df_snmp/ebin"}]}.

{"modules/erlcmdb/src/*",[{ i, "core/src"},{outdir,"modules/erlcmdb/ebin"}]}.

{"modules/nmap_scan/src/*",[{outdir,"modules/nmap_scan/ebin"}]}.

{"modules/nnm/src/*", [{ i, "modules/nnm/include" },{ i,"core/src"},{outdir,"modules/nnm/ebin"}]}.

{"modules/assets/src/*", [{ i, "modules/assets/include" },{ i,"core/src"},{outdir,"modules/assets/ebin"}]}.

{"core/utils/GsmOperateUtils/gsmOperate.erl",[{outdir,"core/utils/GsmOperateUtils/"}]}.

%{ './sec/src/*', [{ i, "./sec/include" },{ outdir, "./sec/ebin" },debug_info]}.
%{ './sec/src/*/*', [{ i, "./sec/include" },{ outdir, "./sec/ebin" },debug_info]}.
%{ './sec/parser/*', [{ i, "./sec/include" },{ outdir, "./sec/ebin" },debug_info]}.

%{["./store/src/*", "./store/test/*"], [{outdir, "./store/ebin"}]}.

%{['./rest/src/*'], [{d,debug},{outdir, "./rest/ebin"}]}.

%{['./rest/deps/*'], [{outdir, "./rest/deps"}]}.


%% mysql support 

{"./eccdb/amnesia/*", [{i,"./eccdb/amnesia"},{outdir,"core/ebin"}]}.
{"./eccdb/*", [{i,"./eccdb/amnesia"},{outdir,"core/ebin"}]}.

