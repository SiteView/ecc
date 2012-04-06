%% This is the application resource file (.app file) for the 'base'
%% application.
{application, svecc_log,
[{description, "siteview ecc logger" },
{vsn, "1.0" },
{modules, [svecc_logapp]},
{registered,[svecc_logapp]},
{applications, [kernel,stdlib]},
{mod, {svecc_logapp,[]}},
{start_phases, []}
]}.