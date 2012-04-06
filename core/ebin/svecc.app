%% This is the application resource file (.app file) for the 'base'
%% application.
{application, svecc,
[{description, "siteview ecc" },
{vsn, "1.0" },
{modules, [svecc_app, monitor_manager,monitor_group,siteview]},
{registered,[svecc_app]},
{applications, [kernel,stdlib]},
{mod, {svecc_app,[]}},
{start_phases, []}
]}.