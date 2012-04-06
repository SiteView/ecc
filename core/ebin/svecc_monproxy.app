%% This is the application resource file (.app file) for the 'base'
%% application.
{application, svecc_monproxy,
[{description, "siteview ecc monitor proxy" },
{vsn, "1.0" },
{modules, [svecc_monproxyapp]},
{registered,[svecc_monproxyapp]},
{applications, [kernel,stdlib]},
{mod, {svecc_monproxyapp,[]}},
{start_phases, []}
]}.