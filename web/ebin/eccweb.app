{application, eccweb,
 [{description, "siteview ecc web"},
  {vsn, "0.01"},
  {modules, [
    eccweb,
    eccweb_sup,
    eccweb_main,
    sample
  ]},
  {registered, []},
  {mod, {eccweb, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
