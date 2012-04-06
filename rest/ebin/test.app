{application, test,
 [{description, "test"},
  {vsn, "0.01"},
  {modules, [
    test,
    test_app,
    test_sup,
    test_web,
    test_deps
  ]},
  {registered, []},
  {mod, {test_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
