{application, exago,
 [{description, "exago"},
  {vsn, "0.01"},
  {modules, [
    exago_event,
    exago_field,
    exago_parser,
    exago_printer,
    exago_state_machine,
    exago_user
    exago_util
    exago_examples
  ]},
  {registered, []},
  {mod, {exago_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
