
erl -env ExtensionMainNode debug@itsm -sname tt -setcookie 3ren -pa %cd%\ebin\ -eval "extension_node:findPluginsRegistToMainNode(),extension_node:global_register_name()."

