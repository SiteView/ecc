{application, content,
    [
        {description, "content store db"},
        {vsn, "0.0.1"},
        {modules, 
            [
                kvs_common,
		kvs_config,
		kvs_cache,
		kvs_storage,
		kvs_table,
                kvs_content
            ]
        },
        {registered, [content]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {kvs_main, []}},
        {env, []}
    ]
}.



