{application, content_store,
    [
        {description, "content store db"},
        {vsn, "0.0.1"},
        {modules, 
            [
                common,
                condition,
                operator,
                content,
                app,
                profile
            ]
        },
        {registered, [content_store]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {content_store, []}},
        {env, []}
    ]
}.



