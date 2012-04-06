{application, wmi,
    [
        {description, "wmi"},
        {vsn, "1.0.0"},
        {modules, 
            [
                wmiproxy,
		wmi
            ]
        },
        {registered, [wmi]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {wmi, []}},
        {env, []}
    ]
}.



