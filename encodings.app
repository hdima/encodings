{application, encodings,
    [{description, "Encodings"},
     {vsn, "0.2"},
     {modules, [
        encodings_app,
        encodings,
        enc_ascii,
        enc_iso8859_1,
        enc_cp1251
        ]},
     {registered, [encodings]},
     {applications, [kernel, stdlib]}
     ]}.
