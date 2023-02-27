-module(compiler).
-export([compile_all/0]).
-on_load(compile_all/0).

compile_all() ->
    compile:file(clustering),
    compile:file(debugging),
    compile:file(erl_backend),
    compile:file(image_parser),
    compile:file(json),
    compile:file(point_math),
    compile:file(processes),
    compile:file(server),
    compile:file(serial_communication),
    compile:file(env),
    ok.