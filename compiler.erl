-module(compiler).
-export([compile_all/0]).
-on_load(compile_all/0).

compile_all() ->
    compile:file(clustering),
    compile:file(debugging),
    compile:file(erl_backend),
    compile:file(image_parser),
    compile:file(json),
    compile:file(partitions),
    compile:file(point_math),
    compile:file(list_processes),
    compile:file(server),
    ok.