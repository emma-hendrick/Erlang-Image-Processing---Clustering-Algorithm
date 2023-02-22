-module(server).
-export([start/1]).


%% Spawn a TCP listener on given port
start(Port) ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]), 
        loop(Sock) end).


%% Accept all incoming connections
loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),

    %% Spawn a handler to handle incoming data
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),

    %% Recurse
    loop(Sock).


%% Handle incoming data
handle(Conn) ->

    %% Send data in JSON format
    gen_tcp:send(Conn, response(
        json:serialize([
            {"fName", "Joe"},
            {"lName", "Doe"},
            {"Age", "11"}
        ])
    )),
    gen_tcp:close(Conn).


%% Respond to client
response(Json) ->

    %% Convert the JSON message to binary, so it can be sent in the body of the HTTP response
    B = iolist_to_binary(Json),

    %% HTTP response
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.1 200 OK\n" ++ 
            "Content-Type: application/json\n" ++
            "Content-Length: " ++
            "~p\n\n~s",
            [size(B), B])).


