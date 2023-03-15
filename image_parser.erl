-module(image_parser).
-export([sample_test_image/2, sample_image/2]).

% Loads and samples the image
sample_test_image(Filename, Sample_size) ->
    wx:new(),
    Image = wxImage:new(Filename),
    io:format("Name: ~p~n", [Filename]),
    {Sample, _Percent} = getSample(Image, Sample_size),
    lists:filter(fun(X) -> X =/= {0, 0, 0} end, Sample),
    wxImage:saveFile(Image, "chosen_pixels.png"),
    Sample.

% Loads and samples the REAL image
sample_image(Body, Sample_size) ->
    wx:new(),
    Image = wxImage:new({120, 90}),
    wxImage:setData(Image, Body),
    io:format("Name: ~p~n", ["Image"]),
    {Sample, _Percent} = getSample(Image, Sample_size),
    lists:filter(fun(X) -> X =/= {0, 0, 0} end, Sample),
    wxImage:saveFile(Image, "chosen_pixels.png"),
    Sample.

% Gets the size
getSize(Image) ->
    Width = wxImage:getWidth(Image),
    Height = wxImage:getHeight(Image),

    io:format("Width: ~p Height: ~p~n", [Width, Height]),
    {Width, Height}.

% Normalizes rgb values
normalize(Value) when Value > 255 ->
    (Value - 4294967040);
normalize(Value) -> Value.

% Get a specific pixel from the image
getPixel(Image, X, Y) -> 
    R = normalize(wxImage:getRed(Image, X, Y)),
    G = normalize(wxImage:getGreen(Image, X, Y)),
    B = normalize(wxImage:getBlue(Image, X, Y)),
    wxImage:setRGB(Image, X, Y, 255, 0, 0),

    if
        {R, G, B} =/= {0, 0, 0} ->
            file:write_file("pixels.csv", io_lib:format("~p, ~p, ~p\n", [R, G, B]), [append]);
        true ->
            ok
    end,

    {R, G, B}.

% Get an approximate number of pixels, spread evenly
getSample(Image, Count) ->
    file:write_file("pixels.csv", "", [write]),
    {Width, Height} = getSize(Image),
    {Sample, Error} = getSample(Image, Width, Height, Count),
    io:format("Percent: ~p Count: ~p Length: ~p~n", [length(Sample)/Count, Count, length(Sample)]),
    {Sample, Error}.

getSample(Image, Width, Height, Count) when Count =< 0 ->
    io:format("Count: ~p ~n", [Count]),
    {Sample, _} = getSample(Image, 0, 0, Width, Height, 1, []),
    {Sample, 1};
getSample(Image, Width, Height, Count) when Count >= Width * Height ->
    io:format("Count: ~p ~n", [Count]),
    getSample(Image, Width, Height, 0);
getSample(Image, Width, Height, Count) ->
    Interval = math:sqrt((Width * Height) / Count),
    X = Interval,
    Y = Interval,
    io:format("Count: ~p X:~p Y:~p~n", [Count, X, Y]),
    {Sample, Error} = getSample(Image, X, Y, Width, Height, Interval, []),
    {Sample, Error}.

getSample(_Image, _X, Y, Width, Height, Interval, Acc) when Y >= Height ->
    Error = length(Acc) / ((Width * Height) / math:pow(Interval, 2)),
    {Acc, Error};
getSample(Image, X, Y, Width, Height, Interval, Acc) when X >= Width ->
    getSample(Image, X-Width, Y+Interval, Width, Height, Interval, Acc);
getSample(Image, X, Y, Width, Height, Interval, Acc) ->
    Pixel = getPixel(Image, floor(X), floor(Y)),
    getSample(Image, X+Interval, Y, Width, Height, Interval, [Pixel|Acc]).