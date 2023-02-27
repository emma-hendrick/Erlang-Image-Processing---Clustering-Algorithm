-module(serial_communication).
-export([initialize_serial/0]).

initialize_serial() ->
    Env_vars = env:initialize_env(),
    Device_name = env:get_var(Env_vars, "DEVICE_NAME"),
    Serial_driver_path = env:get_var(Env_vars, "ERLANG_SERIAL_DRIVER_PATH"),
    {Device_name, Serial_driver_path}.
    