-module(es_command_handler).

-callback apply_command(Command :: map()) ->
    {Status :: atom(), ReplayStrategy :: atom(), Event :: map()}.
