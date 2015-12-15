-module(es_command_handler).

-callback handle_command(Command :: map()) ->
    {Status :: atom(), ReplayStrategy :: atom(), Event :: map()}.
