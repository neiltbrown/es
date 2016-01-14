-module(es_command_handler).

-callback handle_command(Command :: map()) ->
    {ReplayStrategy :: atom(), Event :: map()}.
