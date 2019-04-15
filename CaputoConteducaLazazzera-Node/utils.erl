-module(utils).
-export([sendMessage/2]).
-define(RANDOM, 1).


sendMessage(Dest,Message) ->
    X = rand:uniform(?RANDOM),
    case X of
        2 ->
            Dest ! Message,
            Dest ! Message;
        3 ->
            none;
        _ -> Dest ! Message
    end.
