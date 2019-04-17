-module(utils).
-export([sendMessage/2,sleep/1]).
-define(RANDOM, 1).

sleep(N) -> receive after N*1000 -> ok end.

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
