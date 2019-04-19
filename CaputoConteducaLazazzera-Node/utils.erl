-module(utils).
-export([sendMessage/2,sleep/1,formatChain/1]).
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


formatChain(Chain) ->
    StringFinal = lists:map(
        fun(X)-> 
            {ID, ID_Prev, LisT,_} = X, 
            "-----------------------------------------------\nID_BLOCK: " ++ 
            to_string(ID) ++ "\n\n" ++ 
            stringTransaction(LisT) ++ "ID_PREV: " ++ 
            to_string(ID_Prev)  ++
            "\n-----------------------------------------------" 
        end,Chain), StringFinal.


to_string(StringConvert) ->
        R = io_lib:format("~p",[StringConvert]),
        lists:flatten(R).
stringTransaction(T) ->
        StringT = lists:map(
            fun(X) -> 
                {ID_T, Payload} = X,  
                "> [" ++ to_string(ID_T) ++ "] " ++ Payload ++ "\n"
            end, T), StringT.