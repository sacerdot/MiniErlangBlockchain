-module(transaction_act).
-export([start_T_act/2]).
%%%%%%%%  behavior dell'attore che gestisce le transizioni %%%%%%%%

% TODO: qui mettere random su invio, invio doppio, non invio


% Transaction = {ID.Payload}

compute(PidRoot,ListT,PidB) -> 
    receive
        {printT} -> 
            io:format("[~p] Le mie transazioni sono: ~p~n",[PidRoot,ListT]),
            compute(PidRoot,ListT,PidB);
        {pushLocal, T, FriendsList} ->
            % controllo sulla presenta di T in ListT
            NewList = case lists:member(T,ListT) of
                true -> ListT;
                false ->
                    % io:format("--> Ho ricevuto una T (~p) locale dal RootAct: ~p~n",[T,PidRoot]),
                    sendAll(T,FriendsList),
                    send_T_to_BlockAct(T,PidB),
                    [T] ++ ListT
            end,            
            compute(PidRoot,NewList,PidB) 
    end.

sendAll(T, FriendsList) ->
    % TODO: qui mettere random su invio, invio doppio, non invio
    [ X ! {push, T} || X <- FriendsList]. % gossiping della nuova transaction

send_T_to_BlockAct(T, PidB) ->
    PidB ! {new_transaction,T}.    

start_T_act(PidRoot,PidB) -> 
    % io:format("[~p]: sono l'ttore Transazione di ~p~n",[self(),PidRoot]),
    compute(PidRoot, [],PidB).
   