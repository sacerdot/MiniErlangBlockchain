-module(transaction_act).
-export([start_T_act/2]).
-import (utils , [sendMessage/2]).

%!%%%%%%%  behavior dell'attore che gestisce le transizioni %%%%%%%%
% Transaction = {ID,Payload}


compute(PidRoot,ListT,PidB) -> 
    receive
        % * DEBUG 
        {printT} -> 
            io:format("[~p] Le mie transazioni sono: ~p~n",[PidRoot,ListT]),
            compute(PidRoot,ListT,PidB);
        % * Msg da Root: newTransaction
        {pushLocal, T, FriendsList} ->
            % controllo sulla presenta di T in ListT
            NewList = case lists:member(T,ListT) of
                true -> ListT;
                false ->
                    PidB ! {pushLocal, T},
                    % io:format("--> Ho ricevuto una T (~p) locale dal RootAct: ~p~n",[T,PidRoot]),
                    spawn(fun() -> 
                        sendAll(T,FriendsList)
                    end),                    
                    [T] ++ ListT
            end,            
            compute(PidRoot,NewList,PidB) 
    end.

sendAll(T, FriendsList) ->
    [ sendMessage(X,{push, T}) || X <- FriendsList]. % gossiping della nuova transaction
     
start_T_act(PidRoot,PidB) -> 
    % io:format("[~p]: sono l'ttore Transazione di ~p~n",[self(),PidRoot]),
    compute(PidRoot, [],PidB).
   