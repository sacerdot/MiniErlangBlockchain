-module(transaction_act).

-import(utils, [sendMessage/2]).

-export([start_T_act/2]).

%!%%%%%%%  behavior dell'attore che gestisce le transizioni %%%%%%%%
% Transaction = {ID,Payload}

compute(PidRoot, ListT, PidB) ->
    receive
      {printT} -> compute(PidRoot, ListT, PidB);
      {pushLocal, T, FriendsList} ->
	  % Msg da Root: newTransaction
	  % controllo sulla presenta di T in ListT
	  NewList = case lists:member(T, ListT) of
		      true -> ListT;
		      false ->
			  io:format("Nuova T arrivata: ~p~n", [T]),
			  PidB ! {pushLocal, T},
			  spawn(fun () -> sendAll(T, FriendsList) end),
			  ListT ++ [T]
		    end,
	  compute(PidRoot, NewList, PidB)
    end.

sendAll(T, FriendsList) ->
    % gossiping della nuova transaction
    [sendMessage(X, {push, T}) || X <- FriendsList].

start_T_act(PidRoot, PidB) ->
    compute(PidRoot, [], PidB).
