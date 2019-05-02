-module(miner).
-export([call_miner/3, miner_restarter/4, find_transaction_in_block_list/2, miner_main/3]).

% caso: ricerca di una transazione in un blocco della blockchain vuota
find_transaction(_, BlockChain) when length(BlockChain) =:= 0 ->
  false;
% caso: ricerca di una transazione in un blocco della blockchain
find_transaction(Transazione, BlockChain) ->
  [{_, _, Transactions_list, _} | Tail]  = BlockChain,
  case lists:member(Transazione, Transactions_list) of
    true -> true;
    false -> find_transaction(Transazione, Tail)
  end.
% funzione con eccezioni per la ricerca di una transazione nella blockchain, se
% la trova restituisce true, false altrimenti
find_transaction_in_block_list(Transazione, BlockChain) ->
  try
    find_transaction(Transazione, BlockChain)
  catch
    true -> true
  end.

% funzione che attiva un minatore e lo registra. Input necessari: PID  
% dell'attore principale, blockchain e lista di transazioni da minare
call_miner(Main_actor_Pid, Blockchain, Transactions_list) ->
  M = spawn(fun() -> miner_main(Main_actor_Pid,
    mainActorCR:retreive_ID_blocco_testa(Blockchain),Transactions_list)
  end),
  register(minerCR, M),
  io:format("Miner ~p created~n", [M]).

% funzione che verifica se il minatore sta minando più o meno di 10 transazioni:
% se ne sta minando di meno occorre stopparlo e farlo ripartire, se ne sta
% minando di più non si fa nulla
miner_restarter(PID_miner, Main_actor_Pid, BlockChain, Transactions_list) ->
  case length(Transactions_list) =< 10 of
    true ->
      exit(PID_miner,kill),
      io:format("Miner killed~n"),
      mainActorCR:sleep(0.5),
      call_miner(Main_actor_Pid,BlockChain,Transactions_list);
    false -> ignore
  end.

% MAIN DEL MINATORE
% * input:
%   - pid attore principale,
%   - id ultimo blocco della lista
%   - lista di transazioni da minare
% * codice: se la lista di transazioni contiene almeno una transazione, il
%     minatore inizia a minare chiamando proof_of_work:solve({...}).
%     Nel momento in cui l'operazione termina con successo, si crea il blocco e
%     lo si gira all'attore principale. Terminata la computazione, il minatore
%     termina con successo.
miner_main(Pid_attore_principale, ID_blocco_testa, Transactions_list) ->
  case length(Transactions_list) > 0 of
    true ->
      Soluzione = proof_of_work:solve({ID_blocco_testa, Transactions_list}),
      % io:format("Pid attore principale: ~p~n", [Pid_attore_principale]),
      Pid_attore_principale ! {update, self(),{make_ref(), ID_blocco_testa, Transactions_list, Soluzione}},
      io:format("Blocco minato con successo~n");
    false ->
      ignore
  end.