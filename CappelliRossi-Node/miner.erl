-module(miner).
-import(proof_of_work , [solve/1]).
-import(main , [sleep/1]).
-export([miner_actor/2]).

% codice del Minatore:
% si invia al gestore delle transazioni una richiesta per ricevere le transazioni
% da minare. Se arriva la lista vuota si attende qualche secondo e si rilancia il 
% codice. Se arrivano transazioni le si mina, si invia al main il blocco minato
% e si riesegue il codice.
miner_actor(Pid_attore_bl, PID_attore_tr) ->
  % io:format("miner ~p started ~n", [self()]),

  receive

    % ricevo la lista vuota: attendo 3s e riavvio il miner 
    % {tr_list, Sender, [], _} when Sender =:= PID_attore_tr ->
    %   % io:format("miner riceve lista vuota~n"),
    %   sleep(1),
    %   miner_actor(Pid_attore_bl, PID_attore_tr);

    % ricevo almeno una transazione: inizio a minare e invio il blocco al main.
    % poi riavvio il miner
    {tr_list, Sender, Tr_list, ID_blocco_testa} when Sender =:= PID_attore_tr ->
      % io:format("miner riceve lista~p~n", [Tr_list]),
      Soluzione = solve({ID_blocco_testa, Tr_list}),
      % invio richiesta transazioni da minare
      ID_block = make_ref(),
      PID_attore_tr ! {give_me_tr, self(), Tr_list, ID_block},
      Pid_attore_bl ! {update, Pid_attore_bl, {ID_block, ID_blocco_testa, Tr_list, Soluzione}},
      miner_actor(Pid_attore_bl, PID_attore_tr)

  end.
