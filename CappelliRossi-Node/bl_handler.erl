-module(bl_handler).
-import(proof_of_work , [check/2]).
-import(ch_rec_handler, [chain_reconstruction_actor/5]).
-export([bl_handler_actor/3, find_BlockE/2]).

% codice gestore transazioni: avvia il miner e aspetta di ricevere messaggi
bl_handler_actor(PID_main, PID_tr_handler, Bl_list) ->
  io:format("bl_handler_actor ~p started ~n", [self()]),
  process_flag(trap_exit, true),
  PID_get_previous = spawn_link(fun() -> gp_handler:gp_actor([]) end),
  loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list).

% loop ricezione messaggi del gestore di transazioni
loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list) ->
  Self = self(), 

  receive

    % muore il figlio (get_previous): lo faccio ripartire  
    {'EXIT', Pid, _} when Pid =:= PID_get_previous ->
      % io:format("bl_handler_actor: morte di get_previous_handler ~n"),
      PID_new_get_previous = spawn_link(fun() -> gp_handler:gp_actor([]) end),
      loop_bl_handler_actor(PID_main, PID_new_get_previous, PID_tr_handler, Bl_list);

    % muore il padre (main): muore anche l'attore stesso
    {'EXIT', Pid, _} when Pid =:= PID_main ->
      % io:format("tr_handler_actor: morte del main~n"),
      exit(ko);

    % ricezione messaggio get_previous: rispondo subito se conosco il blocco,
    % altrimenti inviamo la richiesta al gestore delle richieste get_previous
    {get_previous, Mittente, Nonce, ID_blocco} ->
      case find_BlockE(ID_blocco, Bl_list) of
        false ->
          % blocco sconosciuto
          % io:format("bl_hanlder riceve get_previous con blocco sconosciuto~n"),
          PID_get_previous ! {get_previous, Mittente, Nonce, ID_blocco},
          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list);
        Blocco ->
          % blocco conosciuto
          % io:format("bl_hanlder riceve get_previous con blocco conosciuto~n"),
          Mittente ! {previous, Nonce, Blocco},
          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list)
      end;

    % ricezione messaggio get_head: restituisco il blocco in testa allo stack
    {get_head, Mittente, Nonce} ->
      % io:format("bl_handler: ricezione get_head~n"),
      Blocco_testa = retreive_blocco_testa(Bl_list),
      Mittente ! {head, Nonce, Blocco_testa},
      loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list);

    % ricezione di un blocco minato da me
    {update, Sender, Block} when Sender =:= Self->
      {Id_bl, ID_previous_bl, Block_tr, _} = Block,
      % io:format("Blocco: ~p~n", [Block]),
      % capisco se le tr del blocco sono gia presenti in altri blocchi
      Tr_bl_list = lists:flatten([Tr || {_,_,Tr,_} <- Bl_list]),
      case length(Tr_bl_list -- Block_tr) =:= length(Tr_bl_list) of
        true ->
          % io:format("Accetto il mio blocco~n"),
          PID_main ! {send_msg_to_all_friends, {update, PID_main, Block}},
          % controllo se posso aggiungere il blocco in testa
          case retreive_ID_blocco_testa(Bl_list) =:= ID_previous_bl of
            true ->
              % io:format("mio bl: aggiunto in testa. Bl_list:~n~p~n~n", [[Block] ++ Bl_list]),
              PID_tr_handler ! {update_tr, Self, Block_tr, Id_bl},
              loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, [Block] ++ Bl_list);
            false ->
              % io:format("scarto il blocco mio~n"),
              PID_tr_handler ! {blocco_scartato, Self, Block_tr},
              loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list)
          end;
        false ->
          % il blocco che ho minato è da scartare perché contiene tr gia
          % presenti in Tr_bl_list. Rimetto le sue tr in tr_list e
          % scarto il blocco
          % io:format("scarto il blocco mio~n"),
          PID_tr_handler ! {blocco_scartato, Self, Block_tr},
          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list)
      end;


    % ricezione di un blocco: verifico se il blocco è già nella nostra catena,
    % in caso affermativo lo scarto altrimenti capisco cosa farne.
    {update, Sender, Block} ->
      case Block of
        {ID_bl, ID_previous_bl, Block_tr, Solution} ->
          case lists:member(Block, Bl_list) of 
            true ->
              % io:format("bl_handler: blocco conosciuto~n"),
              loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list);
            false ->
              % Ricevuto blocco sconosciuto: verifico che il blocco sia corretto.
              % In caso affermativo decido se aggiungere il blocco in testa, 
              % scartare il blocco o avviare l'attore per la ricostruzione della
              % catena.
              % io:format("bl_handler: blocco sconosciuto~n~p~n", [Block]),
              case check({ID_previous_bl, Block_tr}, Solution) of
                true ->
                  % blocco corretto: lo invio al gp_handler e a tutti gli amici
                  % io:format("check ok~n"),
                  PID_get_previous ! {block_added, Block},
                  PID_main ! {send_msg_to_all_friends, {update, PID_main, Block}},
                  % Confronto l'id dell'ultimo blocco della catena con il
                  % predecessore del blocco arrivato: in caso affermativo
                  % aggiungo il blocco in testa alla bl_list, in caso negativo
                  % valuto se ricostruire la catena o scartare il blocco
                  case retreive_ID_blocco_testa(Bl_list) =:= ID_previous_bl of
                    true ->
                      % aggiorno le liste di tr e aggiungo il blocco in testa
                      % io:format("bl aggiunto in testa:~n~p~n~n", [[Block] ++ Bl_list]),
                      PID_tr_handler ! {update_tr, Sender, Block_tr, ID_bl},
                      loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, [Block] ++ Bl_list);
                    false ->
                      % controllo se id_previous_bl è none o l'id di un blocco
                      % di Bl_list. In caso affermativo Block è da scartare, 
                      % in caso negativo occorre ricostruire la catena
                      % io:format("il blocco non si può aggiungere in testa~n"),
                      case (ID_previous_bl =:= none) or (length([ID_block || {ID_block,_,_,_} <- Bl_list, ID_block =:= ID_previous_bl]) =/= 0) of
                        true ->
                          % io:format("il blocco e' da scartare~n~n"),
                          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list);
                        false ->
                          % avvio un attore per la ricostruzione della catena
                          % io:format("bl_handler: ricostruzione catena iniziata~n~n"),
                          spawn(fun() -> chain_reconstruction_actor(Self, PID_get_previous, Sender, Block, Bl_list) end),
                          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list)
                      end
                  end;
                false ->
                  % io:format("check fallito~n"),
                  loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list)
              end
          end
      end;

    % ricezione di una nuova catena che portebbe sostituire la nostra
    {new_chain, New_bl_list, Tr_bl_to_add, Tr_bl_to_delete} ->
      case length(New_bl_list) < length(Bl_list) of
        true ->
          % io:format("bl_handler: La catena ricostruita è più corta di quella attuale~n"),
          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list);
        false ->
          % io:format("bl_handler: ricevuta nuova catena più lunga di quella attuale~n"),
          % invio al gestore delle tr le tr dei blocchi da scartare e le tr dei
          % blocchi da aggiungere
          PID_tr_handler ! {update_tr, Self, Tr_bl_to_add, Tr_bl_to_delete, retreive_ID_blocco_testa(New_bl_list)},
          % io:format("bl_handler: catena ricostruita:~n~p~n", [New_bl_list]),
          loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, New_bl_list)
      end;

    {dead, PID_main} ->
      exit(normal);

    _ -> loop_bl_handler_actor(PID_main, PID_get_previous, PID_tr_handler, Bl_list)

  end.






% ============ ricerca ID blocco_testa e ricerca blocco_testa =================

% identificazione dell'id dell'ultimo blocco inserito nella lista
retreive_ID_blocco_testa(Bl_list) ->
  case Bl_list of
    [] -> none;
    _ ->
      [{ID_blocco_testa, _, _, _} | _]  = Bl_list,
      ID_blocco_testa
  end.
% identificazione dell'ultimo blocco inserito nella lista
retreive_blocco_testa(Bl_list) ->
  case Bl_list of
    [] -> none;
    _ ->
      [Block | _]  = Bl_list,
      Block
  end.




% ======== ricerca di un blocco nella blockchain dato il suo ID ===============

% caso: ricerca di un blocco nella blockchain vuota
find_BlockR(_, BlockChain) when length(BlockChain) =:= 0 ->
  false;
% caso: ricerca di un blocco nella blockchain non vuota
find_BlockR(ID_Block_to_find, BlockChain) ->
  [{ID_first_block, ID_pb, Tr, Sol} | Tail]  = BlockChain,
  case ID_Block_to_find =:= ID_first_block of
    true ->
      {ID_first_block, ID_pb, Tr, Sol};
    false ->
      find_BlockR(ID_Block_to_find, Tail)
  end.
% funzione con eccezioni
find_BlockE(ID_Block_to_find, BlockChain) ->
  try
    find_BlockR(ID_Block_to_find, BlockChain)
  catch
    Blocco -> Blocco
  end.
