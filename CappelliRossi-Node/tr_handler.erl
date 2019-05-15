-module(tr_handler).
-export([tr_handler_actor/3]).

% codice gestore transazioni: avvia il miner e aspetta di ricevere messaggi
tr_handler_actor(PID_main, Tr_list, Tr_in_block_list) ->
  io:format("tr_handler_actor ~p started ~n", [self()]),
  process_flag(trap_exit, true),
  Self = self(),
  receive
    {send_pid_bl_handler, Pid_bl_handler} ->
      PID_miner = spawn_link(fun() -> miner:miner_actor(Pid_bl_handler, Self) end),
      loop_tr_handler_actor({PID_main, Pid_bl_handler, PID_miner}, {Tr_list, Tr_in_block_list, []}, waiting, none)
  after 3000 -> exit(ko)
  end.

% loop ricezione messaggi del gestore di transazioni
loop_tr_handler_actor(Attori, Liste_tr, Stato_minatore, ID_blocco_testa) ->
  Self = self(),
  {PID_main, Pid_bl_handler, PID_miner} = Attori,
  {Tr_list, Tr_in_block_list, Tr_minate_da_aggiungere_alla_catena} = Liste_tr,

  receive

    % muore il figlio (miner): lo faccio ripartire  
    {'EXIT', Pid, _} when Pid =:= PID_miner ->
      % io:format("tr_handler_actor: morte del minatore ~n"),
      PID_new_miner = spawn_link(fun() -> miner:miner_actor(Pid_bl_handler, Self) end),
      loop_tr_handler_actor({PID_main, Pid_bl_handler, PID_new_miner}, Liste_tr, Stato_minatore, ID_blocco_testa);

    % muore il padre (main): muore anche l'attore stesso
    {'EXIT', Pid, _} when Pid =:= PID_main ->
      % io:format("tr_handler_actor: morte del main~n"),
      exit(ko);

    % ricezione di una transizione dal main:
    %   - se conosco la transazione riavvio il codice
    %   - se non conosco la transazione la invio al main perché la giri a tutti
    %     poi riavvio il codice inserendo la transazione in coda
    {tr_received, Sender, Tr} when Sender =:= PID_main ->
      case lists:member(Tr, Tr_list) or lists:member(Tr, Tr_in_block_list) of
        true -> 
          % conosco la tr: riavvio il codice
          % io:format("tr conosciuta~n"),
          loop_tr_handler_actor(Attori, Liste_tr, Stato_minatore, ID_blocco_testa);
        false ->
          % io:format("tr sconosciuta: ~p~n", [Tr]),
          New_tr_list = Tr_list ++ [Tr],
          PID_main ! {send_msg_to_all_friends, {push, Tr}},
          case Stato_minatore of
            mining ->
              % io:format("tr sconosciuta: miner attivo~n"),
              loop_tr_handler_actor(Attori, {New_tr_list, Tr_in_block_list, Tr_minate_da_aggiungere_alla_catena}, Stato_minatore, ID_blocco_testa);
            waiting ->
              % io:format("tr sconosciuta: miner in attesa~n"),
              Tr_da_minare = lists:sublist(New_tr_list -- Tr_minate_da_aggiungere_alla_catena, 1, 10),
              % io:format("tr sconosciuta: miner in attesa. Gli invio le tr da minare~n~p~n", [Tr_da_minare]),
              PID_miner ! {tr_list, Self, Tr_da_minare, ID_blocco_testa},
              loop_tr_handler_actor(Attori, {New_tr_list, Tr_in_block_list, Tr_minate_da_aggiungere_alla_catena}, mining, ID_blocco_testa)
          end
      end;

    % ricezione richiesta di tr dal miner: invio al miner le tr
    {give_me_tr, Sender, Tr_ricevute, ID_block} when Sender =:= PID_miner ->
      New_Tr_minate_da_aggiungere_alla_catena = Tr_minate_da_aggiungere_alla_catena ++ Tr_ricevute,
      Tr_da_minare = lists:sublist(Tr_list -- New_Tr_minate_da_aggiungere_alla_catena, 1, 10),
      % io:format("il minatore ha finito.~nTr_minate_da aggiungere_alla_catena: ~n~p~n", [New_Tr_minate_da_aggiungere_alla_catena]),
      case length(Tr_da_minare) =:= 0 of
        true ->
          % io:format("non ci sono tr da minare~n"),
          loop_tr_handler_actor(Attori, {Tr_list, Tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena}, waiting, ID_block);
        false ->
          % io:format("give_me_tr -> New_ID_blocco_testa: ~p, ID_blocco_testa: ~p~n", [ID_block, ID_blocco_testa]),
          PID_miner ! {tr_list, Self, Tr_da_minare, ID_block},
          % io:format("ci sono tr da minare. Le invio al miner:~n~p~n", [Tr_da_minare]),
          loop_tr_handler_actor(Attori, {Tr_list, Tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena}, mining, ID_block)
      end;

    % ricezione di tr da aggiungere a Tr_in_block_list (ed eventualmente da 
    % eliminare da Tr_list) blocco minato da noi
    {update_tr, Sender, Tr_block_added, Bl_head} when Sender =:= Pid_bl_handler ->
      % io:format("arrivato blocco minato da noi~n"),
      New_tr_list = Tr_list -- Tr_block_added,
      New_tr_in_block_list = (Tr_in_block_list -- Tr_block_added) ++ Tr_block_added,
      New_Tr_minate_da_aggiungere_alla_catena = (Tr_minate_da_aggiungere_alla_catena -- Tr_block_added),
      % io:format("Tr_list:~n~p~nBl_tr_list:~n~p~nTr_minate_da_aggiungere_alla_catena:~n~p~n", [New_tr_list, New_tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena]),
      Tr_da_minare = lists:sublist(New_tr_list -- New_Tr_minate_da_aggiungere_alla_catena, 1, 10),
      case length(Tr_da_minare) =:= 0 of
        true ->
          % io:format("non ci sono tr da minare~n"),
          loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena}, Stato_minatore, Bl_head);
        false ->
          case Stato_minatore of
            mining ->
              % io:format("miner attivo~n"),
              loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena}, mining, Bl_head);
            waiting ->
              PID_miner ! {tr_list, Self, Tr_da_minare, Bl_head},
              % io:format("ci sono tr da minare. Le invio al miner:~n~p~n", [Tr_da_minare]),
              loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena}, mining, Bl_head)
          end
      end;
    
    % ricezione di tr da aggiungere a Tr_in_block_list (ed eventualmente da 
    % eliminare da Tr_list) blocco non minato da noi
    {update_tr, Sender, Tr_block_added, Bl_head} when Sender =/= Pid_bl_handler ->
      % io:format("arrivato blocco non minato da noi~n"),
      New_tr_list = Tr_list -- Tr_block_added,
      New_tr_in_block_list = (Tr_in_block_list -- Tr_block_added) ++ Tr_block_added,
      % io:format("Tr_list:~n~p~nBl_tr_list:~n~p~n", [New_tr_list, New_tr_in_block_list]),
      Tr_da_minare = lists:sublist(New_tr_list -- Tr_minate_da_aggiungere_alla_catena, 1, 10),
      case length(Tr_da_minare) =:= 0 of
        true ->
          % io:format("non ci sono tr da minare~n"),
          loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, Tr_minate_da_aggiungere_alla_catena}, Stato_minatore, Bl_head);
        false ->
          case Stato_minatore of
            mining ->
              % io:format("miner attivo~n"),
              loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, Tr_minate_da_aggiungere_alla_catena}, mining, Bl_head);
            waiting ->
              PID_miner ! {tr_list, Self, Tr_da_minare, Bl_head},
              % io:format("ci sono tr da minare. Le invio al miner:~n~p~n", [Tr_da_minare]),
              loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, Tr_minate_da_aggiungere_alla_catena}, mining, Bl_head)
          end
      end;

    {blocco_scartato, Pid_bl_handler, Tr_scartate} ->
      New_Tr_minate_da_aggiungere_alla_catena = (Tr_minate_da_aggiungere_alla_catena -- Tr_scartate),
      % io:format("Blocco Scartato.~nNew_Tr_minate_da_aggiungere_alla_catena:~n~p~n", [New_Tr_minate_da_aggiungere_alla_catena]),
      loop_tr_handler_actor(Attori, {Tr_list, Tr_in_block_list, New_Tr_minate_da_aggiungere_alla_catena}, Stato_minatore, ID_blocco_testa);

    % ricezione di tr dei blocchi da eliminare e di tr dei blocchi da aggiungere
    {update_tr, Sender, Tr_bl_to_add, Tr_bl_to_delete, Bl_head} when Sender =:= Pid_bl_handler ->
      % io:format("tr_handler: ricostruzione catena arrivata~n"),
      New_tr_list = (Tr_list ++ Tr_bl_to_delete) -- Tr_bl_to_add,
      New_tr_in_block_list = ((Tr_in_block_list -- Tr_bl_to_delete) -- Tr_bl_to_add) ++ Tr_bl_to_add,
      % io:format("Tr_list:~n~p~nBl_tr_list:~n~p~n", [New_tr_list, New_tr_in_block_list]),
      loop_tr_handler_actor(Attori, {New_tr_list, New_tr_in_block_list, Tr_minate_da_aggiungere_alla_catena}, Stato_minatore, Bl_head);

    {dead, PID_main} ->
      exit(normal);

    _ -> loop_tr_handler_actor(Attori, Liste_tr, Stato_minatore, ID_blocco_testa)

  end.
