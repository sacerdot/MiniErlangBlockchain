-module(main).
-export([main/0, sleep/1]).
-on_load(compileFiles/0).

% Uso:
% c(main).
% main:main()

% ====================== FUNZIONI DI LIBRERIA =================================

%funzione chiamata come prima istruzione del processo principale per compilare
% tutti i file degli attori ausiliari
compileFiles() ->
  compile:file('friends_library.erl'),
  compile:file('miner.erl'),
  compile:file('tr_handler.erl'),
  compile:file('bl_handler.erl'),
  compile:file('gp_handler.erl'),
  compile:file('ch_rec_handler.erl'),
  compile:file('../proof_of_work.erl'),
  ok.

sleep(N) -> receive after N*1000 -> ok end.

% Invio un messaggio a tutti gli amici con inserimento di probabilità di perdere
% il messaggio o di inviarlo due volte
sendMessageToAllFriends(Msg,Friends_list) ->
  lists:foreach(fun(X) ->
    case rand:uniform(10) of
      1 -> ignore;
      2 -> X ! Msg, X ! Msg;
      _ -> X ! Msg
    end
                end, Friends_list).

main_attore_get_head(Blh, Friend) ->
  %io:format("Amico ~p~n", [Friend]),
  Nonce = make_ref(),
  Friend ! {get_head, self(), Nonce},
  receive
    {head, Nonce, none} ->
      exit(normal);
    {head, Nonce, Block} ->
    	%io:format("Ricevuto head ~p~n", [Blh]),
      Blh ! {update, Friend, Block}
  after 10000 -> exit(normal)
  end.

% ======================== LOOP ===============================================
loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list) ->
  %sleep(1),
  Self = self(),
  {Trh, Blh, Af, Cn, Cl} = Actors_list,
  %%io:format("Loop started - node ~p~n", [Self]),
  %io:format("Node ~p -> Friends_list ~p~n", [Self, Friends_list]),
  %io:format("Node ~p -> Friends_list_ask ~p~n", [Self, Friends_list_ask]),

  receive

  % ricezione di un messaggio ping: rispondiamo con pong
    {ping, Sender, Nonce} ->
      %%io:format("Node ~p -> ping from ~p~n", [Self, Sender]),
      Sender ! {pong, Nonce},
      %%io:format("Node ~p -> pong to ~p~n", [Self, Sender]),
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);


  % ricezione di un messaggio exit a casusa della morte di un nostro figlio
    {'EXIT', Pid, Msg} ->
      %%io:format("Morte di ~p, msg = ~p~n", [Pid, Msg]),
      case Pid of

      	Trh when Msg =/= normal ->
      		%%io:format("Muore ~p, msg = ~p~n", [Pid, Msg]),
      		Blh ! {dead, Self},
				  New_trh = spawn_link(fun() -> tr_handler:tr_handler_actor(Self, [], []) end),
 					New_blh = spawn_link(fun() -> bl_handler:bl_handler_actor(Self, New_trh, []) end),
  				New_trh ! {send_pid_bl_handler, New_blh},
  				%spawn(fun() -> main_attore_get_head(New_blh, Friends_list) end),
          [spawn(fun() -> main_attore_get_head(New_blh, F) end) || F <- Friends_list],
  				loop(Friends_list, Friends_list_ask, Watcher_list, {New_trh, New_blh, Af, Cn, Cl});

      	Blh when Msg =/= normal->
      		%%io:format("Muore ~p, msg = ~p~n", [Pid, Msg]),
					Trh ! {dead, Self},
				  New_trh = spawn_link(fun() -> tr_handler:tr_handler_actor(Self, [], []) end),
 					New_blh = spawn_link(fun() -> bl_handler:bl_handler_actor(Self, New_trh, []) end),
  				New_trh ! {send_pid_bl_handler, New_blh},
  				[spawn(fun() -> main_attore_get_head(New_blh, F) end) || F <- Friends_list],
  				loop(Friends_list, Friends_list_ask, Watcher_list, {New_trh, New_blh, Af, Cn, Cl});

  			Af ->
      		%%io:format("Muore af ~p, msg = ~p~n", [Pid, Msg]),
          New_af = spawn_link(fun() -> friends_library:adder_friends(Self) end),
          register(adder_friends_CR, New_af),
          loop(Friends_list, Friends_list_ask, Watcher_list, {Trh, Blh, New_af, Cn, Cl});

      	Cl ->
      		%%io:format("Muore cl ~p, msg = ~p~n", [Pid, Msg]),
          New_cl = spawn_link(fun() -> friends_library:checker_list(Self) end),
          register(checker_list_CR, New_cl),
          loop(Friends_list, Friends_list_ask, Watcher_list, {Trh, Blh, Af, Cn, New_cl});

      	Cn ->
      		%%io:format("Muore cn ~p, msg = ~p~n", [Pid, Msg]),
          New_cn = spawn_link(fun() -> friends_library:checker_Nonce(Self, []) end),
					register(checker_nonce_CR, New_cn),
          loop(Friends_list, Friends_list_ask, Watcher_list, {Trh, Blh, Af, New_cn, Cl});

  			_ when Msg =/= normal ->
          %io:format("~n~n~nSono normal~n~n~n"),
  				Friend_list = [ Y || {_,Y,X} <- Watcher_list, Pid =:= X],
          case length(Friend_list) =/= 0 of
            true ->
              [Friend | _] = Friend_list,
              %io:format("~n~n~nMorte di Watcher ~p. Riavvio del figlio in corso...~n~n~n", [Pid]),
              W = spawn_link(fun() -> friends_library:watch(Self, Friend) end),
              loop(Friends_list, Friends_list_ask, [{watcher, Friend, W}] ++ Watcher_list -- [{watcher, Friend, Pid}], Actors_list);
            false -> 
            	loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
          end;

  			_ ->
          %io:format("~n~n~nSono qui~n~n~n"),
  				loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)

  		end;


  % ricezione di un messaggio get_friends da parte di un attore
  % SITUAZIONE: un attore ha bisogno di amici e ci chiede la nostra lista di amici
  % COSA FARE: verifichiamo se lo abbiamo come amico, se si gli inviamo la nostra lista tramite un messaggio
  % friends, altrimenti prima lo aggiungiamo alla nostra lista di amici e poi gliela inviamo
    {get_friends, Sender, Nonce} ->
      %%io:format("Node ~p -> get_friends from ~p~n", [Self, Sender]),
      {New_friends_list, New_friends_list_ask, New_Watcher_list} =
        case lists:member(Sender, Friends_list) of
          true ->
            {Friends_list, Friends_list_ask, Watcher_list};
          false ->
            case length(Friends_list) < 3 of
              true ->
                %%io:format("Node ~p -> New friend ~p~n",[Self, Sender]),
                W = spawn_link(fun() -> friends_library:watch(Self, Sender) end),
                %io:format("~n~n~nNode ~p -> New watcher ~p~n~n~n",[Self, W]),
                {[Sender] ++ Friends_list, [Sender] ++ Friends_list, [{watcher, Sender, W}] ++ Watcher_list};
              false ->
                {Friends_list, Friends_list_ask, Watcher_list}
            end
        end,
      friends_library:sendMessageToFriend({friends, Nonce, New_friends_list}, Sender),
      %%io:format("Node ~p -> friends to ~p~n", [Self, Sender]),
      loop(New_friends_list, New_friends_list_ask, New_Watcher_list, Actors_list);


  % ricezione di un messaggio friends da parte di un nostro amico
  % SITUAZIONE: precedentemente abbiamo chiesto la lista di amici ad un nostro amico e lui ce la invia
  % COSA FARE: il nostro amico ci invia la sua lista di amici ma prima di gestirla verifichiamo che il messaggio che
  % ci ha inviato abbia lo stesso Nonce del messaggio di richiesta di amici get_friends inviatogli precedentemente
  % così da evitare messaggi disonesti
    {friends, Nonce, Friends_list_received} ->
      case whereis(checker_nonce_CR) of
        undefined -> ignore;
        _ ->
          checker_nonce_CR ! {check_nonce, Self, Nonce, Friends_list_received}
      end,
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);


  % ricezione di un messaggio nonce_checked da parte dell'attore secondario checker_nonce
  % SITUAZIONE: il Nonce tra i messaggi get_friends e friends scambiati con un nostro amico corrispondono
  % e veniamo notificati di questo dal nostro attore scondario
  % COSA FARE: si aggiungono gli amici presenti nella lista inviataci da un nostro amico che noi non abbiamo
  % alla nostra lista di amici
    {nonce_checked, Sender, Friends_list_received} ->
      case Sender =:= whereis(checker_nonce_CR) of
        true ->
          %%io:format("Node ~p -> receive friends~n", [Self]),
          case length(Friends_list) < 3 of
            true ->
              List_tmp = ((Friends_list_received -- [Self]) -- Friends_list),
              case whereis(adder_friends_CR) of
                undefined -> ignore;
                _ ->
                  adder_friends_CR ! {add_friends, Self, List_tmp, Friends_list, Friends_list_ask}
              end,
              loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);
            false ->
              loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
          end;
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;


  % ricezione di un messaggio friends_added da parte dell'attore secondario adder_friends
  % SITUAZIONE: l'attore secondario adder_friends ha aggiornato la nostra lista di amici
  % COSA FARE: si controlla il numero di amici presenti nella lista,
  % -se sono 3 si ha raggiunto il numero massimo di amici
  % -se non si ha nessun amico si richiede la lista al nodo professore
  % -se sono meno di 3 si richiede la lista di amici ad un nostro amico random o se nessuno dei nostri amici ci
  % permette di arrivare a 3 si manda un messaggio al nodo professore
    {friends_added, Sender, New_friends_list, New_friends_list_ask} ->
      %io:format("Ricevuto friends_added~n~n~n"),
      case Sender =:= whereis(adder_friends_CR) of
        true ->
          %%io:format("Node ~p -> Added friends~n", [Self]),
          case length(New_friends_list) =:= 3 of
            true ->
            	%spawn(fun() -> main_attore_get_head(Blh, New_friends_list -- Friends_list) end),
              New_watcher_list = [{watcher, F, spawn_link(fun() -> friends_library:watch(Self, F) end)} || F <- (New_friends_list -- Friends_list)],
              [spawn(fun() -> main_attore_get_head(Blh, F) end) || F <- (New_friends_list -- Friends_list)],
              loop(New_friends_list, New_friends_list_ask, New_watcher_list ++ Watcher_list, Actors_list);
            false ->
              case length(New_friends_list) =:= 0 of
                true ->
                  case global:whereis_name(teacher_node) of
                    undefined -> ignore;
                    %%io:format("Il prof non c'è ~n");
                    _ ->
                      friends_library:sendMessageToTeacher({get_friends, Self, make_ref()})
                  end,
                  loop(New_friends_list, New_friends_list_ask, Watcher_list, Actors_list);
                false ->
                  case length(New_friends_list_ask) =:= 0 of
                    true ->
                      case global:whereis_name(teacher_node) of
                        undefined -> ignore;
                        %%io:format("Il prof non c'è ~n");
                        _ ->
                          friends_library:sendMessageToTeacher({get_friends, Self, make_ref()})
                      end,
                      loop(New_friends_list, New_friends_list_ask, Watcher_list, Actors_list);
                    false ->
                      New_watcher_list = [{watcher, F, spawn_link(fun() -> friends_library:watch(Self, F) end)} || F <- (New_friends_list -- Friends_list)],
                      [spawn(fun() -> main_attore_get_head(Blh, F) end) || F <- (New_friends_list -- Friends_list)],
                      Random_friend = lists:nth(rand:uniform(length(New_friends_list)), New_friends_list),
                      Send = friends_library:sendMessageToFriend({get_friends, Self, make_ref()}, Random_friend),
                      %%io:format("Node ~p -> get_friends to ~p~n", [self(), Random_friend]),
                      case Send of
                        true ->
                          loop(New_friends_list, New_friends_list_ask -- [Random_friend], New_watcher_list ++ Watcher_list, Actors_list);
                        false ->
                          loop(New_friends_list, New_friends_list_ask, New_watcher_list ++ Watcher_list, Actors_list)
                      end
                  end
              end
          end;
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;


  % ricezione di un messaggio get_list da parte dell'attore scondario checker_list
  % SITUAZIONE: ogni 10 secondi l'attore secondario checker_list vuole conoscere la nostra lista di amici per
  % controllarne la cardinalità
  % COSA FARE: si risponde al checker_list inviandogli la nostra lista di amici
    {get_list, Sender} ->
      case Sender =:= whereis(checker_list_CR) of
        true ->
          %io:format("~n~n~nNode ~p -> get_list from ~p ~n~n~n", [Self, Sender]),
          Sender ! {list, Self, Friends_list},
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;


  % ricezione di un messaggio need_friends da parte dell'attore scondario checker_list
  % SITUAZIONE: l'attore secondario checker_list ha verificato che non abbiamo 3 amici e ce lo notifica
  % COSA FARE: se non abbiamo nessun amico mandiamo un messaggio al nodo professore, altrimenti richiediamo la
  % lista di amici da un nostro amico random
    {need_friends, Sender} ->
      case Sender =:= whereis(checker_list_CR) of
        true ->
          %%io:format("Node ~p -> need_friends from ~p ~n", [Self, Sender]),
          case length(Friends_list_ask) =:= 0 of
            true ->
              case global:whereis_name(teacher_node) of
                undefined -> ignore;
                %%io:format("Il prof non c'è ~n");
                _ ->
                  friends_library:sendMessageToTeacher({get_friends, Self, make_ref()})
              end,
              loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);
            false ->
              Random_friend = lists:nth(rand:uniform(length(Friends_list)), Friends_list),
              Send = friends_library:sendMessageToFriend({get_friends, Self, make_ref()}, Random_friend),
              %%io:format("Node ~p -> get_friends to ~p~n", [self(), Random_friend]),
              case Send of
                true ->
                  loop(Friends_list, Friends_list_ask -- [Random_friend], Watcher_list, Actors_list);
                false ->
                  loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
              end
          end;
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;


  % ricezione messaggio di morte di un amico da parte del watcher che lo monitora
    {dead, Sender, Friend} ->
      case lists:member({watcher, Friend, Sender}, Watcher_list) of
        true ->
          %%io:format("Node ~p -> Dead node: ~p~n",[self(), Friend]),
          loop(Friends_list -- [Friend], Friends_list_ask -- [Friend], Watcher_list -- [{watcher, Friend, Sender}], Actors_list);
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;
      

    % ricezione blocco: controllo che il blocco sia una quadrupla poi lo invio 
    % al gestore di blocchi
    {update, Sender, Block} ->
      case Block of
        {_, _, _, _} ->
          % %io:format("main - ricevuto blocco da ~p~n", [Sender]),
          Blh ! {update, Sender, Block},
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);
        _ ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;


    % richiesta di invio del messaggio Msg a tutti i nostri amici
    {send_msg_to_all_friends, Msg} ->
      % %io:format("main - send_msg_to_all_friends~p~n", [Msg]),
      sendMessageToAllFriends(Msg, Friends_list),
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);


    % ricezione transazione: controllo che la transazione sia una coppia la 
    % invio al gestore di transazioni
    {push, Transazione} ->
      case Transazione of
        {_, _} -> 
          % %io:format("ricevuta tr:~n~p~n", [Transazione]),
          Trh ! {tr_received, Self, Transazione},
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);
        _ -> loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)
      end;
    

    % ricezione messaggio get_previous: qualcuno ci chiede un blocco che non
    % non conosce: inoltriamo la richiesta al gestore di blocchi
    {get_previous, Mittente, Nonce, ID_blocco_precedente} ->
      % %io:format("main: riceve get_previous~n"),
      Blh ! {get_previous, Mittente, Nonce, ID_blocco_precedente},
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);


    % ricezione messaggio get_head: qualcuno ci chiede il blocco in cima allo
    % stack. inoltriamo il messaggio al gestore di blocchi.
    {get_head, Mittente, Nonce} ->
      % %io:format("main: riceve get_head~n"),
      Blh ! {get_head, Mittente, Nonce},
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list);


    _ -> loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list)

  end.

% ======================== MAIN ===============================================
main() ->
  process_flag(trap_exit, true),
  Self = self(),
  %%io:format("Main: ~p~n", [Self]),

  % attore che verifica se siamo vivi e se muoriamo ci rilancia
  spawn_link(fun() -> isAlive(Self) end),

  % dichiarazione adder_friends
  Af = spawn_link(fun() -> friends_library:adder_friends(Self) end),
  register(adder_friends_CR, Af),
  %%io:format("Pid ~p -> adder_friends registered ~n", [Af]),

  % dichiarazione checker_nonce
  Cn = spawn_link(fun() -> friends_library:checker_Nonce(Self, []) end),
  register(checker_nonce_CR, Cn),
  %%io:format("Pid ~p -> checker_nonce registered ~n", [Cn]),

  % dichiarazione checker_list
  Cl = spawn_link(fun() -> friends_library:checker_list(Self) end),
  register(checker_list_CR, Cl),
  %io:format("Pid ~p -> checker_list registered ~n", [Cl]),

  % dichiarazione gestore transazioni
  Trh = spawn_link(fun() -> tr_handler:tr_handler_actor(Self, [], []) end),

  % dichiarazione gestore blocchi
  Blh = spawn_link(fun() -> bl_handler:bl_handler_actor(Self, Trh, []) end),

  % invio al gestore di tr il pid del gestore dei blocchi
  Trh ! {send_pid_bl_handler, Blh},

  case global:whereis_name(teacher_node) of
    undefined -> ignore;
    %%io:format("Il prof non c'è ~n");
    _ ->
      friends_library:sendMessageToTeacher({get_friends, Self, make_ref()})
  end,
  
  io:format("Attore avviato ~p~n", [Self]),
  loop([], [], [], {Trh, Blh, Af, Cn, Cl}).


isAlive(Pid) ->
    process_flag(trap_exit, true),
    % %io:format("isAlive ~p", [self()]),
    receive
      {'EXIT', Pid, _} ->
        sleep(2),
        % controllo se ci sono rimasti attori registrati (questo perché )
        case whereis(adder_friends_CR) of
          undefined -> ignore;
          _ -> unregister(adder_friends_CR)
        end,
        case whereis(checker_nonce_CR) of
          undefined -> ignore;
          _ -> unregister(checker_nonce_CR)
        end,
        case whereis(checker_list_CR) of
          undefined -> ignore;
          _ -> unregister(checker_list_CR)
        end,
        main()
    end.
