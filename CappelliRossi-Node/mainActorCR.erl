-module(mainActorCR).
-export([main/0, sleep/1, retreive_ID_blocco_testa/1, sendMessageToAllFriends/2]).

% Uso:
% c(mainActorCR).
% mainActorCR:test()

% ====================== FUNZIONI DI LIBRERIA =================================

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

% identificazione dell'id dell'ultimo blocco inserito nella lista
retreive_ID_blocco_testa(BlockChain) ->
  case BlockChain of
    [] -> none;
    _ ->
      [{ID_blocco_testa, _, _, _} | _]  = BlockChain,
      ID_blocco_testa
  end.

% ======================== LOOP ===============================================
loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain) ->
  sleep(1),
  Self = self(),
  io:format("Loop started~n"),
  io:format("Node ~p -> Friends_list ~p~n", [Self, Friends_list]),
  io:format("Node ~p -> Friends_list_ask ~p~n", [Self, Friends_list_ask]),

  receive 

    % ricezione di un messaggio ping: rispondiamo con pong
    {ping, Sender, Nonce} ->
      io:format("Node ~p -> ping from ~p~n", [Self, Sender]),
      Sender ! {pong, Nonce},
      io:format("Node ~p -> pong to ~p~n", [Self, Sender]),
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);

    % ricezione di un messaggio exit a casusa della morte di un nostro figlio
    {'EXIT', Pid, Msg} ->
      io:format("Morte di ~p~n", [Pid]),
      io:format("Attori ~p~n", [Actors_list]),
      Atomi_attori = [Attore || {Attore, P} <- Actors_list, P =:= Pid],
      case length(Atomi_attori) =:= 0 of
        false ->
          [Actor_name | _] = Atomi_attori, 
          io:format("A= ~p~n", [Actor_name]),
          case Actor_name of
            get_previous_handler_CR ->
              io:format("Morte di get_previous_handler_CR. Riavvio del figlio in corso...~n"),
              Gph = spawn_link(fun() -> get_previous_handler:get_previous_handler_main([]) end),
              register(get_previous_handler_CR, Gph),
              % io:format("Figli:~n~p", [[{get_previous_handler_CR, Gph}] ++ Actors_list -- [{get_previous_handler_CR, Pid}]]),
              loop(Friends_list, Friends_list_ask, Watcher_list, [{get_previous_handler_CR, Gph}] ++ Actors_list -- [{get_previous_handler_CR, Pid}], Transactions_list, BlockChain);
            adder_friends_CR ->
              io:format("Morte di adder_friends_CR. Riavvio del figlio in corso...~n"),
              Af = spawn_link(fun() -> friends_library:adder_friends(self()) end),
              register(adder_friends_CR, Af),
              loop(Friends_list, Friends_list_ask, Watcher_list, [{adder_friends_CR, Af}] ++ Actors_list -- [{adder_friends_CR, Pid}], Transactions_list, BlockChain);
            checker_list_CR ->
              io:format("Morte di checker_list_CR. Riavvio del figlio in corso...~n"),
              Cl = spawn_link(fun() -> friends_library:checker_list(self()) end),
              register(checker_list_CR, Cl),
              loop(Friends_list, Friends_list_ask, Watcher_list, [{checker_list_CR, Cl}] ++ Actors_list -- [{checker_list_CR, Pid}], Transactions_list, BlockChain);
            checker_nonce_CR ->
              io:format("Morte di checker_nonce_CR. Riavvio del figlio in corso...~n"),
              Cn = spawn_link(fun() -> friends_library:checker_Nonce(self(), []) end),
              register(checker_nonce_CR, Cn),
              loop(Friends_list, Friends_list_ask, Watcher_list, [{checker_nonce_CR, Cn}] ++ Actors_list -- [{checker_nonce_CR, Pid}], Transactions_list, BlockChain);
            _ -> ignore
          end;
        true when Msg =/= normal ->
          Friend_list = [ Y || {_,Y,X} <- Watcher_list, Pid =:= X],
          case length(Friend_list) =/= 0 of
            true ->
              [Friend | _] = Friend_list,
              io:format("Morte di Watcher ~p. Riavvio del figlio in corso...~n", [Pid]),
              W = spawn_link(fun() -> friends_library:watch(Self, Friend) end),
              loop(Friends_list, Friends_list_ask, [{watcher, Friend, W}] ++ Watcher_list -- [{watcher, Friend, Pid}], Actors_list, Transactions_list, BlockChain);
            false -> ignore
          end
      end;

    % ricezione di un messaggio get_friends da parte di un attore
    % SITUAZIONE: un attore ha bisogno di amici e ci chiede la nostra lista di amici
    % COSA FARE: verifichiamo se lo abbiamo come amico, se si gli inviamo la nostra lista tramite un messaggio
    % friends, altrimenti prima lo aggiungiamo alla nostra lista di amici e poi gliela inviamo
    {get_friends, Sender, Nonce} ->
      io:format("Node ~p -> get_friends from ~p~n", [Self, Sender]),
      {New_friends_list, New_friends_list_ask, New_Watcher_list} =
        case lists:member(Sender, Friends_list) of
          true ->
            {Friends_list, Friends_list_ask, Watcher_list};
          false ->
            case length(Friends_list) < 3 of
              true ->
                io:format("Node ~p -> New friend ~p~n",[Self, Sender]),
                W = spawn_link(fun() -> friends_library:watch(Self, Sender) end),
                {[Sender] ++ Friends_list, [Sender] ++ Friends_list, [{watcher, Sender, W}] ++ Watcher_list};
              false ->
                {Friends_list, Friends_list_ask, Watcher_list}
            end
        end,
      friends_library:sendMessageToFriend({friends, Nonce, New_friends_list}, Sender),
      io:format("Node ~p -> friends to ~p~n", [Self, Sender]),
      loop(New_friends_list, New_friends_list_ask, New_Watcher_list, Actors_list, Transactions_list, BlockChain);

    % ricezione di un messaggio friends da parte di un nostro amico
    % SITUAZIONE: precedentemente abbiamo chiesto la lista di amici ad un nostro amico e lui ce la invia
    % COSA FARE: il nostro amico ci invia la sua lista di amici ma prima di gestirla verifichiamo che il messaggio che
    % ci ha inviato abbia lo stesso Nonce del messaggio di richiesta di amici get_friends inviatogli precedentemente
    % così da evitare messaggi disonesti
    {friends, Nonce, Friends_list_received} ->
      checker_nonce_CR ! {check_nonce, Self, Nonce, Friends_list_received},
      loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);

    % ricezione di un messaggio nonce_checked da parte dell'attore secondario checker_nonce
    % SITUAZIONE: il Nonce tra i messaggi get_friends e friends scambiati con un nostro amico corrispondono
    % e veniamo notificati di questo dal nostro attore scondario
    % COSA FARE: si aggiungono gli amici presenti nella lista inviataci da un nostro amico che noi non abbiamo
    % alla nostra lista di amici
    {nonce_checked, Sender, Friends_list_received} ->
      case Sender =:= whereis(checker_nonce_CR) of
        true ->
          io:format("Node ~p -> receive friends~n", [Self]),
          case length(Friends_list) < 3 of
            true ->
              List_tmp = ((Friends_list_received -- [Self]) -- Friends_list),
              adder_friends_CR ! {add_friends, Self, List_tmp, Friends_list, Friends_list_ask, Watcher_list},
              loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);
            false ->
              loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
          end;
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
      end;

    % ricezione di un messaggio friends_added da parte dell'attore secondario adder_friends
    % SITUAZIONE: l'attore secondario adder_friends ha aggiornato la nostra lista di amici
    % COSA FARE: si controlla il numero di amici presenti nella lista,
    % -se sono 3 si ha raggiunto il numero massimo di amici
    % -se non si ha nessun amico si richiede la lista al nodo professore
    % -se sono meno di 3 si richiede la lista di amici ad un nostro amico random o se nessuno dei nostri amici ci
    % permette di arrivare a 3 si manda un messaggio al nodo professore
    {friends_added, Sender, New_friends_list, New_friends_list_ask, New_watcher_list} ->
      case Sender =:= whereis(adder_friends_CR) of
        true ->
          io:format("Node ~p -> Added friends~n", [Self]),
          case length(New_friends_list) =:= 3 of
            true ->
              case length(BlockChain) =:= 0 of
                true -> spawn(fun() -> blockchain_handler:main_attore_get_head(Self, New_friends_list) end);
                false -> ignore
              end,
              loop(New_friends_list, New_friends_list_ask, New_watcher_list, Actors_list, Transactions_list, BlockChain);
            false ->
              case length(New_friends_list) =:= 0 of
                true ->
                  friends_library:sendMessageToTeacher({get_friends, Self, make_ref()}),
                  loop(New_friends_list, New_friends_list_ask, New_watcher_list, Actors_list, Transactions_list, BlockChain);
                false ->
                  case length(BlockChain) =:= 0 of
                    true -> spawn(fun() -> blockchain_handler:main_attore_get_head(Self, New_friends_list) end);
                    false -> ignore
                  end,
                  case length(New_friends_list_ask) =:= 0 of
                    true ->
                      friends_library:sendMessageToTeacher({get_friends, Self, make_ref()}),
                      loop(New_friends_list, New_friends_list_ask, New_watcher_list, Actors_list, Transactions_list, BlockChain);
                    false ->
                      Random_friend = lists:nth(rand:uniform(length(New_friends_list)), New_friends_list),
                      Send = friends_library:sendMessageToFriend({get_friends, Self, make_ref()}, Random_friend),
                      io:format("Node ~p -> get_friends to ~p~n", [self(), Random_friend]),
                      case Send of
                        true ->
                          loop(New_friends_list, New_friends_list_ask -- [Random_friend], New_watcher_list, Actors_list, Transactions_list, BlockChain);
                        false ->
                          loop(New_friends_list, New_friends_list_ask, New_watcher_list, Actors_list, Transactions_list, BlockChain)
                      end
                  end
              end
          end;
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
      end;

    % ricezione di un messaggio get_list da parte dell'attore scondario checker_list
    % SITUAZIONE: ogni 10 secondi l'attore secondario checker_list vuole conoscere la nostra lista di amici per
    % controllarne la cardinalità
    % COSA FARE: si risponde al checker_list inviandogli la nostra lista di amici
    {get_list, Sender} ->
      case Sender =:= whereis(checker_list_CR) of
        true ->
          io:format("Node ~p -> get_list from ~p ~n", [Self, Sender]),
          Sender ! {list, Self, Friends_list},
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
      end;

    % ricezione di un messaggio need_friends da parte dell'attore scondario checker_list
    % SITUAZIONE: l'attore secondario checker_list ha verificato che non abbiamo 3 amici e ce lo notifica
    % COSA FARE: se non abbiamo nessun amico mandiamo un messaggio al nodo professore, altrimenti richiediamo la
    % lista di amici da un nostro amico random
    {need_friends, Sender} ->
      case Sender =:= whereis(checker_list_CR) of
        true ->
          io:format("Node ~p -> need_friends from ~p ~n", [Self, Sender]),
          case length(Friends_list_ask) =:= 0 of
            true ->
              friends_library:sendMessageToTeacher({get_friends, Self, make_ref()}),
              loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);
            false ->
              Random_friend = lists:nth(rand:uniform(length(Friends_list)), Friends_list),
              Send = friends_library:sendMessageToFriend({get_friends, Self, make_ref()}, Random_friend),
              io:format("Node ~p -> get_friends to ~p~n", [self(), Random_friend]),
              case Send of
                true ->
                  loop(Friends_list, Friends_list_ask -- [Random_friend], Watcher_list, Actors_list, Transactions_list, BlockChain);
                false ->
                  loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
              end
          end;
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
      end;

    {dead, Sender, Friend} ->
      case lists:member({watcher, Friend, Sender}, Watcher_list) of
        true ->
          io:format("Node ~p -> Dead node: ~p~n",[self(), Friend]),
          loop(Friends_list -- [Friend], Friends_list_ask -- [Friend], Watcher_list -- [{watcher, Friend, Sender}], Actors_list, Transactions_list, BlockChain);
        false ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain)
      end;

    % ricezione messaggio update = ricezione di un blocco
    % * SITUAZIONE: qualcuno ci invia un blocco
    % * COSA FARE: si verifca se il blocco è già nella nostra catena, in caso
    %   affermativo lo si scarta, in caso negativo si controlla che il blocco
    %   sia corretto poi si determina cosa fare del blocco. 
    {update, Sender, Block} ->
      case lists:member(Block, BlockChain) of
        % blocco già presente nella blockchain
        true ->
          io:format("Mittente ~p invia blocco noto: ", [Sender]),
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);
        % blocco non presente nella blockchain
        false ->
          io:format("Mittente ~p invia blocco non noto: ", [Sender]),
          {New_Tr_List, New_Blockchain} = blockchain_handler:rilevato_blocco_sconosciuto(Self,Sender,Friends_list,Block,Transactions_list,BlockChain),
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, New_Tr_List, New_Blockchain)
      end;

    % ricezione messaggio new_chain = l'attore che stava ricostruendo la catena
    % ha terminato ed invia la parte di catena ricostruita all'attore princpiale.
    % Si confrontano le lunghezze delle due parti di catena: se la nostra catena
    % è più lunga si scarta la lista ricevuta, se è più lunga la catena ricevuta
    % aggiorniamo la nostra catena
    {new_chain, BlocksToAdd_list} ->
      case blockchain_handler:isLonger(BlockChain,BlocksToAdd_list) of
        % la nostra catena è più lunga
        true ->
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, Transactions_list, BlockChain);
        % la nostra catena è più corta. Otteniamo l'indice del primo blocco da
        % sostituire
        Index ->
          io:format("L'attore principale riceve la nuova catena ~n"),
          {New_blockChain, New_transactions_list} = blockchain_handler:blockChainReconstruction(BlockChain, BlocksToAdd_list, Transactions_list, Index, Self),
          io:format("catena:~n~p~n", [New_blockChain]),
          loop(Friends_list, Friends_list_ask, Watcher_list, Actors_list, New_transactions_list, New_blockChain)
      end;

    % ricezione messaggio push = ricezione di una transazione 
    %  * SITUAZIONE: qualcuno ci invia una transazione
    %  * COSA FARE: si verifica se la transazione è conosciuta (contenuta in un
    %    blocco o nella lista di transazioni), se non è conosciuta la si
    %    inserisce in coda nella lista di transazioni, la si invia a tutti gli 
    %    amici e si verifica se avviare il minatore
    {push, Transazione} ->
      case lists:member(Transazione, Transactions_list) or miner:find_transaction_in_block_list(Transazione, BlockChain) of
        % Transazione conosciuta
        true ->
          io:format("Ricevuta transazione duplicata~n"),
          loop(Friends_list, Friends_list_ask, Watcher_list,Actors_list, Transactions_list, BlockChain);
        % Transazione sconosciuta: la si invia a tutti gli amici e si verifica
        % se il minatore sta minando
        false ->
          io:format("Ricevuta transazione ~p~n", [Transazione]),
          sendMessageToAllFriends({push, Transazione}, Friends_list),
          case whereis(minerCR) of
            % se il minatore non sta minando, si iniziano a minare le prime 10
            % transazioni della lista (se ce ne sono meno di 10 si minano tutte)
            undefined ->
              miner:call_miner(Self, BlockChain, lists:sublist(Transactions_list ++ [Transazione],1,10)),
              loop(Friends_list, Friends_list_ask, Watcher_list,Actors_list, Transactions_list ++ [Transazione], BlockChain);
            % se il minatore sta già minando si controlla quante transazioni ci
            % sono nella lista: se sono meno di 10 occorre uccidere il minatore
            % e lo si fa ripartire
            PID_miner ->
              spawn(fun() -> miner:miner_restarter(PID_miner, Self, BlockChain, Transactions_list ++ [Transazione]) end),
              loop(Friends_list, Friends_list_ask, Watcher_list,Actors_list, Transactions_list ++ [Transazione], BlockChain)
          end          
      end;

    % ricezione messaggio get_previous:
    %  * SITUAZIONE: qualcuno ci chiede un blocco (ID_blocco_precedente) che 
    %    non conosce
    %  * COSA FARE: dobbiamo o rispondere subito se conosciamo il blocco, o 
    %    salvarci la richiesta e rispondere non appena entriamo a conoscenza di 
    %    quel blocco
    {get_previous, Mittente, Nonce, ID_blocco_precedente} ->
      % verifico se conosco il blocco (= è nella mia blockchain)
      case get_previous_handler:find_BlockE(ID_blocco_precedente, BlockChain) of
        % non conosco il blocco -> passo il messaggio all'attore che si occupa
        % di gestire questa richiesta
        false ->
          io:format("Main riceve get_previous con blocco sconosciuto~n"),
          get_previous_handler ! {get_previous, Mittente, Nonce, ID_blocco_precedente};
        % conosco il blocco -> restituisco il blocco
        Blocco ->
          io:format("Main riceve get_previous con blocco conosciuto~n"),
          Mittente ! {previous, Nonce, Blocco}
      end,
      loop(Friends_list, Friends_list_ask, Watcher_list,Actors_list,  Transactions_list, BlockChain);

    % ricezione messaggio get_head:
    %  * SITUAZIONE: qualcuno ci chiede la cima dello stack
    %  * COSA FARE: restituiamo il blocco di testa della blockchain
    {get_head, Mittente, Nonce} ->
    	case length(BlockChain) =:= 0 of
    		true ->
    			loop(Friends_list, Friends_list_ask, Watcher_list,Actors_list, Transactions_list, BlockChain);
    		false->
    			[First_Block | _] = BlockChain,
		      io:format("Main riceve get_head~n~p~n", [First_Block]),
		      Mittente ! {head, Nonce, First_Block},
		      loop(Friends_list, Friends_list_ask, Watcher_list,Actors_list, Transactions_list, BlockChain)
    	end;
      
    _ -> ignore

  end.

% ======================== MAIN ===============================================
main() ->
  process_flag(trap_exit, true),
  Self = self(),
  io:format("Main: ~p~n", [Self]),

  Af = spawn_link(fun() -> friends_library:adder_friends(Self) end),
  register(adder_friends_CR, Af),
  io:format("Pid ~p -> adder_friends registered ~n", [Af]),

  Nonce = make_ref(),
  friends_library:sendMessageToTeacher({get_friends, Self, Nonce}),
  
  Cn = spawn_link(fun() -> friends_library:checker_Nonce(Self, Nonce) end),
  register(checker_nonce_CR, Cn),
  io:format("Pid ~p -> checker_nonce registered ~n", [Cn]),

  Cl = spawn_link(fun() -> friends_library:checker_list(Self) end),
  register(checker_list_CR, Cl),
  io:format("Pid ~p -> checker_list registered ~n", [Cl]),

  % dichiarazione handler get_previous
  Gph = spawn_link(fun() -> get_previous_handler:get_previous_handler_main([]) end),
  register(get_previous_handler_CR, Gph),

  % dichiarazione minatore
  Miner = spawn(fun() -> miner:miner_main(Self, none, []) end),
  register(miner, Miner),

  loop([], [], [], [{adder_friends_CR, Af},{checker_nonce_CR, Cn}, {checker_list_CR, Cl}, {get_previous_handler_CR, Gph}], [], []).