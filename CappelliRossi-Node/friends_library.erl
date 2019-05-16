-module(friends_library).
-export([watch/2, checker_Nonce/2, addFriendsToList/4, adder_friends/1, checker_list/1, sendMessageToTeacher/1, sendMessageToFriend/2]).

% Il watcher manda un messaggio ping ad un nostro amico che monitora e se entro 5 secondi l'amico non risponde con pong, esso
% viene considerato morto e il watcher ci notifica della sua morte tramite il messaggio dead
watch(Main, Friend) ->
  main:sleep(10),
  Self = self(),
  Nonce = make_ref(),
  Friend ! {ping, Self, Nonce},
  %io:format("~n~n~nWatcher ~p -> ping to ~p~n~n~n",[Self, Friend]),
  receive
    {pong, Nonce} ->
      %io:format("~n~n~nWatcher ~p -> pong from ~p~n~n~n",[Self, Friend]),
      watch(Main, Friend)
  after 5000 ->
    Main ! {dead, Self, Friend}
    %%io:format("Watcher ~p -> dead to ~p~n",[Self, Main])
  end.

% Controllo che il Nonce del messaggio get_friends che noi inviamo ad un nostro amico e del messaggio friends con cui
% ci risponde sia lo stesso in modo tale da garantire l'autenticità dei messaggi.
% Questo controllo viene eseguito da un attore secondario.
% Nel momento in cui inviamo un messaggio get_friends ad un nostro amico, lo inoltriamo (messaggio nonce) al controllore
% dei Nonce, esso verifica che effettivamente siamo stati noi ad inviargli il messaggio e se siamo stati noi aggiunge il
% Nonce alla lista che memorizza nel suo stato. Quando l'amico a cui abbiamo richiesto la lista di amici ci risponde
% con friends, inviamo al controllore il Nonce contenuto nel messaggio (check_nonce). A questo punto esso verifica se
% il Nonce appartiene alla lista, se si vuol dire che il Nonce è corretto e ci invia il messaggio Nonce_checked così
% noi possiamo elaborare la lista di amici ricevuta, se no il Nonce viene scartato
checker_Nonce(Main, Nonce_list) ->
  receive
    {nonce, Main, {get_friends, Main, Nonce_send}} ->
      %%io:format("Node ~p -> Send Nonce ~p~n", [Main, Nonce_send]),
      checker_Nonce(Main, [Nonce_send] ++ Nonce_list);
    {check_nonce, Main, Nonce, Friends_list_received} ->
      case lists:member(Nonce,Nonce_list) of
        true ->
          Main ! {nonce_checked, self(), Friends_list_received},
          %%io:format("Node ~p -> Nonce checked ~p~n", [Main, Nonce]),
          checker_Nonce(Main, Nonce_list -- [Nonce]);
        false ->
          %%io:format("Node ~p -> Nonce not checked ~p~n", [Main, Nonce]),
          checker_Nonce(Main, Nonce_list)
      end
  end.

% Aggiunta di amici:
% List_tmp è la lista di amici ricevuta da un proprio amico tolti quelli in comune e se stessi,
% Friends_list è la propria lista di amici.
% Ad ogni nuovo amico aggiunto alla nostra lista viene assegnato un processo che lo monitora (watcher)

% Si aggiungono amici da List_tmp alla nostra lista Friends_list fino a quando List_tmp non diventa vuota o fino a
% quando Friends_list non contiene 3 elementi. Quando List_tmp diventa vuota si controlla la lunghezza di Friends_list:
% 1) se la lunghezza è pari a 3 allora si termina restituendo la lista di amici
% 2) se la lunghezza è minore di 3 allora si invia un messaggio get_friends ad un amico random
% Se all'inizio dell'esecuzione, sia List_tmp che Friends_list sono vuote, significa che si hanno zero amici, per cui
% si invierà un messaggio get_friends al nodo professore
addFriendsToList(Main, List_tmp, Friends_list, Friends_list_ask) ->
  case length(List_tmp) > 0 of
    true ->
      case length(Friends_list) < 3 of
        true ->
          Friend = lists:nth(rand:uniform(length(List_tmp)), List_tmp),
          %%io:format("Node ~p -> New friend ~p~n",[Main, Friend]),
          %W = spawn_link(fun() -> watch(Main, Friend) end),
          %%io:format("~n~n~nNode ~p -> New watcher ~p~n~n~n",[Main, W]),
          addFriendsToList(Main, List_tmp -- [Friend], [Friend] ++ Friends_list, [Friend] ++ Friends_list);
        false ->
          {Friends_list, Friends_list_ask}
      end;
    false ->
      {Friends_list, Friends_list_ask}
  end.

% funzione eseguita in loop dell'attore che aggiunge gli amici
adder_friends(Main) ->
  receive
    {add_friends, Main, List_tmp, Friends_list, Friends_list_ask} ->
      {New_friends_list, New_friends_list_ask} = addFriendsToList(Main, List_tmp, Friends_list, Friends_list_ask),
      Main ! {friends_added, self(), New_friends_list, New_friends_list_ask}
  end,
  adder_friends(Main).

% Si controlla la lunghezza della lista degli amici. Questo controllo viene eseguito da un attore secondario.
% Ogni 10 secondi il controllore ci invia un messaggio get_list per chiederci la lista degli amici a cui noi rispondiamo
% con list e la lista. Una volta inviata la lista, il controllore verifica la sua lunghezza, se è pari a 3 non fa nulla
% mentre se è minore di 3 ci manda un messaggio need_friends con cui ci notifica che abbiamo bisogno di amici
checker_list(Main) ->
  main:sleep(5),
  Self = self(),
  Main ! {get_list, Self},
  receive
    {list, Main, Friends_list} ->
      case length(Friends_list) =:= 3 of
        true ->
          checker_list(Main);
        false ->
          Main ! {need_friends, Self}
      end
  after 2000 ->
    ok
  end,
  checker_list(Main).

% Invio del messaggio get_friends al nodo professore con inserimento di probabilità di perdere
% il messaggio o di inviarlo due volte
sendMessageToTeacher(Msg) ->
  case rand:uniform(10) of
    1 ->
      %%%io:format("Node ~p -> Messaggio al teacher_node non inviato ~n",[self()]),
      ignore;
    2 ->
      %%io:format("Node ~p -> Chiedo la lista degli amici al nodo professore ~n",[self()]),
      %%io:format("Node ~p -> Messaggio al teacher_node inviato due volte ~n",[self()]),
      global:send(teacher_node, Msg),
      global:send(teacher_node, Msg),
      % invio il messaggio al controllore dei Nonce
      case whereis(checker_nonce_CR) of
        undefined -> ignore;
        _ ->
          checker_nonce_CR ! {nonce, self(), Msg}
      end;
    _ ->
      %%io:format("Node ~p -> Chiedo la lista degli amici al nodo professore ~n",[self()]),
      global:send(teacher_node, Msg),
      % invio il messaggio al controllore dei Nonce
      case whereis(checker_nonce_CR) of
        undefined -> ignore;
        _ ->
          checker_nonce_CR ! {nonce, self(), Msg}
      end
  end.

% Invio del messaggio get_friends ad un amico random con inserimento di probabilità di perdere
% il messaggio o di inviarlo due volte
sendMessageToFriend(Msg, Friend) ->
  case rand:uniform(10) of
    1 ->
      %%io:format("Node ~p -> Messaggio a ~p non inviato ~n", [self(), Friend]),
      false;
    2 ->
      Friend ! Msg,
      Friend ! Msg,
      % invio il messaggio al controllore dei Nonce
      case whereis(checker_nonce_CR) of
        undefined -> ignore;
        _ ->
          checker_nonce_CR ! {nonce, self(), Msg}
      end,
      %%io:format("Node ~p -> Messaggio a ~p  inviato due volte ~n", [self(), Friend]),
      true;
    _ ->
      Friend ! Msg,
      % invio il messaggio al controllore dei Nonce
      case whereis(checker_nonce_CR) of
        undefined -> ignore;
        _ ->
          checker_nonce_CR ! {nonce, self(), Msg}
      end,
      %%io:format("Node ~p -> get_friends to ~p~n", [self(), Friend]),
      true
  end.
