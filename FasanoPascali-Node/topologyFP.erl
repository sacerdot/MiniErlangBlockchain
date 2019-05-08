-module(topologyFP).
-export([watch/2, managerNonce/1, newFriendsRequest/6, gossipingMessage/1, sendGetFriends/2, sendMaybeWrongMessages/3]).

%%monitora gli amici e capisce se uno di questi è morto o è in loop
watch(Main, Node) ->
  main:sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node}
  end.

%% gestisce lo storage dei Nonce e il loro controllo
managerNonce(Nonces) ->
  receive
    {updateNonce, Nonce} ->
      managerNonce(Nonces ++ [Nonce]);
    {checkNonce, Nonce, TempNonce, Sender} ->
      case lists:member(Nonce, Nonces) of
        true ->
          Sender ! {nonce, ok, TempNonce},
          managerNonce(Nonces--[Nonce]);
        false ->
          Sender ! {nonce, false, TempNonce},
          managerNonce(Nonces)
      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Step: 0-> Skip; 1-> 1° richiesta; 2->sleep e 2° richiesta ; 3-> chiedo al nodo prof e sleep. Sleep in attore secondario
newFriendsRequest(PIDMain, Friends, Step, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage) ->
  io:format("PIDMain: ~p Fun newFriendsRequest -> Friends: ~p~n", [PIDMain, Friends]),
  MyPid = self(),
  case Step of
    0 -> ok; %% Skip
    3 -> PIDManagerMessage ! {global}; %% richiesta verso il nodo prof
    _ -> %% caso 1 e 2 ovvero 1° e 2° richiesta verso i miei amici
      case length(Friends) of
        0 -> PIDManagerMessage ! {global};
        1 -> PIDManagerMessage ! {lists:nth(1, Friends), Step};
        2 -> PIDManagerMessage ! {lists:nth(Step, Friends), Step};%% Step possibili a questo livello solo 1 e 2
        _ -> ok
      end
  end,
  receive
    {friends, Nonce, FriendsOfFriend} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce, self()},
      receive
        {nonce, false, TempNonce} -> do_nothing;
        {nonce, ok, TempNonce} ->
%%          estraggo solo i nuovi amici e avvio i loro watcher
          NewFriends = extractNewFriends(FriendsOfFriend, Friends ++ [PIDMain], Friends),
          [spawn_link(fun() -> watch(MyPid, X) end) || X <- NewFriends],
          NewTotalFriends = Friends ++ NewFriends,
          PIDGossipingMessage ! {updateFriends, NewTotalFriends},
          if
            length(NewTotalFriends) >= 3 ->
              newFriendsRequest(PIDMain, NewTotalFriends, 0, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage);
            Step == 3 ->
              newFriendsRequest(PIDMain, NewTotalFriends, 1, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage);
            true ->
              newFriendsRequest(PIDMain, NewTotalFriends, Step + 1, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage)
          end
      after 5000 -> self() ! {friends, Nonce, FriendsOfFriend}
      end;

    {dead, Node} ->
      FriendsLess = Friends -- [Node],
      PIDGossipingMessage ! {updateFriends, FriendsLess},
      newFriendsRequest(PIDMain, FriendsLess, 1, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage);

    {get_friends, Sender, Nonce} ->
      sendMaybeWrongMessages(Sender, {friends, Nonce, Friends}, true);

    {sendMessageRandFriend, Message} ->
      R = rand:uniform(length(Friends)),
      lists:nth(R, Friends) ! Message
  after 30000 -> wait_no_more
  end,
  case length(Friends) of
    3 -> newFriendsRequest(PIDMain, Friends, 0, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage);
    _ -> newFriendsRequest(PIDMain, Friends, Step, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage)
  end.

%%l'attore avviato su tale funzione si ossupa del gossiping dei messaggi verso i nostri amici
gossipingMessage(Friends) ->
  receive
    {updateFriends, NewFriends} ->
      gossipingMessage(NewFriends);
    {gossipingMessage, Message} ->
      main:sleep(1),
      gossipingMessage(Friends, Message)
  end,
  gossipingMessage(Friends).
gossipingMessage([], _) -> ok;
gossipingMessage([H | T], Message) ->
  sendMaybeWrongMessages(H, Message, true),
  gossipingMessage(T, Message).

%% extractNewFriends è stato inserito per evitare lo spawn di nodi monitor già esistenti e quindi far ritornare solo i nuovi amici
extractNewFriends(FriendsOfFriend, MyFriendsAndI, InitFriends) ->
  FilterFriends = FriendsOfFriend--MyFriendsAndI,
  if
    length(FilterFriends) < 1 -> lists:sublist(MyFriendsAndI, length(MyFriendsAndI) - 1)-- InitFriends;
    true ->
      R = rand:uniform(length(FilterFriends)),
      if
        (length(MyFriendsAndI) - 1) < 3 ->
          extractNewFriends(FilterFriends, [lists:nth(R, FilterFriends)] ++ MyFriendsAndI, InitFriends);
        true -> lists:sublist(MyFriendsAndI, length(MyFriendsAndI) - 1) -- InitFriends
      end
  end.

%%è stato creato un attore secondario per l'invio di un messaggio di richiesta degli amici perchè in alcune
%% casistiche siamo bloccanti (per evitare la congestione della rete)
sendGetFriends(PIDMain, ManagerNonce) ->
  TempNonce = make_ref(),
  ManagerNonce ! {updateNonce, TempNonce},
  receive
    {global} -> global:send(teacher_node, {get_friends, PIDMain, TempNonce}), main:sleep(5);
    {Receive, Step} when Step == 2 -> main:sleep(5), Receive ! {get_friends, PIDMain, TempNonce};
    {Receive, _} -> Receive ! {get_friends, PIDMain, TempNonce}
  end,
%%  viene svuotata la coda dei messaggi perchè se ci sono arrivate ulteriori richieste di amici dobbiamo scartarle
%% avendone appena inviata una, inoltre se così non fosse l'essere bloccanti non ci evita la congestione della rete
  flushMailBox(),
  sendGetFriends(PIDMain, ManagerNonce).

%%pulisce la coda dei messaggi
flushMailBox() ->
  receive _ -> flushMailBox()
  after 0 -> ok
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% IsErrorActivated is used only for test reasons
sendMaybeWrongMessages(PidRecevier, Message, IsErrorActivated) ->
  case IsErrorActivated of
    false -> PidRecevier ! Message;
    true ->
      RandomNumber = rand:uniform(10),
      case RandomNumber of
        1 -> do_nothing;
        2 -> PidRecevier ! Message, PidRecevier ! Message;
        _ -> PidRecevier ! Message
      end
  end.