-module(my_topo).

-export([init/0, test/0]).

%% todo mi risultano morti gli attori anche se non lo sono

%%attori che vengono lanciati:
%% 1) Main: è l'attore principale, l'attore che viene esposto nella
%% 3) WatcherFriend: uno per ogni amico. Avvisa Main quando il nodo amico che stanno controllando muore

sleep(N) -> receive after N * 1000 -> ok end.


watch(MainPID, FriendPID) ->
  sleep(10),
  Ref = make_ref(),
  FriendPID ! {ping, self(), Ref},
  io:format("ping da ~p a ~p~n", [MainPID, FriendPID]),
  receive
    {pong, Ref} -> watch(MainPID, FriendPID)
  after 2000 -> MainPID ! {dead, FriendPID}
  end.


test() ->
  spawn(teacher_node, main, []),
  sleep(3),
  spawn(my_topo, init, []),
  spawn(my_topo, init, []),
  spawn(my_topo, init, []),
  spawn(my_topo, init, []),
  io:format("teacher e i 4 nodi spawnati ~n", [])
.

%% primo passo: all'inizio non avendo amici, chiedo al nodo teacher degli amici, finché non me ne ritorna almeno uno
init() ->
  MainPID = self(),
  io:format("~p sta in init ~n", [MainPID]),
  NonceFirstRequest = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceFirstRequest}), %% chiedo al nodo teacher la richiesta d'amicizia la prima volta
  receive
    {friends, NonceFirstRequest, ListNodes} ->
      io:format("~p ha ricevuto i seguenti nodi da get_friends: ~p ~n", [MainPID, ListNodes]),
      ListFriends = case length(ListNodes) of
        1 -> sleep(10), init(); %% ha solo se stesso
        N when N > 4 -> getNRandomFriend([], ListNodes -- [MainPID], 3);
        _ -> ListNodes -- [MainPID]
      end,

      [spawn(fun() -> watch(MainPID, X) end) || X <- ListFriends], %% lancio i WatcherFriend

      loopMain(ListFriends, [], 3)
  after 10000 -> init()
  end.


%% Step serve per la gestione delle varie fasi di ricerca di amici: 1 e 2 quando ci mancano amici e chiediamo ai nostri amici, 3 quando ancora ci mancano amici e allora chiediamo al teacher
loopMain(ListMyFriends, ListNonces, Step) ->
  sleep(1),
  io:format("~p ha i seguenti amici: ~p ~n", [self(), ListMyFriends]),
  MainPID = self(),
  case length(ListMyFriends) of
    0 -> MyNonce = make_ref(), global:send(teacher_node, {get_friends, self(), MyNonce});
    N when N =< 2 -> case Step of
           1 -> MyNonce = make_ref(), lists:nth(rand:uniform(length(ListMyFriends)), ListMyFriends) ! {get_friends, MainPID, MyNonce};
           2 -> MyNonce = make_ref(), lists:nth(rand:uniform(length(ListMyFriends)), ListMyFriends) ! {get_friends, MainPID, MyNonce};
           3 -> MyNonce = make_ref(), global:send(teacher_node, {get_friends, MainPID, MyNonce})
         end;
    3 -> []
  end,
  receive
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref},
      loopMain(ListMyFriends,ListNonces, Step);

    {get_friends, Sender, Nonce} ->
      Sender ! {friends, Nonce, ListMyFriends},
      loopMain(ListMyFriends, ListNonces, Step);

    {dead, NodePID} ->
      io:format("Nodo ~p morto, ricevuto da ~p~n", [NodePID, MainPID]),
      NewListFriends = ListMyFriends -- [NodePID],
      Nonce = make_ref(),
      loopMain(NewListFriends, ListNonces ++ [Nonce], Step);

    {friends, Nonce, ListFriendsOfFriend} ->
      case lists:member(Nonce, ListNonces) of
        true ->  ListPotentialNewFriends = ListFriendsOfFriend -- [MainPID] -- [ListMyFriends],
          ListMyNewFriends = getNRandomFriend([], ListPotentialNewFriends, 3 - length(ListMyFriends)),
          [spawn(fun() -> watch(MainPID, X) end) || X <- ListMyNewFriends], %% lancio i WatcherFriend
          loopMain(ListMyFriends ++ ListMyNewFriends, ListNonces -- [Nonce], Step);
        false ->  io:format("Nonce non esistente, lanciato da ~p~n", [MainPID]), loopMain(ListMyFriends, ListNonces, Step)
      end
  after 10000 -> loopMain(ListMyFriends, ListNonces, Step)
  end.

getNRandomFriend(NewFriends, ListNodes, N) ->
  case N of
    N when N =< 0 -> NewFriends;
    _ ->  I = rand:uniform(length(ListNodes)),
      NewFriend = lists:nth(I, ListNodes),
      getNRandomFriend(NewFriends ++ [NewFriend], ListNodes -- [NewFriend], N - 1)
  end.






