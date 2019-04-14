-module(test).
-export([test/0, init/0, loopMain/4, newFriendsRequest/3, managerNonce/2, extractNewFriends/3]).

% This is the teacher node, the one responsible
% for letting new nodes in the blockchain.
%
% It implements the topology maintainance algorithm
%
% It registers itself globally under the name teacher_node

sleep(N) -> receive after N * 1000 -> ok end.


watch(Main, Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  io:format("ping da ~p a ~p~n", [self(), Node]),
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node}
  end.


test() ->
  spawn(teacher_node, main, []),
  sleep(3),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, [])
.

init() ->
%%      chiedo amici
  PID = self(),
  NonceGlobalSend = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  self() ! {updateNonce, NonceGlobalSend},
  ManagerFriends = spawn(test, newFriendsRequest, [[], PID, 1]),
  ManagerNonce = spawn(test, managerNonce, [[], PID]),
  loopMain([], [], ManagerFriends, ManagerNonce).


%% todo testare
loopMain(ListTransazioni, BlockChain, PIDManagerFriends, PIDManagerNonce) ->
  PID = self(),

  receive
    {friends, Nonce, ListFriends} ->
      PIDManagerNonce ! {checkNonce, Nonce},
      receive
        {nonce, false} -> loopMain(ListTransazioni, BlockChain, PIDManagerFriends, PIDManagerNonce);
        {nonce, ok} ->
          PIDManagerFriends ! {friend, ListFriends},
          io:format("friend receive ~p ~p~n", [PID, Nonce])
      end;

    {push, Transazione} when Transazione == {_, _} ->
      case lists:member(Transazione, ListTransazioni) of
        false -> PIDManagerFriends ! {gossiping_transazione, Transazione},
          NewListTransazioni = ListTransazioni ++ [Transazione],
          loopMain(NewListTransazioni, BlockChain, PIDManagerFriends, PIDManagerNonce)
      end;

    {get_friends, Sender, Nonce} ->
      PIDManagerFriends ! {get_friends, Sender, Nonce};

    {ping, Sender, Ref} ->
      Sender ! {pong, Ref}
  end,
  loopMain(ListTransazioni, BlockChain, PIDManagerFriends, PIDManagerNonce).



%% gestisce lo storage dei Nonce e il loro controllo
managerNonce(ListNonce, PIDMain) ->
  receive
    {updateNonce, Nonce} ->
      managerNonce(ListNonce ++ [Nonce], PIDMain);
    {checkNonce, Nonce} ->
      case lists:member(Nonce, ListNonce) of
        true ->
          PIDMain ! {nonce, ok},
          managerNonce(ListNonce--[Nonce], PIDMain);
        false ->
          PIDMain ! {nonce, false},
          managerNonce(ListNonce, PIDMain)
      end
  end.

%% InitFriends è inserito per evitare di spawn nodi monitor già esistenti
extractNewFriends(ListFriends, MyListFriends, InitFriends) ->
  FilterListFriends = lists:filter(fun(Elem) -> not lists:member(Elem, MyListFriends) end, ListFriends),
  LfilterFriends = length(FilterListFriends),
  if
    LfilterFriends < 1 -> lists:sublist(MyListFriends, length(MyListFriends) - 1)-- InitFriends;
    true ->
      R = rand:uniform(length(FilterListFriends)),
      Llist3Friends = length(MyListFriends) - 1,
      if
        Llist3Friends < 3 ->
          extractNewFriends(FilterListFriends, [lists:nth(R, FilterListFriends)] ++ MyListFriends, InitFriends);
        true -> lists:sublist(MyListFriends, length(MyListFriends) - 1) -- InitFriends
      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Step: 0-> Skip; 1-> 1° richiesta; 2-> 2° richiesta sleep; 3-> chiedo al nodo prof.
%% todo testare
newFriendsRequest(Friends, PIDMain, Step) ->
  MyPid = self(),
  case Step of
    0 -> ok; %% Skip
    3 -> sendGetFriends(PIDMain, global); %% richiesta verso il nodo prof
    _ -> %% caso 1 e 2 ovvero 1° e 2° richiesta verso i miei amici
      if
        Step == 2 -> sleep(10);
        true -> ok
      end,
%%  todo gestire casisitica 2 amici
      case length(Friends) of
        0 -> sendGetFriends(PIDMain, global);
        1 -> sendGetFriends(PIDMain, lists:nth(1, Friends));
        2 -> sendGetFriends(PIDMain, lists:nth(2, Friends));%%, sendGetFriends(PID, lists:nth(2, Friends))
        _ -> ok
      end
  end,
  receive
    {friend, ListFriends} ->
      NewFriends = extractNewFriends(ListFriends, Friends ++ [PIDMain], Friends),
      [spawn(fun() -> watch(MyPid, X) end) || X <- NewFriends],
      NewListFriends = Friends ++ NewFriends,
      if
        length(NewListFriends) >= 3 -> newFriendsRequest(NewListFriends, PIDMain, 0);
        Step == 3 -> newFriendsRequest(NewListFriends, PIDMain, 1);
        true -> newFriendsRequest(NewListFriends, PIDMain, Step + 1)
      end;
    {dead, Node} ->
      io:format("Dead node ~p~n", [Node]),
      FriendsLess = Friends -- [Node],
      newFriendsRequest(FriendsLess, PIDMain, 0);
    {get_friends, Sender, Nonce} ->
      Sender ! {friends, Nonce, Friends};
    {gossiping_transazione, Transazione} ->
      [X ! {push, Transazione} || X <- Friends]
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendGetFriends(Main, Receive) ->
  TempNonce = make_ref(),
  Main ! {updateNonce, TempNonce},
  if
    Receive == global -> global:send(teacher_node, {get_friends, Main, TempNonce});
    true -> Receive ! {get_friends, Main, TempNonce}
  end.




sendMaybeWrongMessages(PidRecevier, Message, IsErrorActivated) ->
  if
    IsErrorActivated == false ->
      PidRecevier ! Message;
    true ->
      RandomNumber = rand:uniform(10),
      case RandomNumber of
        1 -> do_nothing;
        2 -> PidRecevier ! Message, PidRecevier ! Message;
        _ -> PidRecevier ! Message
      end
  end.

%% c(test). test:test().  exit(<0.71.0>, kill). spawn(test, init, []).