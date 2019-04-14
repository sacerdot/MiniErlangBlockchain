-module(test).
-export([test/0, init/0, loopMain/4, newFriendsRequest/4, managerNonce/2, extractNewFriends/3]).

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
%%  spawn(test, init, []),
%%  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, [])
.

init() ->
%%      chiedo amici
  PID = self(),
  NonceGlobalSend = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  ManagerNonce = spawn_link(test, managerNonce, [[], PID]),
  ManagerFriends = spawn_link(test, newFriendsRequest, [[], PID, 0, ManagerNonce]),
  receive
    {friends, NonceGlobalSend, ListFriends} ->
      ManagerFriends ! {friend, ListFriends}
  end,
  loopMain([], [], ManagerFriends, ManagerNonce).


loopMain(ListTransazioni, BlockChain, PIDManagerFriends, PIDManagerNonce) ->
%%  PID = self(),
  receive
    {friends, Nonce, ListFriends} ->
      PIDManagerNonce ! {checkNonce, Nonce},
      receive
        {nonce, false} -> loopMain(ListTransazioni, BlockChain, PIDManagerFriends, PIDManagerNonce);
        {nonce, ok} ->
          PIDManagerFriends ! {friend, ListFriends}
      end;

    {push, Transaction} ->%%when Transaction == {_, _} ->
      case lists:member(Transaction, ListTransazioni) of
        false -> PIDManagerFriends ! {gossipingTransaction, Transaction},
          NewListTransazioni = ListTransazioni ++ [Transaction],
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
      io:format("~p -> updateNonce Nonce ~p~n", [PIDMain, Nonce]),
      managerNonce(ListNonce ++ [Nonce], PIDMain);
    {checkNonce, Nonce} ->
      case lists:member(Nonce, ListNonce) of
        true ->
          io:format("~p -> checkNonce Nonce ~p OK ++++++++++->~n", [PIDMain, Nonce]),
          PIDMain ! {nonce, ok},
          managerNonce(ListNonce--[Nonce], PIDMain);
        false ->
          io:format("~p -> checkNonce Nonce ~p False -------->~n", [PIDMain, Nonce]),
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
%%  todo gestire casisitica 2 amici
newFriendsRequest(Friends, PIDMain, Step, ManagerNonce) ->
  io:format("~p ->Start newFriendsRequest Friends: ~p Step: ~p~n", [PIDMain, Friends, Step]),
  MyPid = self(),
  case Step of
    0 -> ok; %% Skip
    3 -> sendGetFriends(PIDMain, global, ManagerNonce); %% richiesta verso il nodo prof
    _ -> %% caso 1 e 2 ovvero 1° e 2° richiesta verso i miei amici
      if
        Step == 2 -> sleep(20);
        true -> ok
      end,
      case length(Friends) of
        0 -> sendGetFriends(PIDMain, global, ManagerNonce);
        1 -> sendGetFriends(PIDMain, lists:nth(1, Friends), ManagerNonce);
        2 -> RandomNumber = rand:uniform(2),
          sendGetFriends(PIDMain, lists:nth(RandomNumber, Friends), ManagerNonce);%%, sendGetFriends(PID, lists:nth(2, Friends), ManagerNonce)
        _ -> ok
      end
  end,
  receive
    {friend, ListFriends} ->
      io:format("~p -> Friend receive Friends: ~p Step: ~p~n", [PIDMain, ListFriends, Step]),
      NewFriends = extractNewFriends(ListFriends, Friends ++ [PIDMain], Friends),
      [spawn(fun() -> watch(MyPid, X) end) || X <- NewFriends],
      NewListFriends = Friends ++ NewFriends,
      if
        length(NewListFriends) >= 3 ->
          io:format("~p ->+++++ 1 ~p ~p~n", [PIDMain, Step, length(NewListFriends)]),
          newFriendsRequest(NewListFriends, PIDMain, 0, ManagerNonce);
        Step == 3 ->
          io:format("~p ->+++++ 2 ~p~n", [PIDMain, Step]),
          newFriendsRequest(NewListFriends, PIDMain, 1, ManagerNonce);
        true ->
          io:format("~p ->++++++ 3 ~p~n", [PIDMain, Step]),
          newFriendsRequest(NewListFriends, PIDMain, Step + 1, ManagerNonce)
      end;

    {dead, Node} ->
      io:format("~p -> Dead node ~p~n", [PIDMain, Node]),
      FriendsLess = Friends -- [Node],
      newFriendsRequest(FriendsLess, PIDMain, 1, ManagerNonce);

    {get_friends, Sender, Nonce} ->
      io:format("~p -> get_friends from node ~p~n", [PIDMain, Sender]),
      Sender ! {friends, Nonce, Friends};

    {gossipingTransaction, Transaction} ->
      io:format("~p -> gossipingTransaction : transaction ~p~n", [PIDMain, Transaction]),
      [X ! {push, Transaction} || X <- Friends]
  end,
  if
    length(Friends) >= 3 -> newFriendsRequest(Friends, PIDMain, 0, ManagerNonce);
    true -> newFriendsRequest(Friends, PIDMain, Step, ManagerNonce)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendGetFriends(Main, Receive, ManagerNonce) ->
  TempNonce = make_ref(),
  ManagerNonce ! {updateNonce, TempNonce},
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
%% exit(<0.68.0>, kill).
%% exit(<0.69.0>, kill).
%% exit(<0.70.0>, kill).
%% spawn(test, init, []).
