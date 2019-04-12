-module(test).
-export([test/0, init/0, start/5, getNewFriends/3, ext3Friends/3]).

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
  NonceGlobalSend = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  self() ! {updateNonce, NonceGlobalSend},
  start([], [], [], [], 0).

start(Friends, ListTransazioni, BlockChain, ListNonce, Step) ->
  PID = self(),
  receive
    {friends, Nonce, ListFriends} ->
      io:format("friend receive ~p ~p~n", [PID, Nonce]),
      NewFriends = ext3Friends(ListFriends, Friends ++ [PID], Friends),
      [spawn(fun() -> watch(PID, X) end) || X <- NewFriends],
%%      io:format("List3Friends~p~p      ~n", [self(),NewFriends]),
      NewListFriends = Friends ++ NewFriends,
      spawn(test, getNewFriends, [NewListFriends, PID, Step]),
      start(NewListFriends, ListTransazioni, BlockChain, ListNonce, Step);

    {ping, Sender, Ref} ->
      Sender ! {pong, Ref};

    {push, Transazione} ->
      case lists:member(Transazione, ListTransazioni) of
        true -> [X ! {push, Transazione} || X <- Friends],
          NewListTransazioni = ListTransazioni ++ Transazione,
          start(Friends, NewListTransazioni, BlockChain, ListNonce, Step)
      end;

    {get_friends, Sender, Nonce} ->
      TempNonce = make_ref(),
      PID ! {updateNonce, TempNonce},
      Sender ! {friends, TempNonce, Friends};

    {dead, Node} ->
      io:format("Dead node ~p, da ~p~n", [Node, PID]),
      FriendsLess = Friends -- [Node],
      spawn(test, getNewFriends, [FriendsLess, PID, Step]),
      start(FriendsLess, ListTransazioni, BlockChain, ListNonce, Step);

    {updateNonce, Nonce} ->
      start(Friends, ListTransazioni, BlockChain, ListNonce ++ Nonce, Step);

    {updateStep, NewStep} ->
      start(Friends, ListTransazioni, BlockChain, ListNonce, NewStep)
  end,
  start(Friends, ListTransazioni, BlockChain, ListNonce, Step).

%% InitFriends è inserito per evitare di spawn nodi monitor già esistenti
ext3Friends(ListFriends, MyListFriends, InitFriends) ->  %%  when ListFriends=:= _:_
  FilterListFriends = lists:filter(fun(Elem) -> not lists:member(Elem, MyListFriends) end, ListFriends),
  LfilterFriends = length(FilterListFriends),
  if
    LfilterFriends < 1 -> lists:sublist(MyListFriends, length(MyListFriends) - 1)-- InitFriends;
    true ->
      R = rand:uniform(length(FilterListFriends)),
      Llist3Friends = length(MyListFriends) - 1,
      if
        Llist3Friends < 3 ->
          ext3Friends(FilterListFriends, [lists:nth(R, FilterListFriends)] ++ MyListFriends, InitFriends);
        true -> lists:sublist(MyListFriends, length(MyListFriends) - 1) -- InitFriends
      end
  end.

getNewFriends(Friends, PID, Step) -> %%manca sleep quindi chiede iterativamente sempre
%%  todo sleep prima di provare a parlare col prof
%% todo  esaurire le richieste agli amici
  case Step of
    2 ->
      TempNonce = make_ref(),
      PID ! {updateNonce, TempNonce},
      global:send(teacher_node, {get_friends, PID, TempNonce}),
      PID ! {step, 0};
    1 -> sleep(3),
      PID ! {step, 2}
  end,
  case length(Friends) of
    0 ->
      Nonce = make_ref(),
      PID ! {step, 0},
      PID ! {updateNonce, Nonce},
      global:send(teacher_node, {get_friends, PID, Nonce});
    1 -> sendGetFriends(PID, lists:nth(1, Friends));
    2 -> sendGetFriends(PID, lists:nth(1, Friends)), sendGetFriends(PID, lists:nth(2, Friends));
    _ -> PID ! {step, 0},
      exit(self(), kill)
  end.

sendGetFriends(Main, Friend) ->
  Nonce = make_ref(),
  Main ! {step, 1},
  Main ! {updateNonce, Nonce},
  Friend ! {get_friends, Main, Nonce}
.

%% c(test). test:test().  test:start(). exit(<0.71.0>, kill).