-module(test).
-export([test/0, init/0, start/1, getNewFriends/2, ext3Friends/3]).

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
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node, self()}
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
  global:send(teacher_node, {get_friends, self(), make_ref()}),
  start([]).

start(Friends) ->
  receive
    {friends, Nonce, ListFriends} ->
      io:format("friend receive ~p ~p~n", [self(),Nonce]),
      NewFriends = ext3Friends(ListFriends, [self()] ++ Friends, Friends),
      [spawn(fun() -> watch(self(), X) end) || X <- NewFriends],
%%      io:format("List3Friends~p~p      ~n", [self(),NewFriends]),
      NewListFriends = Friends ++ NewFriends,
      spawn(test, getNewFriends, [NewListFriends, self()]),
      start(NewListFriends);
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref};
    {get_friends, Sender, Nonce} ->
      Sender ! {friends, make_ref(), Friends};
    {dead, Node, PID} ->
      exit(PID, kill),
      FriendsLess = Friends -- [Node],
      spawn(test, getNewFriends, [FriendsLess, self()]),
      start(FriendsLess)
  end,
  start(Friends).

%% InitFriends è inserito per evitare di spawn nodi monitor già esistenti
ext3Friends(ListFriends, MyListFriends, InitFriends) ->  %%  when ListFriends=:= _:_
  FilterListFriends = lists:filter(fun(Elem) -> not lists:member(Elem, MyListFriends) end, ListFriends),
  LfilterFriends = length(FilterListFriends),
  if
    LfilterFriends < 1 -> lists:sublist(MyListFriends, length(MyListFriends) - 1);
    true ->
      R = rand:uniform(length(FilterListFriends)),
      Llist3Friends = length(MyListFriends) - 1,
      if
        Llist3Friends < 3 -> ext3Friends(FilterListFriends, [lists:nth(R, FilterListFriends)] ++ MyListFriends, InitFriends);
        true -> lists:sublist(MyListFriends, length(MyListFriends) - 1) -- InitFriends
      end
  end.

getNewFriends(Friends, PID) -> %%manca sleep quindi chiede iterativamente sempre
%%  todo sleep prima di provare a parlare col prof
%% todo  esaurire le richieste agli amici
  NumFriends = length(Friends),
%%  io:format("friend receive ~p ~p~n", [self(),NumFriends]),
  if
    NumFriends >= 3 -> exit(self(), kill);
    true -> if
              NumFriends == 0 ->%%chiamo il nodo prof
                global:send(teacher_node, {get_friends, PID, make_ref()});
              true ->
                R = rand:uniform(NumFriends),
                lists:nth(R, Friends) ! {get_friends, PID, make_ref()}
            end
  end.


%% c(test). test:test().  test:start().