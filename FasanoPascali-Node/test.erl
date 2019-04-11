-module(test).
-export([test/0, start/0, iter/1]).

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
  spawn(test, start, []),
  spawn(test, start, []),
  spawn(test, start, []),
  spawn(test, start, [])
  .

init() ->
%%      chiedo amici
  global:send(teacher_node, {get_friends, self(), make_ref()}),
  start().

start() ->
  receive
    {friends, _, ListFriends} ->
      io:format("friend receive~n"),
      List3Friends = ext3Friends(ListFriends, [self()]),
      io:format("List3Friends~p~p      ~n", [self(),List3Friends]),
      PID = spawn(test, iter, [List3Friends]),
      [spawn(fun() -> watch(PID, X) end) || X <- List3Friends],
      start();
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref},
      start()
  end.


iter(ListFriends) ->
  receive
    {get_friends, Sender, _} ->
      Sender ! {friends, make_ref(), ListFriends},
      iter(ListFriends);
    {dead, Node, PID} ->
      exit(PID, kill),
      iter(ListFriends)
  %%      chiedo amici
  end.

ext3Friends(ListFriends, List3Friends) ->  %%  when ListFriends=:= _:_
  FilterListFriends = lists:filter(fun(Elem) -> not lists:member(Elem, List3Friends) end, ListFriends),
  LfilterFriends=length(FilterListFriends),
  if
    LfilterFriends< 1 -> lists:sublist(List3Friends, length(List3Friends) - 1);
    true ->
      R = rand:uniform(length(FilterListFriends)),
      Llist3Friends=length(List3Friends)-1,
      if
        Llist3Friends < 3 -> ext3Friends(FilterListFriends, [lists:nth(R, FilterListFriends)] ++ List3Friends);
        true -> lists:sublist(List3Friends, length(List3Friends) - 1)
      end
  end.

%% c(test). test:test().  test:start().