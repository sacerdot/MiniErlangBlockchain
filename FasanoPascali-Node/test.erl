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
  PID = self(),
  NonceGlobalSend = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  self() ! {updateNonce, NonceGlobalSend},
  GetFriend = spawn(test, getNewFriends, [[], PID, 1]),
  start([], [], [], [], GetFriend).

start(Friends, ListTransazioni, BlockChain, ListNonce, PIDGetFriend) ->
  PID = self(),
  receive
    {friends, Nonce, ListFriends} ->
      io:format("friend receive ~p ~p~n", [PID, Nonce]),
      NewFriends = ext3Friends(ListFriends, Friends ++ [PID], Friends),
      [spawn(fun() -> watch(PID, X) end) || X <- NewFriends],
%%      io:format("List3Friends~p~p      ~n", [self(),NewFriends]),
      NewListFriends = Friends ++ NewFriends,
      if
        length(NewListFriends) >= 3 -> ok;
        true -> PIDGetFriend ! {friend, NewListFriends}
      end,
      start(NewListFriends, ListTransazioni, BlockChain, ListNonce, PIDGetFriend);

    {ping, Sender, Ref} ->
      Sender ! {pong, Ref};

    {push, Transazione} when Transazione == {_,_} ->
      case lists:member(Transazione, ListTransazioni) of
        false -> [X ! {push, Transazione} || X <- Friends],
          NewListTransazioni = ListTransazioni ++ [Transazione],
          start(Friends, NewListTransazioni, BlockChain, ListNonce, PIDGetFriend)
      end;

    {get_friends, Sender, Nonce} ->
      Sender ! {friends, Nonce, Friends};

    {dead, Node} ->
      io:format("Dead node ~p, da ~p~n", [Node, PID]),
      FriendsLess = Friends -- [Node],
      PIDGetFriend ! {friend, FriendsLess},
      start(FriendsLess, ListTransazioni, BlockChain, ListNonce, PIDGetFriend);

    {updateNonce, Nonce} ->
      start(Friends, ListTransazioni, BlockChain, ListNonce ++ [Nonce], PIDGetFriend)
  end,
  start(Friends, ListTransazioni, BlockChain, ListNonce, PIDGetFriend).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  todo gestire casisitica 2 amici
getNewFriends(Friends, PID, Step) ->
  if
    Step == 3 ->
      sendGetFriends(PID, global);
    true ->
      case length(Friends) of
        0 -> sendGetFriends(PID, global);
        1 -> sendGetFriends(PID, lists:nth(1, Friends));
        2 -> sendGetFriends(PID, lists:nth(2, Friends));%%, sendGetFriends(PID, lists:nth(2, Friends))
        _ -> ok
      end
  end,
%% 3 step finale
%%  2 step 2 richeste amici
  case Step of
    2 -> receive
           {friend, NewFriends} -> getNewFriends(NewFriends, PID, 3)
         after 10000 -> getNewFriends(Friends, PID, 0)
         end;
    1 -> receive
           {friend, NewFriends} -> sleep(5), getNewFriends(NewFriends, PID, 2)
         after 10000 -> getNewFriends(Friends, PID, 0)
         end;
    _ -> receive
           {friend, NewFriends} -> getNewFriends(NewFriends, PID, 1)
         end
  end.

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

%% c(test). test:test().  test:start(). exit(<0.71.0>, kill). spawn(test, init, []).