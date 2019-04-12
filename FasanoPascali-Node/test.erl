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
  io:format("ping da ~p a ~p~n", [self(),Node]),
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
  global:send(teacher_node, {get_friends, self(), make_ref()}), %% chiedo al nodo teacher la lista di tutti i nodi nella rete
  start([]).

start(Friends) ->
  PID=self(),
  receive
    {friends, Nonce, ListFriends} ->
      io:format("friend receive ~p ~p~n", [PID,Nonce]),
      NewFriends = ext3Friends(ListFriends, Friends++[PID], Friends),
      [spawn(fun() -> watch(PID, X) end) || X <- NewFriends],
%%      io:format("List3Friends~p~p      ~n", [self(),NewFriends]),
      NewListFriends = Friends ++ NewFriends,
      io:format("friend receive ~p ~p~n", [PID,length(NewListFriends)]),
      spawn(test, getNewFriends, [NewListFriends, PID]),
      start(NewListFriends);

    {ping, Sender, Ref} ->
      Sender ! {pong, Ref};

    {get_friends, Sender, Nonce} ->
      Sender ! {friends, make_ref(), Friends};

    {dead, Node} ->
      io:format("Dead node ~p, da ~p~n",[Node, PID]),
      FriendsLess = Friends -- [Node],
      spawn(test, getNewFriends, [FriendsLess, PID]),
      start(FriendsLess)
  end,
  start(Friends).

%% ListFriends: lista dei nodi ricevuti da un amico o dal teacher
%% MyListFriends: i miei amici più me stesso
%% InitFriends: i miei amici (è inserito per evitare di spawn nodi monitor già esistenti)
ext3Friends(ListFriends, MyListFriends, InitFriends) ->  %%  when ListFriends=:= _:_
  %%crea una lista con tutti i valori di ListFriends escluso i valori in MyListFriends
  FilterListFriends = lists:filter(fun(Elem) -> not lists:member(Elem, MyListFriends) end, ListFriends),
  LfilterFriends = length(FilterListFriends),
  if
    LfilterFriends < 1 -> lists:sublist(MyListFriends, length(MyListFriends) - 1) -- InitFriends;
    true ->
      R = rand:uniform(length(FilterListFriends)),
      Llist3Friends = length(MyListFriends) - 1,
      if
        Llist3Friends < 3 -> ext3Friends(FilterListFriends, [lists:nth(R, FilterListFriends)] ++ MyListFriends, InitFriends);
        true -> lists:sublist(MyListFriends, length(MyListFriends)-1) -- InitFriends
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


%% c(test). test:test().  test:start(). exit(<0.71.0>, kill).