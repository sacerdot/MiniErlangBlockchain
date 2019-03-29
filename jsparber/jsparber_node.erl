-module(jsparber_node).
-export([main/0, test/0]).

% This is blockchain node, based on teacher_node.

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 -> Main ! {dead, Node}
  end.

loop(Nodes) ->
  % TODO: ask teacher_node for friends 
  if 
    length(Nodes) == 0 ->
      ask_teacher();
    length(Nodes) < 3 ->
      io:format("I only have ~p friends. Ask a friend for more friends~n", [length(Nodes)]),
      lists:nth(rand:uniform(length(Nodes)), Nodes) ! {get_friends, self(), make_ref()};
    true ->
      true
  end,
  receive
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref} ,
      loop(Nodes) ;
    {get_friends, Sender, Nonce} ->
      New_nodes = add_nodes(Sender, Nodes),
      Sender ! {friends, Nonce, New_nodes},
      loop(New_nodes) ;
    {friends, _Nonce, Incomming_nodes} ->
      New_nodes = add_nodes(Incomming_nodes, Nodes),
      io:format("~p nodes discovered~n", [length(New_nodes)]),
      % ask teacher for more friends if we didn't get enough from a friend
      if 
        length(New_nodes) < 3 ->
          ask_teacher();
        true ->
          true
      end,
      loop(New_nodes);
    {dead, Node} ->
      io:format("Dead node ~p~n",[Node]),
      loop(Nodes -- [Node])
  end.

ask_teacher() -> 
      io:format("Ask teacher for more friends~n"),
      {teacher_node, 'teacher_node@librem'} ! {get_friends, self(), make_ref()}.
add_nodes([], Nodes) -> Nodes;
add_nodes([H|T], Nodes) -> 
  if 
    self() /= H ->
      case lists:member(H, Nodes) of
        true -> add_nodes(T, Nodes);
        false ->
          io:format("New node ~p~n",[H]),
          Self = self(),
          spawn(fun () -> watch(Self, H) end),
          add_nodes(T, [H|Nodes])
      end;
    true -> 
      add_nodes(T, Nodes)
  end.

main() ->
  %register(jsparber_node, self()),
  %global:register_name(jsparber_node, self()),
  io:format("A new jsparber_node registered~n"),
  loop([]).

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

test() ->
  spawn(fun main/0).
