-module(teacher_node).
-export([main/0, test/0]).

% This is the teacher node, the one responsible
% for letting new nodes in the blockchain.
%
% It implements the topology maintainance algorithm
%
% It registers itself globally under the name teacher_node

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
  receive
    {ping, Sender, Ref} ->
       Sender ! {pong, Ref} ,
       loop(Nodes) ;
    {get_friends, Sender, Nonce} ->
       New_nodes =
          case lists:member(Sender,Nodes) of
             true -> Nodes ;
             false ->
                io:format("New node ~p~n",[Sender]),
                Self = self(),
                spawn(fun () -> watch(Self,Sender) end),
                [Sender|Nodes]
          end,
       Sender ! {friends, Nonce, New_nodes},
       loop(New_nodes) ;
    {dead, Node} ->
       io:format("Dead node ~p~n",[Node]),
       loop(Nodes -- [Node])
  end.

main() ->
  register(teacher_node,self()),
  global:register_name(teacher_node,self()),
  io:format("teacher_node registered~n"),
  loop([]).

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

test_node() ->
   receive
      {ping, Sender, Ref} -> Sender ! {pong, Ref} ;
      {friends, _, Nodes} ->
         io:format("~p nodes discovered~n", [length(Nodes)])
   after 3000 -> teacher_node ! {get_friends, self(), make_ref()}
   end,
   test_node ().

test() ->
  spawn(fun main/0),
  T1 = spawn(fun test_node/0),
  spawn(fun test_node/0),
  spawn(fun test_node/0),
  sleep(6),
  exit(T1,nasty).
