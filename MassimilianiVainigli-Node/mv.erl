%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------
-module(mv).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-export([test/0, test_node/0]).

sleep(N) -> receive after N*1000 -> ok end.

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

%% P = spawn(mv, test_node, []).
%% P1 = spawn(mv, test_node, []).
%% P ! {ping, P1, 0}.

test_node() ->
  receive
    {ping, Sender, Ref} -> Sender ! {pong, Ref} ;
    {pong, Ref} -> io:format("Pong!") ;
    {friends, _, Nodes} ->
      io:format("~p nodes discovered~n", [length(Nodes)])
  after 300000 -> teacher_node ! {get_friends, self(), make_ref()}
  end,
  test_node ().

test() ->
  spawn(teacher_node, main, []), % teacher node
  T1 = spawn(fun test_node/0),
  spawn(fun test_node/0),
  spawn(fun test_node/0),
  sleep(6),
  exit(T1,nasty).