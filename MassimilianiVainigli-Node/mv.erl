%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------

-module(mv).
-export([test/0, test_node/0]).

sleep(N) -> receive after N*1000 -> ok end.





test_node() ->
  receive
    {ping, Sender, Ref} -> Sender ! {pong, Ref} ;
    {friends, _, Nodes} ->
      io:format("~p nodes discovered~n", [length(Nodes)])
  after 3000 -> teacher_node ! {get_friends, self(), make_ref()}
  end,
  test_node ().




test() ->
  spawn(teacher_node,main,[]), % teacher_node
  T1 = spawn(fun test_node/0),
  spawn(fun test_node/0),
  spawn(fun test_node/0),
  sleep(6),
  exit(T1,nasty).