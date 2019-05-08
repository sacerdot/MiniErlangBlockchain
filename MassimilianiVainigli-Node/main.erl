%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------
-module(main).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-export([ test/0, test2/0, test3/0, compile/0]).
-import(support, [sleep/1, flatten/1, all_elements_are_different/2, get_first_elements/2, index_of_block/2]).
-import(friends, [check_nodes/4]).
-import(manager, [manager/7]).
-import(block_chain, [block_chain/3]).


%inizio
start() ->
  Parent = self(),
  Checker = spawn(fun() -> check_nodes(Parent, [],0, []) end),
  Chain = spawn(fun() -> block_chain(Parent, [], []) end),
  sleep(2),
  manager(Checker, Chain, 0, [], [], [], [])
.

compile() ->
  compile:file('block_chain.erl'),
  compile:file('friends.erl'),
  compile:file('manager.erl'),
  compile:file('support.erl'),
  compile:file('../teacher_node.erl'),
  compile:file('../proof_of_work.erl').

test() ->
  spawn(teacher_node, main, []), % teacher node
  sleep(2), % waiting teacher
  PID1 = spawn(fun() -> start() end),
  PID2 = spawn(fun() -> start() end),
  PID3 = spawn(fun() -> start() end),
  PID2 ! {push, {1} },
  PID3 ! {push, {2} },
  sleep(2),
  PID2 ! {push, {3} },
  sleep(2),
  PID3 ! {push, {4} },
  sleep(5),

  [Friend ! {stampa} || Friend <-  [PID1]++[PID2]++[PID3] ],
  sleep(1000).



test2() ->
  spawn(teacher_node, main, []), % teacher node
  sleep(2), % waiting teacher

  PID1 = spawn(fun() -> start() end),
  PID2 = spawn(fun() -> start() end),
  PID3 = spawn(fun() -> start() end),
  PID5 = spawn(fun() -> start() end),
  sleep(2),
  PID2 ! {push, {1} },
  sleep(2),
  PID2 ! {push, {2} },
  sleep(2),
  PID1 ! {push, {3} },
  sleep(2),
  PID3 ! {push, {4} },
  sleep(2),
  sleep(5),

  [Friend ! {stampa} || Friend <-  [PID5]],
  sleep(1000).

test3() ->
  spawn(teacher_node, main, []), % teacher node
  sleep(2), % waiting teacher
  PID1 = spawn(fun() -> start() end),
  PID2 = spawn(fun() -> start() end),
  PID3 = spawn(fun() -> start() end),
  PID4 = spawn(fun() -> start() end),
  PID5 = spawn(fun() -> start() end),
  PID6 = spawn(fun() -> start() end),
  PID7 = spawn(fun() -> start() end),
  PID8 = spawn(fun() -> start() end),
  sleep(3),
  PID1 ! {push, {"Uno"} },
  sleep(1),
  PID2 ! {push, {"Due"} },
  sleep(1),
  PID3 ! {push, {"Tre"} },
  sleep(1),
  PID4 ! {push, {"Quattro"} },
  sleep(1),
  PID5 ! {push, {"Cinque"} },
  sleep(1),
  PID6 ! {push, {"Sei"} },
  sleep(1),
  PID7 ! {push, {"Sette"} },
  sleep(1),
  PID8 ! {push, {"Otto"} },
  io:format("Gossiping...~n"),
  Receiver = spawn(fun() -> receive_blockchain() end),
  sleep(10),
  io:format("~n~nAfter 20 seconds:~n"),
  [Friend ! {Receiver, send_blockchain} || Friend <- [PID1]++[PID2]++[PID3]++[PID4]++[PID5]++[PID6]++[PID7]++[PID8]].


receive_blockchain() ->
  receive
    {Sender, blockchain, List_blocks} ->
      io:format("~p: ~p ~n", [Sender, lists:map(fun(Block) -> element(3, Block) end, List_blocks)])
  end,
  receive_blockchain().