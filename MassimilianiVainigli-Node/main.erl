%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------
-module(main).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-export([ test/0, test2/0]).
-import(support, [sleep/1, flatten/1, all_elements_are_different/2, get_first_elements/2, index_of_block/2]).
-import(friends, [check_nodes/4]).
-import(manager, [manager/7]).
-import(blockChain, [block_chain/3]).


%inizio
start() ->
  Parent = self(),
  Checker = spawn(fun() -> check_nodes(Parent, [],0, []) end),
  Chain = spawn(fun() -> block_chain(Parent, [], []) end),
  sleep(2),
  manager(Checker, Chain, 0, [], [], [], [])
.


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