-module(test).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-import(main, [start/0, start/1]).
-import(support, [sleep/1]).
-export([test/0, test2/0, test3/0]).

% Esegue entrambi i test del teacher client
test() ->
  spawn(teacher_node, main, []), % teacher node
  sleep(2), % waiting teacher
  PID1 = spawn(fun() -> start() end),
  PID2 = spawn(fun() -> start() end),
  PID3 = spawn(fun() -> start() end),
  PID4 = spawn(fun() -> start() end),
  sleep(3),
  teacher_client:main(),
  sleep(20),
  [Friend ! {stampa} || Friend <-  [PID1]++[PID2]++[PID3]++[PID4]].


%%% Altri Test %%%

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
  PID2 ! {push, {30} },
  sleep(2),
  PID1 ! {push, {3} },
  sleep(2),
  PID3 ! {push, {4} },
  sleep(8),
  [Friend ! {stampa} || Friend <-  [PID1]++[PID2]++[PID3]++[PID5]],
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
  PID1 ! {push, {1} },
  sleep(1),
  PID2 ! {push, {2} },
  sleep(1),
  PID3 ! {push, {3} },
  sleep(1),
  PID4 ! {push, {4} },
  sleep(1),
  PID5 ! {push, {5} },
  sleep(1),
  PID6 ! {push, {6} },
  sleep(1),
  PID7 ! {push, {7} },
  sleep(1),
  PID8 ! {push, {8} },
  sleep(15),
  PID9 = spawn(fun() -> start() end),
  sleep(10),
  io:format("~n~nAfter 20 seconds:~n"),
  [Friend !  {stampa_amici} || Friend <- [PID1]++[PID2]++[PID3]++[PID4]++[PID5]++[PID6]++[PID7]++[PID8]++[PID9]],
  [Friend !  {stampa} || Friend <- [PID1]++[PID2]++[PID3]++[PID4]++[PID5]++[PID6]++[PID7]++[PID8]++[PID9]].

