%%%-------------------------------------------------------------------
%%% @author andrea
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. apr 2019 14.47
%%%-------------------------------------------------------------------
-module(testFP).
-author("andrea").
-export([test/0, minimalTest/0, stressfulTest/0, testX/0, testBlock/0]).

initTest() ->
  compile:file(teacher_node),
  compile:file(nodeFP),
  compile:file(topologyFP),
  compile:file(blockChain),
  spawn(teacher_node, main, []),
  nodeFP:sleep(3).

test() ->
  initTest(),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []).

testBlock() ->
  io:format("Start ~n"),
  initTest(),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  TempPid3 = spawn(nodeFP, init, []),
  TempPid4 = spawn(nodeFP, init, []),
  nodeFP:sleep(2),
  TempPid5 = spawn(nodeFP, init, []),
  io:format("spawn TempPid5 e invio TRANSAZIONI----------------------------------------->~n"),
  TempPid1 ! {push, {make_ref(), transazione1}},
  nodeFP:sleep(7),
  TempPid1 ! {push, {make_ref(), transazione2}},
  TempPid2 ! {push, {make_ref(), transazione3}},
  TempPid4 ! {push, {make_ref(), transazione4}},
  TempPid4 ! {push, {make_ref(), transazione5}},
  TempPid2 ! {push, {make_ref(), transazione6}},
  nodeFP:sleep(5),
  TempPid3 ! {push, {make_ref(), transazione7}},
  nodeFP:sleep(15),
  nodeFP:sleep(20),
  TempPid4 ! {push, {make_ref(), transazione8}},
  TempPid1 ! {push, {make_ref(), transazione9}},
  TempPid3 ! {push, {make_ref(), transazione10}},
  TempPid4 ! {push, {make_ref(), transazione11}},
  TempPid2 ! {push, {make_ref(), transazione12}},
  TempPid1 ! {push, {make_ref(), transazione13}},
  nodeFP:sleep(3),
  TempPid4 ! {push, {make_ref(), transazione14}},
  TempPid2 ! {push, {make_ref(), transazione15}},
  TempPid1 ! {push, {make_ref(), transazione16}},
  nodeFP:sleep(5),
  TempPid4 ! {push, {make_ref(), transazione20}},
  TempPid2 ! {push, {make_ref(), transazione21}},
  nodeFP:sleep(1),
  TempPid5 ! {push, {make_ref(), transazione22}},
  TempPid1 ! {push, {make_ref(), transazione23}},
  io:format("--------------------Sleep 60-------------------------------~n"),
  nodeFP:sleep(60),
  io:format("--------------------KILL TempPid3-------------------------------~n"),
  exit(TempPid3, kill),
  TempPid4 ! {push, {make_ref(), transazione81}},
  TempPid1 ! {push, {make_ref(), transazione91}},
  TempPid5 ! {push, {make_ref(), transazione101}},
  TempPid4 ! {push, {make_ref(), transazione111}},
  TempPid2 ! {push, {make_ref(), transazione121}},
  TempPid5 ! {push, {make_ref(), transazione131}},
  nodeFP:sleep(3),
  TempPid4 ! {push, {make_ref(), transazione141}},
  TempPid2 ! {push, {make_ref(), transazione115}},
  TempPid1 ! {push, {make_ref(), transazione161}},
  nodeFP:sleep(5),
  TempPid4 ! {push, {make_ref(), transaszione201}},
  TempPid2 ! {push, {make_ref(), transazione211}},
  nodeFP:sleep(1),
  TempPid5 ! {push, {make_ref(), tsransazione221}},
  TempPid1 ! {push, {make_ref(), transazione231}},
  nodeFP:sleep(3),
  TempPid4 ! {push, {make_ref(), transaz1ione14}},
  TempPid5 ! {push, {make_ref(), transa1zione15}},
  TempPid1 ! {push, {make_ref(), transazi1one16}},
  nodeFP:sleep(5),
  TempPid4 ! {push, {make_ref(), transazi1one20}},
  TempPid2 ! {push, {make_ref(), transazio1ne21}},
  nodeFP:sleep(1),
  TempPid5 ! {push, {make_ref(), transazio1ne22}},
  TempPid1 ! {push, {make_ref(), trans1azione23}},
  nodeFP:sleep(25),
  TempPid4 ! {push, {make_ref(), tra1nsazione8}},
  TempPid5 ! {push, {make_ref(), transa1zione9}},
  TempPid5 ! {push, {make_ref(), tran1sazione10}},
  TempPid4 ! {push, {make_ref(), trans1azione11}},
  TempPid2 ! {push, {make_ref(), transazio1ne12}},
  TempPid1 ! {push, {make_ref(), transaz1ione13}},
  nodeFP:sleep(3),
  TempPid4 ! {push, {make_ref(), transa1zione14}},
  TempPid2 ! {push, {make_ref(), transa1zione15}},
  TempPid1 ! {push, {make_ref(), transa1zione16}}
.


minimalTest() ->
  initTest(),
  spawn(nodeFP, init, []),
  nodeFP:sleep(11),
  TempPid = spawn(nodeFP, init, []),
  nodeFP:sleep(5),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  nodeFP:sleep(20),
  exit(TempPid, kill),
  exit(TempPid1, kill),
  nodeFP:sleep(5),
  exit(TempPid2, kill).

stressfulTest() ->
  test(),
  stressfulTestLoop().

stressfulTestLoop() ->
  nodeFP:sleep(3),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  stressfulTestLoop().


%% c(testFP). testFP:test().  exit(<0.71.0>, kill). spawn(nodeFP, init, []).
%% testFP:minimalTest().
%% testFP:testX().
%% c(testFP).testFP:testBlock().
%% testFP:testBlock().
%% exit(<0.68.0>, kill).
%% exit(<0.69.0>, kill).
%% exit(<0.70.0>, kill).
%% spawn(nodeFP, init, []).


testX() ->
  BlockChain = [{2, 5, fsdf, sads}, {5, 2, adsd, sad}, {7, 2, adsd, sad}, {10, 2, adsd, sad}],

  case index_of(7, BlockChain) of
    not_found -> do_nothing;
    N -> ok
  end.



index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _) -> not_found;
index_of(Item, [{Item, _, _, _} | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).