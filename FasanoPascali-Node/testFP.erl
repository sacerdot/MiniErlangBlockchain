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
-export([test/0, minimalTest/0, stressfulTest/0, testX/0, testBlock/0, testA/0, testB/0]).

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


sendTransactions(Pid, Transaction) ->
  io:format("INVIO transazione------------:::::::::::::::::::::::::::::::-->~p~n", [Transaction]),
  Pid ! {push, {make_ref(), Transaction}}.

testBlock() ->
  io:format("Start ~n"),
  initTest(),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  TempPid3 = spawn(nodeFP, init, []),
  TempPid4 = spawn(nodeFP, init, []),
  nodeFP:sleep(2),
  io:format("SPAWN+++++++++++++++++ TempPid5----------------------------------------->~n"),
  TempPid5 = spawn(nodeFP, init, []),

  nodeFP:sleep(5),

  sendTransactions(TempPid1, transazione1),
  nodeFP:sleep(3),
  sendTransactions(TempPid2, transazione2),
  sendTransactions(TempPid3, transazione3),
  sendTransactions(TempPid4, transazione4),
  nodeFP:sleep(5),
  sendTransactions(TempPid2, transazione5),
  nodeFP:sleep(1),
  sendTransactions(TempPid3, transazione6),
  nodeFP:sleep(2),
  sendTransactions(TempPid4, transazione7),
  nodeFP:sleep(1),
  sendTransactions(TempPid4, transazione8),
  nodeFP:sleep(3),
  sendTransactions(TempPid1, transazione9),

  io:format("--------------------Sleep 60-------------------------------~n"),
  nodeFP:sleep(60),
  io:format("--------------------KILL TempPid3-------------------------------~n"),
  exit(TempPid3, kill),

  nodeFP:sleep(1),
  sendTransactions(TempPid5, transazione10),
  sendTransactions(TempPid4, transazione11),
  nodeFP:sleep(1),
  sendTransactions(TempPid1, transazione12),
  sendTransactions(TempPid4, transazione13),
  nodeFP:sleep(1),
  sendTransactions(TempPid1, transazione14),
  nodeFP:sleep(3),
  sendTransactions(TempPid5, transazione15),
  nodeFP:sleep(1),
  sendTransactions(TempPid2, transazione16),
  nodeFP:sleep(5),
  sendTransactions(TempPid5, transazione17),
  nodeFP:sleep(2),
  sendTransactions(TempPid1, transazione18),
  nodeFP:sleep(5),
  sendTransactions(TempPid2, transazione19),
  sendTransactions(TempPid5, transazione20),
  nodeFP:sleep(1),
  sendTransactions(TempPid1, transazione21),
  nodeFP:sleep(3),
  sendTransactions(TempPid5, transazione22),
  sendTransactions(TempPid5, transazione23),
  sendTransactions(TempPid2, transazione24),
  nodeFP:sleep(1),
  sendTransactions(TempPid2, transazione25),
  sendTransactions(TempPid5, transazione26),
  nodeFP:sleep(2),
  sendTransactions(TempPid4, transazione27),
  sendTransactions(TempPid1, transazione28),
  nodeFP:sleep(5),
  sendTransactions(TempPid5, transazione29),
  sendTransactions(TempPid4, transazione30),
  nodeFP:sleep(10),
  sendTransactions(TempPid2, eNDtransazione100).


testA() ->
  io:format("Start ~n"),
  initTest(),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  TempPid3 = spawn(nodeFP, init, []),
  TempPid4 = spawn(nodeFP, init, []),
  nodeFP:sleep(2),
  io:format("SPAWN+++++++++++++++++ TempPid5----------------------------------------->~n"),
  spawn(nodeFP, init, []),

  nodeFP:sleep(5),

  sendTransactions(TempPid1, transazione1),

  nodeFP:sleep(2),
  sendTransactions(TempPid2, transazione2),
  sendTransactions(TempPid3, transazione3),
  sendTransactions(TempPid4, transazione4),
  nodeFP:sleep(10),
  sendTransactions(TempPid2, transazione5),
  nodeFP:sleep(1),

  sendTransactions(TempPid2, eNDtransazione100).

testB() ->
  io:format("Start ~n"),
  initTest(),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  TempPid3 = spawn(nodeFP, init, []),
  TempPid4 = spawn(nodeFP, init, []),
  nodeFP:sleep(2),
  io:format("SPAWN+++++++++++++++++ TempPid5----------------------------------------->~n"),
  TempPid5 = spawn(nodeFP, init, []),

  nodeFP:sleep(5),

  sendTransactions(TempPid1, transazione1),

  nodeFP:sleep(2),
  sendTransactions(TempPid2, transazione2),
  sendTransactions(TempPid3, transazione3),
  sendTransactions(TempPid4, transazione4),
  nodeFP:sleep(10),
  sendTransactions(TempPid2, transazione5),
  nodeFP:sleep(1),
  sendTransactions(TempPid3, transazione6),
  nodeFP:sleep(2),
  sendTransactions(TempPid4, transazione7),
  nodeFP:sleep(1),
  sendTransactions(TempPid4, transazione8),
  nodeFP:sleep(3),
  sendTransactions(TempPid1, transazione9),

  io:format("--------------------Sleep 60-------------------------------~n"),
  nodeFP:sleep(60),
  io:format("--------------------KILL TempPid3-------------------------------~n"),
  exit(TempPid3, kill),

  nodeFP:sleep(1),
  sendTransactions(TempPid5, transazione10),
  sendTransactions(TempPid4, transazione11),
  nodeFP:sleep(1),
  sendTransactions(TempPid1, transazione12),
  sendTransactions(TempPid4, transazione13),
  nodeFP:sleep(1),
  sendTransactions(TempPid2, eNDtransazione100).

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

testGossip()->
  io:format("Start ~n"),
  initTest(),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  TempPid3 = spawn(nodeFP, init, []),
  TempPid4 = spawn(nodeFP, init, []),
  nodeFP:sleep(1),
  TempPid5 = spawn(nodeFP, init, []),

  sendTransactions(TempPid1, transazione1),

  nodeFP:sleep(2),
  sendTransactions(TempPid2, transazione2),
  sendTransactions(TempPid3, transazione3),
  sendTransactions(TempPid4, transazione4),
  nodeFP:sleep(10),
  sendTransactions(TempPid2, transazione5),
  nodeFP:sleep(1),

  sendTransactions(TempPid2, eNDtransazione100).


%% c(testFP).
%% testFP:testA().
%% testFP:testB().
%% testFP:testBlock().



%% c(testFP). testFP:test().  exit(<0.71.0>, kill). spawn(nodeFP, init, []).
%% testFP:minimalTest().
%% testFP:testX().
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