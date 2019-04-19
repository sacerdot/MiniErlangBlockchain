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
-export([compileModule/0, spawn5/0, sendTransaction/2,
  testGossip/0, testBlockKillPID3/0, testBlockANDtransactionDouble/0,
  minimalTest/0, stressfulTest/0]).

compileModule() ->
  compile:file(teacher_node),
  compile:file(nodeFP),
  compile:file(topologyFP),
  compile:file(blockChain),
  spawn(teacher_node, main, []),
  nodeFP:sleep(3).

spawn5() ->
  io:format("Start ~n"),
  compileModule(),
  TempPid1 = spawn(nodeFP, init, []),
  TempPid2 = spawn(nodeFP, init, []),
  TempPid3 = spawn(nodeFP, init, []),
  TempPid4 = spawn(nodeFP, init, []),
  nodeFP:sleep(2),
  TempPid5 = spawn(nodeFP, init, []),
  io:format("SPAWN +++++++++++++++++ 5 NODI----------------------------------------->~n"),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

sendTransaction(Pid, Payload) ->
  io:format("INVIO transazione------------:::::::::::::::::::::::::::::::-->~p~n", [Payload]),
  Pid ! {push, {make_ref(), Payload}}.

testGossip() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = spawn5(),
  nodeFP:sleep(7),
  sendTransaction(TempPid1, pid1__transazione1),
  nodeFP:sleep(2),
  sendTransaction(TempPid2, pid2__transazione2),
  sendTransaction(TempPid3, pid3__transazione3),
  sendTransaction(TempPid4, pid4__transazione4),
  nodeFP:sleep(10),
  sendTransaction(TempPid2, pid2__transazione5),
  nodeFP:sleep(1),
  sendTransaction(TempPid2, pid2__testGossipENDtransazione100),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

testBlockKillPID3() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = testGossip(),
  nodeFP:sleep(5),
  sendTransaction(TempPid3, transazione6),
  nodeFP:sleep(2),
  sendTransaction(TempPid4, transazione7),
  nodeFP:sleep(1),
  sendTransaction(TempPid4, transazione8),
  nodeFP:sleep(3),
  sendTransaction(TempPid1, transazione9),

  io:format("--------------------Sleep 60-------------------------------~n"),
  nodeFP:sleep(60),
  io:format("--------------------KILL TempPid3-------------------------------~n"),
  exit(TempPid3, kill),

  nodeFP:sleep(1),
  sendTransaction(TempPid5, transazione10),
  sendTransaction(TempPid4, transazione11),
  nodeFP:sleep(1),
  sendTransaction(TempPid1, transazione12),
  sendTransaction(TempPid4, transazione13),
  nodeFP:sleep(1),
  sendTransaction(TempPid2, testBlockKillPID3ENDtransazione100),
  {TempPid1, TempPid2, TempPid4, TempPid5}.

testBlockANDtransactionDouble() ->
  {TempPid1, TempPid2, TempPid4, TempPid5} = testBlockKillPID3(),
  nodeFP:sleep(1),
  sendTransaction(TempPid5, transazione10),
  sendTransaction(TempPid1, transazione14),
  sendTransaction(TempPid5, transazione17),
  sendTransaction(TempPid4, transazione13),
  nodeFP:sleep(3),
  sendTransaction(TempPid5, transazione10),
  sendTransaction(TempPid4, transazione11),
  nodeFP:sleep(1),
  sendTransaction(TempPid1, transazione12),
  sendTransaction(TempPid4, transazione13),
  sendTransaction(TempPid5, transazione15),
  nodeFP:sleep(1),
  sendTransaction(TempPid2, transazione16),
  sendTransaction(TempPid2, transazione16),
  sendTransaction(TempPid5, transazione20),
  nodeFP:sleep(5),
  sendTransaction(TempPid5, transazione17),
  nodeFP:sleep(2),
  sendTransaction(TempPid1, transazione18),
  nodeFP:sleep(5),
  sendTransaction(TempPid2, transazione19),
  sendTransaction(TempPid5, transazione20),
  sendTransaction(TempPid2, transazione16),
  nodeFP:sleep(10),
  sendTransaction(TempPid2, eNDtransazione100).

minimalTest() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = spawn5(),
  nodeFP:sleep(20),
  exit(TempPid1, kill),
  nodeFP:sleep(5),
  exit(TempPid2, kill),
  nodeFP:sleep(5),
  exit(TempPid3, kill),
  nodeFP:sleep(10),
  exit(TempPid4, kill),
  nodeFP:sleep(15),
  exit(TempPid5, kill).

stressfulTest() ->
  spawn5(),
  stressfulTestLoop().

stressfulTestLoop() ->
  nodeFP:sleep(3),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  spawn(nodeFP, init, []),
  stressfulTestLoop().

%%<0.93.0>,<0.94.0>,<0.95.0>,<0.96.0>,<0.133.0>

%% c(testFP).
%% testFP:testGossip().
%% testFP:testBlockKillPID3().
%% testFP:testBlockANDtransactionDouble().

%% sendTransaction(<0.82.0>, ciao111111333333311)
%% exit(<0.82.0>, kill).
%% spawn(nodeFP, init, []).

%% testFP:minimalTest().
%% testFP:stressfulTest().





