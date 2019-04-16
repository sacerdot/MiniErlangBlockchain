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
-export([test/0, minimalTest/0, stressfulTest/0, testX/0]).

%% todo lanciare transazioni

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