-module(testFP).
-export([test1/0, minimalTest/0, testBlockKillPID3/0, sendTransaction/2,
  testMining2PrevNone/0, testGossip1/0, testGossip/0, testFork/0,
  testBlockANDtransactionDouble/0, testNoFollowers/0, testRebuild/0]).



compileModule() ->
  compile:file(teacher_node),
  compile:file(main),
  compile:file(topologyFP),
  compile:file(blockChain),
  spawn(teacher_node, main, []),
  main:sleep(3).

test1() ->
  io:format("Start ~n"),
  compileModule(),
  TempPid1 = spawn(main, init, []),
  sendTransaction(TempPid1, pid1__transazione1),
  main:sleep(15),
  sendTransaction(TempPid1, pid1__transazione2),
  main:sleep(15),
  sendTransaction(TempPid1, pid1__transazione3),
  main:sleep(15),
  sendTransaction(TempPid1, pid1__transazione4),
  main:sleep(15),
  sendTransaction(TempPid1, pid1__transazione5),
  main:sleep(15),
  sendTransaction(TempPid1, pid1__transazione6),
  sendTransaction(TempPid1, pid1__transazione6END).

spawn5() ->
  io:format("Start ~n"),
  compileModule(),
  TempPid1 = spawn(main, init, []),
  TempPid2 = spawn(main, init, []),
  TempPid3 = spawn(main, init, []),
  TempPid4 = spawn(main, init, []),
  main:sleep(2),
  TempPid5 = spawn(main, init, []),
  io:format("SPAWN +++++++++++++++++ 5 NODI----------------------------------------->~n"),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

minimalTest() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = spawn5(),
  main:sleep(20),
  exit(TempPid1, kill),
  main:sleep(5),
  exit(TempPid2, kill),
  main:sleep(5),
  exit(TempPid3, kill),
  main:sleep(10),
  exit(TempPid4, kill),
  main:sleep(15),
  exit(TempPid5, kill).

testBlockKillPID3() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = testGossip(),
  main:sleep(5),
  sendTransaction(TempPid3, transazione6),
  main:sleep(2),
  sendTransaction(TempPid4, transazione7),
  main:sleep(1),
  sendTransaction(TempPid4, transazione8),
  main:sleep(3),
  sendTransaction(TempPid1, transazione9),

  io:format("--------------------Sleep 60-------------------------------~n"),
  main:sleep(60),
  io:format("--------------------KILL TempPid3-------------------------------~n"),
  exit(TempPid3, kill),

  main:sleep(1),
  sendTransaction(TempPid5, transazione10),
  sendTransaction(TempPid4, transazione11),
  main:sleep(1),
  sendTransaction(TempPid1, transazione12),
  sendTransaction(TempPid4, transazione13),
  main:sleep(1),
  sendTransaction(TempPid2, testBlockKillPID3ENDtransazione100),
  {TempPid1, TempPid2, TempPid4, TempPid5}.

sendTransaction(Pid, Payload) ->
  io:format("INVIO transazione------------:::::::::::::::::::::::::::::::-->~p~n", [Payload]),
  Pid ! {push, {make_ref(), Payload}}.

testMining2PrevNone() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = testGossip1(),
  main:sleep(30),
  io:format("pid5__transazione5~n~n~n~n~n~n~n~n~n~n~n~n~n ~n"),
  sendTransaction(TempPid5, pid5__transazionepid5),
  main:sleep(30),
  sendTransaction(TempPid1, pid1_dopo5),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

testGossip1() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = spawn5(),
  sendTransaction(TempPid1, pid1__transazione1),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

testGossip() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = spawn5(),
  main:sleep(7),
  main:sleep(2),
  sendTransaction(TempPid2, pid2__transazione2),
  sendTransaction(TempPid3, pid3__transazione3),
  sendTransaction(TempPid4, pid4__transazione4),
  main:sleep(10),
  sendTransaction(TempPid2, pid2__transazione5),
  main:sleep(1),
  sendTransaction(TempPid2, pid2__testGossipENDtransazione100),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

testFork() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = testMining2PrevNone(),
  main:sleep(30),
  io:format("--------------------------------------------~n~n~n~n~n~n~n~n~n~n~n~n~n ~n"),
  sendTransaction(TempPid1, pid1__transazione2),
  sendTransaction(TempPid1, pid1__transazione3),
  sendTransaction(TempPid1, pid1__transazione4),
  sendTransaction(TempPid1, pid1__transazione5),
  sendTransaction(TempPid1, pid1__transazione6),
  sendTransaction(TempPid1, pid1__transazione7),
  sendTransaction(TempPid1, pid1__transazione8),
  sendTransaction(TempPid1, pid1__transazione9),
  sendTransaction(TempPid1, pid1__transazione10),
  sendTransaction(TempPid1, pid1__transazione11),
  sendTransaction(TempPid1, pid1__transazione12),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

testBlockANDtransactionDouble() ->
  {TempPid1, TempPid2, TempPid4, TempPid5} = testBlockKillPID3(),
  main:sleep(1),
  sendTransaction(TempPid5, transazione10),
  sendTransaction(TempPid1, transazione14),
  sendTransaction(TempPid5, transazione17),
  sendTransaction(TempPid4, transazione13),
  main:sleep(3),
  sendTransaction(TempPid5, transazione10),
  sendTransaction(TempPid4, transazione11),
  main:sleep(1),
  sendTransaction(TempPid1, transazione12),
  sendTransaction(TempPid4, transazione13),
  sendTransaction(TempPid5, transazione15),
  main:sleep(1),
  sendTransaction(TempPid2, transazione16),
  sendTransaction(TempPid2, transazione16),
  sendTransaction(TempPid5, transazione20),
  main:sleep(5),
  sendTransaction(TempPid5, transazione17),
  main:sleep(2),
  sendTransaction(TempPid1, transazione18),
  main:sleep(5),
  sendTransaction(TempPid2, transazione19),
  sendTransaction(TempPid5, transazione20),
  sendTransaction(TempPid2, transazione16),
  main:sleep(10),
  sendTransaction(TempPid2, eNDtransazione100).

testNoFollowers() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = testGossip1(),
  main:sleep(30),
  io:format("pid5__transazione5~n~n~n~n~n~n~n~n~n~n~n~n~n ~n"),
  sendTransaction(TempPid1, pid1__transazionepid5),
  main:sleep(10),
  sendTransaction(TempPid1, pid21_dopo5),
  main:sleep(20),
  io:format("No Followers~n~n~n~n~n~n~n~n~n~n~n~n~n ~n"),
  main:sleep(15),
  sendTransaction(TempPid1, pid31_dopo5),
  main:sleep(3),
  sendTransaction(TempPid1, 'ho comprato il latte'),
  sendTransaction(TempPid1, endMess),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

testRebuild() ->
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5} = spawn5(),
  main:sleep(7),
  io:format("pid5__transazione5~n~n~n~n~n~n~n~n~n~n~n~n~n ~n"),
  sendTransaction(TempPid1, pid1__transazionepid5),
  main:sleep(10),
  sendTransaction(TempPid1, pid21_dopo5),
  main:sleep(15),
  sendTransaction(TempPid5, pid31_dopo5),
  main:sleep(3),
  sendTransaction(TempPid1, endMess),
  {TempPid1, TempPid2, TempPid3, TempPid4, TempPid5}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cmd test
%% c(main).
%% testFP:sendTransaction(<0.130.0>, ciao111111333333311).
%% exit(<0.85.0>, kill).





%% net_adm:ping('docente@andrea-X550JX').
%% spawn(main, main, []).
%% spawn(teacher_node, main, []).
%% testFP:test1().
%% testFP:compileM().
%% testFP:testGossip1().
%% testFP:testMining2PrevNone().
%% testFP:testFork().
%% testFP:testBlockKillPID3().
%% testFP:testBlockANDtransactionDouble().
%% testFP:testNoFollowers().
%% testFP:testRebuild().
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test terminati
%% topologia amici testata e funzionante
%% gossiping testato e funzionante
%% mining blocco
%% update della visione della catena
%% transazioni ripetute nei blocchi
%% algoritmo di ricostruzione della catena
%% NoFollowers
%% testare perdita di messaggi e arrivo di messaggi doppi
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quando ricevo un blocco che non conosco creo un loop (effettuando il gossiping) finchè qualcuno non mina
%% un nuovo blocco con almeno 1 transazione del blocco gossippato in Loop. Ciò avviene specialmente quando 1 nodo
%% è isolato dalla rete ovvero quando non ha followers e riceve una transazione, quindi mina e ritrasmette il
%% blocco che nessuno inserirà nella catena perchè già più lunga o semplicemente perchè le transazioni sono già
%% presenti, quindi lo ritrasmette continuamente creando il loop solo se le transazioni allinterno non sono presenti nella blockChain,
%% ciò è garantito perchè se parte delle transazioni del nuovo blocco è contenuta già nella BlockChain ritardo il
%% gossiping sino alla terminazione dell'algoritmo di ricostruzione della catena, gossippando solo se accetto la catena.