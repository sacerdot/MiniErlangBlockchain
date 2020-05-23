%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------
-module(main).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-import(support, [sleep/1, flatten/1, all_elements_are_different/2, get_first_elements/2, index_of_block/2]).
-import(teacher_client, [main/0]).
-import(friends, [check_nodes/4]).
-import(manager, [manager/7]).
-import(block_chain, [block_chain/3]).
-export([compile/0, start/0]).


compile() ->
  compile:file('block_chain.erl'),
  compile:file('friends.erl'),
  compile:file('manager.erl'),
  compile:file('support.erl'),
  compile:file('test.erl'),
  compile:file('main.erl'),
  compile:file('../teacher_node.erl'),
  compile:file('../teacher_client.erl'),
  compile:file('../proof_of_work.erl').


% Lancia un singolo nodo della blockchain
% Facendo lo spawn dei vari attori che si occupano dei diversi compiti
start() ->
  Parent = self(),
  Checker = spawn(fun() -> check_nodes(Parent, [],0, []) end),
  Chain = spawn(fun() -> block_chain(Parent, [], []) end),
  sleep(2),
  manager(Checker, Chain, 0, [], [], [], []).


