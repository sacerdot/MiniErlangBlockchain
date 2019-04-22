-module(manager).
-import(support, [get_first_elements/2, send_msg/2]).
-import(blockChain, [mining/3]).

-export([manager/7]).


%% nodo principale, registrato dal nodo professore
% Checker = nodo che controlla gli amici
% BlockChain = nodo che controlla blockChain
% Miner = nodo che fa mining
% List_friends = lista amici
% List_blocks = propria visione della blockChain
% Not insert transactions = transactions ancora da esaminare
% Mining transactions = transactions sotto mining di questo nodo
manager(Checker, BlockChain, Miner, List_friends, List_blocks, Not_inserted_transactions, Mining_transactions) ->


  %% se non sto facendo mining e ho delle trans in attesa faccio mining di esse
  if (length(Not_inserted_transactions) > 0 ) and (Miner =:= 0) ->

    IDBlocco_precedente = if length(List_blocks) > 0 -> element(1,  lists:nth(length(List_blocks), List_blocks)); true -> 0 end,
    Transazioni1 = get_first_elements(Not_inserted_transactions, 10),
    Manager1 = self(),
    PidMiner1 = spawn(fun() -> mining(IDBlocco_precedente, Transazioni1, Manager1) end),
    manager(Checker, BlockChain, PidMiner1, List_friends, List_blocks, Not_inserted_transactions -- Transazioni1, Transazioni1);

    true -> ok
  end,


  receive

    {ping, Sender, Ref2} ->
      Sender ! {pong, Ref2};


  % comunicazione con Checker
    {get_friends, P, Ref} ->
      Checker ! {get_friends, P, Ref};

    {friends, R, Lista} ->
      Checker ! {friends, R, Lista};

    {update_friends, List} ->
      manager(Checker, BlockChain, Miner, List,  List_blocks, Not_inserted_transactions, Mining_transactions);


  % comunicazione con BlockChain
    {newblock, IDBlocco, Lista_di_transazioni, Soluzione} ->
      BlockChain ! {newblock, IDBlocco, Lista_di_transazioni, Soluzione };

    {update, Sender, Blocco} ->
      BlockChain ! {update, Sender, Blocco };

    {updateChain, Chain} ->

      % se mi aggiorno mando messaggio di update agli amici con l'ultimo blocco che ho inserito
      [ send_msg(Friend, {update, self(), lists:nth(length(Chain), Chain)}) || Friend <- List_friends],

      BlockTransactions = lists:map(fun(Bloc) -> lists:nth(1,element(3, Bloc)) end, Chain),
      New_not_inserted_transactions = [X || X <- (Mining_transactions ++ Not_inserted_transactions), not lists:member(X, BlockTransactions)],

      if (Miner =/= 0) -> exit(Miner, kill); true-> ok end,
      manager(Checker, BlockChain, 0, List_friends, Chain, New_not_inserted_transactions, [] ) ;


  %ricostruzione catena
    {get_previous, Mittente, Nonce2, Idblocco_precedente} ->

      Blocco = lists:filter(fun (Blocco) -> element(1, Blocco) == Idblocco_precedente end, List_blocks),
      send_msg(Mittente, {previous, Nonce2, Blocco});

    {get_head, Mittente, Nonce3} ->
      [Blocco | _] = List_blocks,
      Mittente ! {head, Nonce3, Blocco};


  % le transazioni vengono gesite direttamente da questo nodo
    {push, Transazione} ->

      BlockTransactions = lists:map(fun(Block) -> element(3, Block) end, List_blocks),
      IsTransactionKnown = lists:any(fun(E) -> E == Transazione end, BlockTransactions ++ Not_inserted_transactions ++ Mining_transactions),

      if
        IsTransactionKnown -> manager(Checker, BlockChain, Miner, List_friends, List_blocks, Not_inserted_transactions, Mining_transactions);
        true ->
          [send_msg(Friend, {push, Transazione}) || Friend <- List_friends],
          manager(Checker, BlockChain, Miner, List_friends, List_blocks, Not_inserted_transactions ++ [Transazione], Mining_transactions)
      end;

  % stampe
    {stampa_amici} ->
      io:format("Manager: Sono ~p e ho come amici ~p ~n", [self(), List_friends]);

    {stampa_blockChain} ->
      io:format("Sono ~p e la mia catena e' : ~p ~n", [self(), List_blocks]);

    {stampa} ->
      io:format("Sono ~p. ~n Amici: ~p ~n Blocchi ~p ~n Trans non inserite: ~p ~n Sto minando ~p ~n", [self(), List_friends, List_blocks, Not_inserted_transactions, Mining_transactions])



  after 1000 -> manager(Checker, BlockChain, Miner, List_friends,  List_blocks, Not_inserted_transactions, Mining_transactions)

  end,
  manager(Checker, BlockChain, Miner, List_friends,  List_blocks, Not_inserted_transactions, Mining_transactions).
