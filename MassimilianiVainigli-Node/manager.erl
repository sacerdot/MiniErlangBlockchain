-module(manager).
-import(support, [get_first_elements/2, send_msg/2, flatten/1]).
-import(block_chain, [mining/3]).
-export([manager/7]).


%% nodo principale, registrato dal nodo professore%%
% Friendship = nodo che controlla gli amici
% BlockChain = nodo che controlla blockChain
% Miner = nodo che fa mining
% List_friends = lista amici
% List_blocks = propria visione della blockChain
% Not insert transactions = transactions ancora da esaminare
% Mining transactions = transactions sotto mining da questo nodo
manager(Friendship, BlockChain, Miner, List_friends, List_blocks, Not_inserted_transactions, Mining_transactions) ->


  %% se non sto facendo mining e ho delle trans in attesa faccio mining di esse
  if (length(Not_inserted_transactions) > 0 ) and (Miner =:= 0) ->

    % trovo l'ID che dovra essere usato come ID del blocco precedente per il nuovo blocco
    IDBlocco_precedente = if length(List_blocks) > 0 -> element(1,  lists:nth(length(List_blocks), List_blocks)); true -> 0 end,
    % prendo le prime 10 transizioni non ancora inserite
    Transazioni1 = get_first_elements(Not_inserted_transactions, 10),
    Manager1 = self(),
    PidMiner1 = spawn(fun() -> mining(IDBlocco_precedente, Transazioni1, Manager1) end),
    manager(Friendship, BlockChain, PidMiner1, List_friends, List_blocks, Not_inserted_transactions -- Transazioni1, Transazioni1);
    true -> ok
  end,


  receive

  % controlla se sono vivo rispondo con pong
    {ping, Sender, Ref2} ->
      Sender ! {pong, Ref2};

  % messaggio inoltrato
    {get_friends, P, Ref} ->
      Friendship ! {get_friends, P, Ref};

  % messaggio inoltrato
    {friends, R, Lista} ->
      Friendship ! {friends, R, Lista};

  % il Friendship ha inviato la nuova lista amici, mi aggiorno
    {update_friends, List} ->
      manager(Friendship, BlockChain, Miner, List,  List_blocks, Not_inserted_transactions, Mining_transactions);


  % messaggio inoltrato
    {newblock, IDBlocco, Lista_di_transazioni, Soluzione} ->
      BlockChain ! {newblock, IDBlocco, Lista_di_transazioni, Soluzione };

  % messaggio inoltrato
    {update, Sender, Blocco} ->
      BlockChain ! {update, Sender, Blocco };

  % messaggio ricevuto da attore che gestisce blockchain
  % indica che devo aggiornare la mia visione della catena
    {updateChain, Chain} ->

      % se mi aggiorno mando messaggio di update agli amici con l'ultimo blocco che ho inserito
      [ send_msg(Friend, {update, self(), lists:nth(length(Chain), Chain)}) || Friend <- List_friends],


      BlockTransactions = flatten(lists:map(fun(Bloc) -> element(3, Bloc) end, Chain)),
      New_not_inserted_transactions = [X || X <- ( Not_inserted_transactions ++ Mining_transactions), not lists:member(X, BlockTransactions)],

      % se sto facendo maining termino in quanto sicuramente quel blocco dovra essere scartato
      if (Miner =/= 0) -> exit(Miner, kill); true-> ok end,
      % a questo punto aumentano le transazioni non ancora inserite aggiungono le transazioni sotto mining
      manager(Friendship, BlockChain, 0, List_friends, Chain, New_not_inserted_transactions, [] ) ;


  % messaggio ricevuto da attore che vuole ricostruire la catena
    {get_previous, Mittente, Nonce2, Idblocco_precedente} ->

      % invio il blocco richiesto, quello che ha come ID quello richiesto
      Blocco = lists:filter(fun (Blocco) -> element(1, Blocco) == Idblocco_precedente end, List_blocks),
      send_msg(Mittente, {previous, Nonce2, Blocco});

    % messaggio ricevuto da attore che vuole conoscere il mio primo blocco
    {get_head, Mittente, Nonce3} ->
      if(List_blocks =/= []) -> [Blocco | _] = List_blocks;
          true -> Blocco = []
        end
      ,
      send_msg(Mittente, {head, Nonce3, Blocco}) ;



  % le transazioni vengono gesite direttamente da questo nodo
    {push, Transazione} ->

      % ottengo tutte le transazioni
      BlockTransactions = lists:map(fun(Block) -> element(3, Block) end, List_blocks),

      % se eventualmente non sono liste le rendo liste
      Trans_list = if is_list(Transazione) -> Transazione; true -> [Transazione] end,
      BlockTransactions_list = if is_list(BlockTransactions) -> BlockTransactions; true -> [BlockTransactions] end,
      Not_inserted_list =  if is_list(Not_inserted_transactions) -> Not_inserted_transactions; true -> [Not_inserted_transactions] end,
      Mining_list = if is_list(Mining_transactions) -> Mining_transactions; true -> [Mining_transactions] end,

      % Transazioni ricevute meno transazioni già presenti in una delle liste
      NewTransitions = ((flatten(Trans_list) -- flatten(BlockTransactions_list))  -- flatten(Not_inserted_list)) -- flatten(Mining_list),


      if
        length(NewTransitions) == 0 -> ok;
        true ->
          % invio la transazione agli amici
          [send_msg(Friend , {push, Transazione}) || Friend <- List_friends],
          % mi aggiorno considerando la nuova transazione
          manager(Friendship, BlockChain, Miner, List_friends, List_blocks, Trans_list ++ Not_inserted_transactions, Mining_transactions)
      end;

    % for debugging
    {Sender, send_blockchain} ->
      send_msg(Sender, {self(), blockchain, List_blocks});

  % stampe
    {stampa_amici} ->
      io:format("Manager: Sono ~p e ho come amici ~p ~n", [self(), List_friends]);

    {stampa_blockChain} ->
      io:format("Sono ~p e la mia catena e' : ~p ~n", [self(), List_blocks]);

    {stampa} ->
      io:format("Sono ~p. ~n Amici: ~p ~n Blocchi ~p ~n Trans non inserite: ~p ~n Sto minando ~p ~n", [self(), List_friends, List_blocks, Not_inserted_transactions, Mining_transactions])


  after 1000 -> manager(Friendship, BlockChain, Miner, List_friends,  List_blocks, Not_inserted_transactions, Mining_transactions)

  end,
  manager(Friendship, BlockChain, Miner, List_friends,  List_blocks, Not_inserted_transactions, Mining_transactions).



