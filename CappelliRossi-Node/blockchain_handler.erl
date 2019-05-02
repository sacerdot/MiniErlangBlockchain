-module(blockchain_handler).
-export([safeDeleteTransactions/4, find_transazioni_minate/2, rilevato_blocco_sconosciuto/6, isLonger/2, blockChainReconstruction/5, sendReceivePreviousMessages/3, main_attore_get_head/2]).

% caso: ricerca di una transazione in un blocco della blockchain vuota
find_tr_minateR(Block_transactions,_) when length(Block_transactions) =:= 0 ->
  false;
% caso: ricerca della transazione di testa nella lista di transazioni
find_tr_minateR(Block_transactions,Transactions_list) ->
  [Block_Transaction_Head | Tail]  = Block_transactions,
  case lists:member(Block_Transaction_Head, Transactions_list) of
    true -> true;
    false -> find_tr_minateR(Tail,Transactions_list)
  end.
% funzione con eccezioni per cercare se almeno una delle transazioni del blocco
% è tra le prime 10 transazioni della lista di transazioni
find_transazioni_minate(Transactions_list, Block_transactions) ->
  try
    find_tr_minateR(Block_transactions, lists:sublist(Transactions_list, 1, 10))
  catch
    true -> true
  end.

% Eliminazione delle transazioni contenute nel blocco che stiamo aggiungendo 
% alla nostra catena. Se il minatore è morto lo attiviamo togliendo dalla lista
% le transazioni del blocco. Se il minatore sta minado occorre verificare se 
% almeno una delle transazioni del blocco è in fase di mining, in questo caso
% occorre fermare il minatore e riavviarlo con la lista di transazioni
% aggiornata. La funzione restituisce la nuova lista di transazioni
safeDeleteTransactions(Transactions_list, Block_transactions, BlockChain, PID) ->
  case whereis(minerCR) of
    % caso in cui il minatore è morto: eliminazione delle transazioni dalla
    % lista e avvio del minatore
    undefined ->
      io:format("minatore morto~n"),
      miner:call_miner(PID, BlockChain, Transactions_list -- Block_transactions),
      Transactions_list -- Block_transactions;
    % caso in cui il minaore è vivo
    PID_miner ->
      io:format("minatore vivo~n"),
      case find_transazioni_minate(Transactions_list, Block_transactions) of
        % caso in cui una transazione dal eliminare sta venendo minata
        true ->
          io:format("minatore da far ripartire~n"),
          exit(PID_miner, kill),
          mainActorCR:sleep(0.5),
          miner:call_miner(PID, BlockChain, Transactions_list -- Block_transactions);
        % caso in cui si possono eliminare le transazioni senza interferire con
        % il minatore
        false ->
          ignore
      end,
      Transactions_list -- Block_transactions
  end.

% funzione che calcola l'indice del blocco passato come primo input nella lista
% passata come secondo input
index_of(_, [], _) -> not_found;
index_of(ID, [ID | _], Index) -> Index;
index_of(ID, [_|Tail], Index) -> index_of(ID, Tail, Index+1).

% Funzione che controlla quale catena è più lunga tra quella ricostruita e la
% nostra fino ad arrivare al blocco in comune. Se la nostra sotto-catena è più
% lunga si restituisce true, altrimenti si restituisce l'indice del primo blocco
% da sostituire
isLonger(BlockChain, BlocksToAdd_list) ->
  % recupero il blocco radice della lista ricostruita e, da questo, il blocco in 
  % comune alle due liste
  case lists:last(BlocksToAdd_list) of
    % la catena ricostruita termina con none quindi le due catene non hanno un
    % blocco in comune e la nostra è più lunga di quella ricostruita
    {_, none, _, _} when length(BlockChain) > length(BlocksToAdd_list) ->
      true;
    % la catena ricostruita termina con none quindi le due catene non hanno un
    % blocco in comune e la catena ricostruita è più lunga della nostra  
    {_, none, _, _} ->
      length(BlockChain);
    % la catena ricostruita termina con un blocco. Verifichiamo che questo abbia
    % come predecessore un blocco della nostra catena. Tale blocco è comune alle
    % due catene 
    {_, ID_blocco, _, _} ->
      case get_previous_handler:find_BlockE(ID_blocco, BlockChain) of
        % non c'è un blocco in comune -> la nostra catena è più lunga
        false -> true;
        % troviamo il blocco in comune -> cerchiamo il suo indice e confrontiamo 
        % le due lunghezze
        Block -> 
          Index = index_of(Block, BlockChain, 1), 
          case Index-1 > length(BlocksToAdd_list) of
            % la nostra catena è più lunga
            true -> true;
            % la nostra catena è più corta: Index-1 è l'indice del primo blocco da
            % sostituire
            false -> Index-1
          end
      end
  end.

% Algortimo di ricostruzione della catena. E' costituito da 4 fasi:
%   1) Si rimettono le transazioni dei blocchi da scartare nella lista delle
%      transazioni (senza duplicati)
%   2) Dalla lista di transazioni ottenuta si tolgono le transazioni dei blocchi
%      da aggiungere alla catena (utilizzando safeDeleteTransaction)
%   3) Attraverso la funzione safeDeleteTransactions si capisce se occorre o meno
%      avviare il miner
%   4) Si sostituiscono i blocchi vecchi con quelli nuovi
blockChainReconstruction(BlockChain, BlocksToAdd_list, Transactions_list, Index, PID) ->
  % memorizzo le tranzazioni dei blocchi da scartare e le transazioni dei blocchi
  % da aggiungere
  BlockToDelete_transactions = lists:flatten([Tr || {_,_,Tr,_} <- lists:sublist(BlockChain, 1, Index)]),
  BlockToAdd_transactions = lists:flatten([Tr || {_,_,Tr,_} <- BlocksToAdd_list]),
  % fase 1: inserisco in testa a Transactions_list le transazioni dei blocchi da
  % scartare
  SumTransactions = BlockToDelete_transactions ++ (Transactions_list -- BlockToDelete_transactions),
  % fasi 2-3-4: con safeDeleteTransactions si eliminano le transazioni dei
  % blocchi da aggiungere dalla lista di transazioni verificando cosa fare del
  % minatore (avviarlo, riavviarlo, non fare nulla, ...). Si restituiscono al
  % chiamante le liste di blocchi e di transazioni aggiornate
  New_BlockChain = BlocksToAdd_list ++ BlockChain -- lists:sublist(BlockChain, 1, Index),
  New_Transactions_list = safeDeleteTransactions(SumTransactions, BlockToAdd_transactions, New_BlockChain, PID),
  {New_BlockChain, New_Transactions_list}.

% la funzione viene invocata dall'attore principale ogni volta che arriva un
% blocco sconosciuto. Si verifica che il blocco sia corretto e, in caso
% affermativo, si decide se: aggiungere il blocco in testa, scartare il blocco,
% avviare l'attore per la ricostruzione della catena.
rilevato_blocco_sconosciuto(Main_Actor_Pid,Sender,Friends_list,Block,Transactions_list,BlockChain) ->
  % verifica del blocco
  case Block of
    {_, ID_previous_block, Block_transactions, Solution} ->
      case proof_of_work:check({ID_previous_block, Block_transactions}, Solution) of
        % blocco "falso": la verifica ha fallito -> scarto il blocco
        false -> 
          io:format("il blocco risulta falso~n"),
          {Transactions_list, BlockChain};
        % la verifica del blocco ha avuto successo:
        %   - invio il blocco al get_previous_handler per vedere se qualcuno
        %     lo sta attendendo
        %   - si invia il blocco a tutti gli amici
        %   - si controlla se aggiungere il blocco in coda, scartarlo o se
        %     occorre ricostruire la catena
        true ->
          io:format("il blocco risulta vero~n"),
          get_previous_handler ! {block_added, Block},
          mainActorCR:sendMessageToAllFriends({update, Main_Actor_Pid, Block}, Friends_list),
          % Si confronta l'id dell'ultimo blocco della catena con il  
          % predecessore del blocco arrivato: in caso affermativo si 
          % aggiunge il blocco in testa alla blockchain, in caso negativo
          % occorre capire se ricostruire la catena o scartare il blocco 
          case mainActorCR:retreive_ID_blocco_testa(BlockChain) =:= ID_previous_block of
            true ->
              % si eliminano dalla lista di transazioni quelle contenute nel
              % blocco da aggiungere e si aggiunge il blocco in testa
              io:format("il blocco ricevuto si puo' aggiungere in coda~n"),
              New_trList = safeDeleteTransactions(Transactions_list, Block_transactions, BlockChain, Main_Actor_Pid),
              {New_trList, [Block | BlockChain]};
            false ->
              % controllo se l'id_previous_block del blocco che ci è
              % arrivato è l'id di un blocco della nostra catena o none. In 
              % caso affermativo il blocco arrivato è da scartare, in
              % caso negativo occorre ricostruire la catena
              io:format("il blocco ricevuto non si puo' aggiungere in coda~n"),
              case (ID_previous_block =/= none) and (length([ID_block || {ID_block,_,_,_} <- BlockChain, ID_block =:= ID_previous_block]) =:= 0) of
                % blocco da scartare
                false ->
                  io:format("il blocco e' da scartare~n"),
                  {Transactions_list, BlockChain};
                % ricostruzione catena: si delega un attore per la ricostruzione
                true ->
                  io:format("ricostruzione catena in corso...~n"),
                  P = spawn(fun() -> main_attore_ricostruzione_catena(Main_Actor_Pid, Sender, Block, BlockChain) end),
                  register(prova, P),
                  io:format("prova ~p created~n", [P]),
                  {Transactions_list, BlockChain}
              end
          end
      end;
    _ ->
      io:format("il blocco risulta falso~n"),
      {Transactions_list, BlockChain}
  end.

% Funzione eseguita quando, con un messaggio update, arriva un blocco di cui
% non si conosce l'ID_blocco_precedente. Si inviano messaggi get_previous a
% colui che ha inviato il messaggio update in modo da ricostruire la catena dei
% blocchi che non si conosce. La funzione è ricorsiva.
sendReceivePreviousMessages(Sender, BlockChain, BlocksToAdd_list) ->
  % Si invia il messaggio get_previous per ottenere il blocco sconosciuto che
  % precede quello arrivato
  {_, ID_blocco_sconosciuto, _, _} = lists:last(BlocksToAdd_list),
  Nonce = make_ref(),
  Sender ! {get_previous, self(), Nonce, ID_blocco_sconosciuto},
  io:format("Processo ~p in attesa del blocco sconosciuto...~n", [self()]),
  % si attende di ricevere il blocco in questione
  receive
    {previous, _, {ID_block, ID_previous_block, Block_transactions, Solution}} ->
      % si verifica di aver ricevuto il blocco corretto che si stava aspettando
      case (proof_of_work:check({ID_previous_block, Block_transactions}, Solution)) and (ID_block =:= ID_blocco_sconosciuto) of
        % se si riceve un blocco non corretto si scarta l'intera catena che si 
        % sta ricostruendo
        false -> [];
        % il blocco è quello corretto. Lo si invia al get_previous_handler. Si
        % verifica di conoscere il blocco precedente a quello arrivato
        true ->
          get_previous_handler ! {block_added, {ID_block, ID_previous_block, Block_transactions, Solution}},
          case (length([ID || {ID,_,_,_} <- BlockChain, ID =:= ID_previous_block]) =:= 0) and (ID_previous_block =/= none) of
            % non si conosce il blocco precedente a quello arrivato. Si esegue
            % nuovamente la funzione per ottenere il suo precedente aggiungendo
            % il blocco ricevuto in coda alla catena che si sta ricostruendo
            true ->
              sendReceivePreviousMessages(Sender, BlockChain, BlocksToAdd_list ++ [{ID_block, ID_previous_block, Block_transactions, Solution}]);
            % si conosce il blocco precedente a quello arrivato -> si restituisce
            % la lista costruita
            false ->
              BlocksToAdd_list ++ [{ID_block, ID_previous_block, Block_transactions, Solution}]
          end
      end
  after 30000 -> ko
  end.

% Codice dell'attore che ha il compito di ricostruire la catena
main_attore_ricostruzione_catena(PID_attore_principale, Sender, Block, BlockChain) ->
  % ricostruzione catena
  BlocksToAdd_list = sendReceivePreviousMessages(Sender, BlockChain, [Block]),
  io:format("Catena ricostruita:~n~p~n", [BlocksToAdd_list]),
  % verifica lunghezza catena ricostruita. Se è 0 significa che la ricostruzione
  % è fallita, quindi si termina senza fare nulla. In caso contrario si invia la
  % catena all'attore principale
  case length(BlocksToAdd_list) of
    0 -> ignore;
    _ -> PID_attore_principale ! {new_chain, BlocksToAdd_list}
  end.

main_attore_get_head(PID_attore_principale, Friends_list) ->
    Random_friend = lists:nth(rand:uniform(length(Friends_list)), Friends_list),
    Nonce = make_ref(),
    Random_friend ! {get_head, self(), Nonce},
    receive
      {head, Nonce, {ID_block, ID_previous_block, Tr_block_list, Sol}} ->
        PID_attore_principale ! {update, Random_friend, {ID_block, ID_previous_block, Tr_block_list, Sol}}
    end.