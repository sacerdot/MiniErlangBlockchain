%%%-------------------------------------------------------------------
%%% @author andrea
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. apr 2019 14.58
%%%-------------------------------------------------------------------
-module(blockChain).
-author("andrea").
-export([managerTransactions/4, initManagerBlock/5, initRebuildBlockChain/5, mining/2, managerHead/1]).

%%Blocco= {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni, Soluzione}

%% l'attore istanziato su tale funzione gestisce la Pool delle Transazioni e tutte le richieste inerenti ad essa
managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions, TransactionsInBlocks) ->
%%  io:format("~p ->+++++++ managerTransactions  ~nPoolTransactions ~p ~n  TransactionsInBlocks~p ~n", [self(), PoolTransactions, TransactionsInBlocks]),
  receive
    {push, Transaction} ->
      case lists:member(Transaction, PoolTransactions) or lists:member(Transaction, TransactionsInBlocks) of
        false ->
          PIDGossipingMessage ! {gossipingMessage, {push, Transaction}},%% ritrasmetto agli amici
          NewTransactions = PoolTransactions ++ [Transaction],
          managerTransactions(PIDMain, PIDGossipingMessage, NewTransactions, TransactionsInBlocks);
        true -> do_nothing
      end;
    {pop, Transactions} ->
      managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions--Transactions, TransactionsInBlocks ++ Transactions);

    {updateTransactions, TransactionsToRemove, TransactionsToAdd} ->
      managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions ++ TransactionsToAdd--TransactionsToRemove,
        TransactionsInBlocks--TransactionsToAdd ++ TransactionsToRemove);

    {getTransactionsToMine, PIDSender, Nonce} ->
      TransactionsChosen = case length(PoolTransactions) of
                             N when N > 10 -> getNRandomTransactions([], PoolTransactions, 10);
                             _ -> PoolTransactions
                           end,
      PIDSender ! {transactionsToMine, Nonce, TransactionsChosen}
  end,
  managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions, TransactionsInBlocks).

%%inizializza il rebuild della catena ottenendo gli amici dal manager degli amici e avviando la procedura iterativa di ricostruzione
initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, PIDMining, NewBlock, Sender) ->
  TempNonceF = make_ref(),
  PIDManagerFriends ! {get_friends, self(), TempNonceF},
  receive
    {friends, TempNonceF, Friends} ->
      FriendsPlusSender = case Sender of
                            PIDManagerBlock -> Friends;
                            _ -> Friends ++ [Sender]
                          end,
      rebuildBlockChain(PIDManagerBlock, [NewBlock], FriendsPlusSender, 0)
  after 10000 -> initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, PIDMining, NewBlock, Sender)
  end.

%% InitIndex è necessario per cambiare amico a cui chiedere il precedente se quello con cui stiamo comunicando non risonde più perchè morto o perchè non ha il blocco
rebuildBlockChain(PIDManagerBlock, NewBlockChain, FriendsPlusSender, InitIndex) ->
  TempNonceF = make_ref(),
  TempLength = length(FriendsPlusSender),
  Index = case InitIndex of %%corrisponde a modulo TempLength + 1
            TempLength -> 1;
            _ -> InitIndex + 1
          end,
  lists:nth(Index, FriendsPlusSender) ! {get_previous, self(), TempNonceF, element(2, lists:nth(1, NewBlockChain))},
  receive
    {previous, TempNonceF, BlockPrevious} ->
      case checkTransactionsToBlockChain(element(3, BlockPrevious), NewBlockChain) of
        false ->%%stopRebuild perchè entrando in questa casistica vuol dire che la catena che sto ricostruendo è errata poichè contiene transazioni ripetute
          do_nothing;
        true ->
          TempNewBlockChain = [BlockPrevious] ++ NewBlockChain,
          TempNonceB = make_ref(),
          PIDManagerBlock ! {isForkPoint, TempNewBlockChain, self(), TempNonceB},
          receive
            {not_found, TempNonceB} -> rebuildBlockChain(PIDManagerBlock, TempNewBlockChain, FriendsPlusSender, Index - 1);
            {stopRebuild, TempNonceB} -> do_nothing
          end
      end
  after 10000 -> rebuildBlockChain(PIDManagerBlock, NewBlockChain, FriendsPlusSender, Index)
  end.

%% l'attore istanziato su tale funzione gestisce la BlockChain e tutte le richieste inerenti ad essa
initManagerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage) ->
  PIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, []).

managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain) ->
  process_flag(trap_exit, true),
  MyPid = self(),
  io:format("~p ->MANAGER_BLOCK: ~p length: ~p BlockChain: ~p~n !!!!!!!!!!!!!!!!!!!!!!!~n", [PIDMain, MyPid, length(BlockChain), BlockChain]),
  receive
    {update, Sender, {IDBlock, IDPreviousBlock, BlockTransactions, Solution}} ->
      case BlockTransactions of
        [] -> do_nothing;
        _ ->
          Block = {IDBlock, IDPreviousBlock, BlockTransactions, Solution},
          case Sender of
            PIDMining -> %% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
              case equalsPrevious(IDPreviousBlock, BlockChain) of
                false ->
                  do_nothing;
                true ->
                  case checkTransactionsToBlockChain(BlockTransactions, BlockChain) of
                    true ->
                      PIDManagerTransactions ! {pop, BlockTransactions},
                      PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, Block}},
                      managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain ++ [Block]);
                    false ->
                      do_nothing
                  end
              end;
            _ ->
              %% NON accetta un blocco se lo conosco, se non passa il proof_of_work:check e se [IDPreviousBlock == none && la BlockChain /= []]
              case checkBlockUpdate(Block, BlockChain) of
                true ->
                  case checkTransactionsToBlockChain(BlockTransactions, BlockChain) of
                    false ->%% non effettuo il gossiping, nel caso accetto la catena ricostruita farò il gossiping della testa
                      %% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
                      case equalsPrevious(IDPreviousBlock, BlockChain) of
                        true ->%% vuol dire che si attacca alla testa della mia BlockChain ma contiene transazioni già presenti in altri blocchi
                          do_nothing;
                        false -> %%ricostruzione della catena (chiedendo al Sender o agli amici)
                          spawn_link(blockChain, initRebuildBlockChain, [MyPid, PIDManagerFriends, PIDMining, Block, Sender])
                      end;
                    true ->
                      case Sender of %% effettuo il gossiping
                        MyPid -> do_nothing; %% solo caso che mi ritrasmetto la testa da me stesso
                        _ ->
                          PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, Block}} %% ritrasmetto agli amici
                      end,
                      case IDPreviousBlock of
                        none ->%% se IDPreviousBlock == none && la BlockChain /= [] scarto il blocco in checkBlockUpdate
                          PIDManagerTransactions ! {pop, BlockTransactions},
                          exit(PIDMining, kill),
                          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, [Block]);
                        _ ->%% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
                          case equalsPrevious(IDPreviousBlock, BlockChain) of
                            true ->
                              PIDManagerTransactions ! {pop, BlockTransactions},
                              exit(PIDMining, kill),
                              managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain ++ [Block]);
                            false -> %%ricostruzione della catena (chiedendo al Sender o agli amici)
                              spawn_link(blockChain, initRebuildBlockChain, [MyPid, PIDManagerFriends, PIDMining, Block, Sender])
                          end
                      end
                  end;
                false ->
                  do_nothing
              end
          end
      end;

    {get_previous, Sender, Nonce, IdBlockPrevious} ->
      case indexOfBlock(IdBlockPrevious, BlockChain) of
        not_found -> do_nothing;
        N -> Sender ! {previous, Nonce, lists:nth(N, BlockChain)}
      end;

    {get_head, Sender, Nonce} ->
      Head = case BlockChain of
               [] -> none;
               _ -> lists:nth(length(BlockChain), BlockChain)
             end,
      Sender ! {head, Nonce, Head};

    {head, Nonce, Block} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce, MyPid},
      receive
        {nonce, false, TempNonce} -> do_nothing;
        {nonce, ok, TempNonce} ->
          case Block of
            none -> do_nothing;
            _ -> MyPid ! {update, MyPid, Block}
          end
      after 5000 -> MyPid ! {head, Nonce, Block}
      end;

    {isForkPoint, NewPartBlockChain, Sender, Nonce} ->
      IdPrevious = element(2, lists:nth(1, NewPartBlockChain)),
      NewBlockChain = case IdPrevious of
                        none ->%% trovato punto di fork quindi controlla quale catena è più lunga
                          Sender ! {stopRebuild, Nonce},
                          establishLongestChain(PIDManagerTransactions, BlockChain, NewPartBlockChain, 0);
                        _ -> case indexOfBlock(IdPrevious, BlockChain) of
                               not_found ->
                                 Sender ! {not_found, Nonce},
                                 BlockChain;
                               N -> %% trovato punto di fork quindi controlla quale catena è più lunga
                                 Sender ! {stopRebuild, Nonce},
                                 TempNewBlockChain = lists:sublist(BlockChain, N) ++ NewPartBlockChain,
                                 establishLongestChain(PIDManagerTransactions, BlockChain, TempNewBlockChain, 0)
                             end
                      end,
      case NewBlockChain of
        BlockChain -> do_nothing;
        _ ->
          PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, lists:nth(length(NewBlockChain), NewBlockChain)}}, %% gossiping della testa agli amici
          exit(PIDMining, kill),
          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, NewBlockChain)
      end;
    {'EXIT', Pid, _} ->
      case Pid of
        PIDMining ->
          NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, BlockChain);
        _ -> do_nothing
      end
  end,
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain).

%% NON accetta un blocco se lo conosco, se non passa il proof_of_work:check e se [IDPreviousBlock == none && la BlockChain /= []]
checkBlockUpdate({IDBlock, IDPreviousBlock, BlockTransactions, Solution}, BlockChain) ->
  case indexOfBlock(IDBlock, BlockChain) of
    not_found ->
      case proof_of_work:check({IDPreviousBlock, BlockTransactions}, Solution) of
        false -> false;
        true ->
          case IDPreviousBlock of
            none when BlockChain /= [] ->
              false;
            _ -> true
          end
      end;
    _ -> false
  end.

%% ritorna la catena più lunga, aggiunge le transazioni della catena scartata e rimuove le transazioni della nuova catena dalla pool transazioni
establishLongestChain(ManagerTransaction, BlockChain, NewBlockChain, PointFork) ->
  LengthBC = length(BlockChain),
  LengthNewBC = length(NewBlockChain),
  if
    LengthNewBC > LengthBC ->
      TransactionsToRemovePool = extractTransaction(lists:sublist(NewBlockChain, PointFork + 1, LengthNewBC - PointFork)),
      %% Controlla che le TransactionsToRemovePool, ovvero le transazioni della parte nuova della blockChain non siano contenute nella parte vecchia
      case checkTransactionsToBlockChain(TransactionsToRemovePool, lists:sublist(NewBlockChain, 1, PointFork)) of
        false -> BlockChain;
        true ->
          TransactionsToAddPool = extractTransaction(lists:sublist(BlockChain, PointFork + 1, LengthBC - PointFork)),
          ManagerTransaction ! {updateTransactions, TransactionsToRemovePool, TransactionsToAddPool},
          NewBlockChain
      end;
    true -> BlockChain
  end.

%%estrae la lista di tutte le transazioni contenute in una BlockChain
extractTransaction(BlockChain) -> extractTransaction(BlockChain, []).
extractTransaction([], Transactions) -> Transactions;
extractTransaction([H | T], Transactions) ->
  NewTransactions = Transactions ++ element(3, H),
  extractTransaction(T, NewTransactions).

%%controlla se nella BlockChain è contenuto un blocco con Id uguale a quello ricercato, nel caso affermativo risponde con la posizione del blocco
indexOfBlock(IdBlock, List) -> indexOfBlock(IdBlock, List, 1).
indexOfBlock(_, [], _) -> not_found;
indexOfBlock(IdBlock, [{IdBlock, _, _, _} | _], Index) -> Index;
indexOfBlock(IdBlock, [_ | Tl], Index) -> indexOfBlock(IdBlock, Tl, Index + 1).

%%controlla tramite patter matching se l'id del blocco precedente corrisponde alla testa della BlockChain
equalsPrevious(none, []) -> true;
equalsPrevious(_, []) -> false;
equalsPrevious(IDPreviousBlock, BlockChain) -> equalsPrevious(check, IDPreviousBlock, lists:nth(length(BlockChain), BlockChain)).
equalsPrevious(check, IDPreviousBlock, {IDPreviousBlock, _, _, _}) -> true;
equalsPrevious(check, _, _) -> false.

%%l'attore istanziato su tale funzione si occupa del mining di un nuovo blocco
mining(PIDManagerTransactions, PIDManagerBlocks) ->
%%  nodeFP:sleep(rand:uniform(5)),%% rand time only for test
  MyPid = self(),
%%  io:format("PID_block ~p PidMining ~p -> ----------------------------START MINING--------------------------------------- ~n", [PIDManagerBlocks, self()]),
  Nonce = make_ref(),
  PIDManagerTransactions ! {getTransactionsToMine, MyPid, Nonce},
  receive
    {transactionsToMine, Nonce, TransactionsToMine} ->
      case TransactionsToMine of
        [] ->
          nodeFP:sleep(3),
          mining(PIDManagerTransactions, PIDManagerBlocks);
        _ ->
          Nonce2 = make_ref(),
          PIDManagerBlocks ! {get_head, MyPid, Nonce2},
          receive
            {head, Nonce2, Block} ->
              IDPreviousBlock = case Block of
                                  none -> none;
                                  _ -> element(1, Block)
                                end,
              Solution = proof_of_work:solve({IDPreviousBlock, TransactionsToMine}),
              PIDManagerBlocks ! {update, MyPid, {make_ref(), IDPreviousBlock, TransactionsToMine, Solution}}
          end
      end
  end.

%%lo scopo dell'attore istanziato su tale funzione è quello di tenere la visione della BlockChain sempre aggiornata almeno rispetto ai sui amici
managerHead(MainPID) ->
  receive
    {follower} -> do_nothing
  after 60000 -> MainPID ! {maybeNoFollowers}
  end,
  managerHead(MainPID).

getNRandomTransactions(TransactionsChosen, PoolTransactions, N) ->
  case PoolTransactions of
    [] -> TransactionsChosen;
    _ ->
      case N of
        N when N =< 0 -> TransactionsChosen;
        _ -> I = rand:uniform(length(PoolTransactions)),
          NewTransactions = lists:nth(I, PoolTransactions),
          getNRandomTransactions(TransactionsChosen ++ [NewTransactions], PoolTransactions -- [NewTransactions], N - 1)
      end
  end.

%%Controlla che nessuna delle transazioni sia contenuta nella BlockChain, rispondendo true se non sono contenute false altrimenti
checkTransactionsToBlockChain(_, []) -> true;
checkTransactionsToBlockChain(Transactions, [H | T]) ->
  case checkListsOfTransactions(Transactions, element(3, H)) of
    true -> checkTransactionsToBlockChain(Transactions, T);
    false -> false
  end.
%% funzioni ausiliare per checkTransactionsToBlockChain
checkListsOfTransactions([], _) -> true;
checkListsOfTransactions([H | T], BlockTransactions) ->
  case indexOfTransaction(H, BlockTransactions) of
    not_found -> checkListsOfTransactions(T, BlockTransactions);
    _ -> false
  end.
indexOfTransaction(Transaction, ListTransaction) -> indexOfTransaction(Transaction, ListTransaction, 1).
indexOfTransaction(_, [], _) -> not_found;
indexOfTransaction(Transaction, [Transaction | _], Index) -> Index;
indexOfTransaction(Transaction, [_ | Tl], Index) -> indexOfTransaction(Transaction, Tl, Index + 1).