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
%%Soluzione= proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni})
%%proof_of_work:check({IDblocco_precedente,Lista_di_transazioni}, Soluzione)


%% Attualmente il mining riparte esclusivamente se accetto un blocco o se accetto una catena diversa dalla mia che ha generato un fork


%% gestisce le transazioni
managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions, TransactionsInBlocks) ->
  io:format("~p ->+++++++ managerTransactions  ~nPoolTransactions ~p ~n  TransactionsInBlocks~p ~n", [self(), PoolTransactions, TransactionsInBlocks]),
  receive
    {push, Transaction} ->
      case lists:member(Transaction, PoolTransactions) or lists:member(Transaction, TransactionsInBlocks) of
        false ->
%%          io:format("~p ->push Transaction ~p ~n", [PIDMain, Transaction]),
          PIDGossipingMessage ! {gossipingMessage, {push, Transaction}},%% ritrasmetto agli amici
          NewTransactions = PoolTransactions ++ [Transaction],
          managerTransactions(PIDMain, PIDGossipingMessage, NewTransactions, TransactionsInBlocks);
        true -> do_nothing
      end;
    {pop, Transactions} ->
%%      io:format("~p -> pop ~n", [PIDMain]),
      managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions--Transactions, TransactionsInBlocks ++ Transactions);

    {updateTransactions, TransactionsToRemove, TransactionsToAdd} ->
%%      io:format("~p -> updateTransactions ~n", [PIDMain]),
      managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions ++ TransactionsToAdd--TransactionsToRemove,
        TransactionsInBlocks--TransactionsToAdd ++ TransactionsToRemove);

    {getTransactionsToMine, PIDSender, Nonce} ->
%%      io:format("~p -> getTransactionsToMine ~n", [PIDMain]),
      TransactionsChosen = case length(PoolTransactions) of
                             N when N > 10 -> getNRandomTransactions([], PoolTransactions, 10);
                             _ -> PoolTransactions
                           end,
      PIDSender ! {transactionsToMine, Nonce, TransactionsChosen}
  end,
  managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions, TransactionsInBlocks).


initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, PIDMining, NewBlock, Sender) ->
  io:format("~p -> initRebuildBlockChain NewBlock ----> ~p ~n", [PIDManagerBlock, NewBlock]),
  TempNonceF = make_ref(),
  PIDManagerFriends ! {get_friends, self(), TempNonceF},
  receive
    {friends, TempNonceF, Friends} ->
      FriendsPlusSender = case Sender of
                            PIDManagerBlock -> Friends;
                            PIDMining -> Friends; %%???????non so se serve rivedere
                            _ -> Friends ++ [Sender]
                          end,
      rebuildBlockChain(PIDManagerBlock, [NewBlock], FriendsPlusSender, 0)
  after 10000 -> initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, PIDMining, NewBlock, Sender)
  end.

%% InitIndex è necessario per cambiare amico a cui chiedere il precedente se quello con cui stiamo comunicando non risonde più perchè morto o perchè non ha il blocco
rebuildBlockChain(PIDManagerBlock, NewBlockChain, FriendsPlusSender, InitIndex) ->
  io:format("~p -> rebuildBlockChain NewBlockChain ---------> ~p ~n", [PIDManagerBlock, NewBlockChain]),
  TempNonceF = make_ref(),
  Index = case InitIndex of %%corrisponde a modulo 4 + 1
            4 -> 1;
            _ -> InitIndex + 1
          end,
  lists:nth(Index, FriendsPlusSender) ! {get_previous, self(), TempNonceF, element(2, lists:nth(1, NewBlockChain))},
  receive
    {previous, TempNonceF, BlockPrevious} ->
      TempNewBlockChain = [BlockPrevious] ++ NewBlockChain,
      TempNonceB = make_ref(),
      PIDManagerBlock ! {isBlockOfBlockChain, TempNewBlockChain, self(), TempNonceB},
      receive
        {not_found, TempNonceB} -> rebuildBlockChain(PIDManagerBlock, TempNewBlockChain, FriendsPlusSender, Index - 1);
        {stopRebuild, TempNonceB} -> do_nothing
      end
  after 10000 -> rebuildBlockChain(PIDManagerBlock, NewBlockChain, FriendsPlusSender, Index)
  end.

%% gestisce i blocchi
initManagerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage) ->
  PIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, []).

managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain) ->
  process_flag(trap_exit, true),
  MyPid = self(),
  case BlockChain of %%todo eliminare dopo test
    [_ | _] ->
      io:format("~p ->MANAGER_BLOCK: ~p length: ~p Head: ~p !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n", [PIDMain, MyPid, length(BlockChain), lists:nth(length(BlockChain), BlockChain)]);
    _ -> do_nothing
  end,
  receive
    {update, Sender, {IDBlock, IDPreviousBlock, BlockTransactions, Solution}} ->
      io:format("~p-> Receive update ~n", [PIDMain]),
      case BlockTransactions of
        [] -> do_nothing;
        _ ->
          Block = {IDBlock, IDPreviousBlock, BlockTransactions, Solution},
          case Sender of
            PIDMining -> case equalsPrevious(IDPreviousBlock, BlockChain) of
                           true ->
                             io:format("--------------------> Blocco minato da :::~p::: IDBlock->~p ~n", [PIDMain, IDBlock]),
                             PIDManagerTransactions ! {pop, BlockTransactions},
                             PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, Block}},
%%                             NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
                             managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain ++ [Block]);
                           false ->
                             io:format("--------------------> Blocco minato da :::~p::: IDBlock->~p MA SCARTATO~n", [PIDMain, Block]),
%%                             NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
                             managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain)
                         end;
            _ ->
              case checkBlockUpdate(Block, BlockChain) of
                true -> io:format("~p -> checkBlockUpdate ------->>>>> TRUE ~n", [PIDMain]),
                  case checkTransactionsToBlockChain(BlockTransactions, BlockChain) of
                    false ->
                      %% non effettuo il gossiping, nel caso accetto la catena ricostruita farò il gossiping della testa
                      io:format("~p -> checkTransactionsToBlockChain --> FALSE ---", [PIDMain]),
                      %% se IDPreviousBlock è none e la BlockChain /= [] non accetto il blocco mentre
                      %% se IDPreviousBlock è none e la BlockChain == [] non entrerà mai in questo ramo negli altri casi dovrà
                      %% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
                      case equalsPrevious(IDPreviousBlock, BlockChain) of
                        true -> io:format("equalsPrevious---> TRUE  ~n"),
                          %% vuol dire che si attacca alla testa della mia BlockChain ma contiene transazioni già presenti in altri blocchi
                          do_nothing;
                        false -> io:format("equalsPrevious---> FALSE  ~n"),
                          %%ricostruzione della catena (chiedendo al Sender o agli amici)
                          io:format("/////////////////////////////////~p -> Case prev. NO none and IDPreviousBlock!!!!= head->BlockChain ~n", [PIDMain]),
                          spawn_link(blockChain, initRebuildBlockChain, [MyPid, PIDManagerFriends, PIDMining, Block, Sender])
                      end;
                    true ->
                      case Sender of
                        MyPid -> do_nothing; %% solo caso che mi ritrasmetto la testa della
                        _ ->
                          PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, Block}} %% ritrasmetto agli amici
                      end,
                      case IDPreviousBlock of
                        none ->%%when BlockChain =:= []
                          io:format("~p -> Case prev. NONE and BlockChain=:= [] ~n", [PIDMain]),
                          PIDManagerTransactions ! {pop, BlockTransactions},
                          exit(PIDMining, kill),
%%                          NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
                          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, [Block]);
                        _ ->
                          case equalsPrevious(IDPreviousBlock, BlockChain) of %% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
                            true ->
                              io:format("~p -> Case prev. no none and IDPreviousBlock== head->BlockChain ~n", [PIDMain]),
                              PIDManagerTransactions ! {pop, BlockTransactions},
                              exit(PIDMining, kill),
%%                              NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
                              managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain ++ [Block]);
                            false -> %%ricostruzione della catena (chiedendo al Sender o agli amici)
                              io:format("/////////////////////////////////~p -> Case prev. no none and IDPreviousBlock!!!!= head->BlockChain ~n", [PIDMain]),
                              spawn_link(blockChain, initRebuildBlockChain, [MyPid, PIDManagerFriends, PIDMining, Block, Sender])
                          end
                      end
                  end;
                false ->
                  io:format("~p -> checkBlockUpdate ------->>>>> FALSE ~n", [PIDMain]),
                  do_nothing
              end
          end
      end;

    {get_previous, Sender, Nonce, IdBlockPrevious} ->
      case indexOfBlockToBlockChain(IdBlockPrevious, BlockChain) of
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
                        _ -> case indexOfBlockToBlockChain({IdPrevious, none, none, none}, BlockChain) of
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
%%          NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, NewBlockChain)
      end;
    {'EXIT', Pid, Reason} ->
      io:format("Kill ~p Reason ~p~n", [Pid, Reason]),
      case Pid of
        PIDMining ->
          NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, MyPid]),
          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, BlockChain);
        _-> do_nothing
      end
  end,
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain).

%% NON accetta un blocco se lo conosco, se non passa il proof_of_work:check e se [IDPreviousBlock == none && la BlockChain == []]
checkBlockUpdate({IDBlock, IDPreviousBlock, BlockTransactions, Solution}, BlockChain) ->
  case indexOfBlockToBlockChain(IDBlock, BlockChain) of
    not_found ->
      case proof_of_work:check({IDPreviousBlock, BlockTransactions}, Solution) of
        false -> false;
        true ->
          case IDPreviousBlock of
            none when BlockChain /= [] ->
              io:format("~p -> Case prev. NONE do_nothing~n", [self()]),
              false;
            _ -> true
          end
      end;
    _ -> false
  end.

%% ritorna la catena più lunga, aggiunge le transazioni della catena scartata e rimuove le transazioni della nuova catena dalla pool transazioni
establishLongestChain(ManagerTransaction, BlockChain, NewBlockChain, PointFork) ->
  LengthBC = lists:length(BlockChain),
  LengthNewBC = lists:length(NewBlockChain),
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

extractTransaction(BlockChain) -> extractTransaction(BlockChain, []).
extractTransaction([], Transactions) -> Transactions;
extractTransaction([H | T], Transactions) ->
  NewTransactions = Transactions ++ element(3, H),
  extractTransaction(T, NewTransactions).

indexOfBlockToBlockChain(Item, List) -> indexOfBlockToBlockChain(Item, List, 1).
indexOfBlockToBlockChain(_, [], _) -> not_found;
indexOfBlockToBlockChain(Item, [{Item, _, _, _} | _], Index) -> Index;
indexOfBlockToBlockChain(Item, [_ | Tl], Index) -> indexOfBlockToBlockChain(Item, Tl, Index + 1).

equalsPrevious(none, []) -> true;
equalsPrevious(Item, BlockChain) -> equalsPrevious(check, Item, lists:nth(length(BlockChain), BlockChain)).
equalsPrevious(check, Item, {Item, _, _, _}) -> true;
equalsPrevious(check, _, _) -> false.

mining(PIDManagerTransactions, PIDManagerBlocks) ->
  nodeFP:sleep(rand:uniform(5)),%%todo rand time for test
  MyPid = self(),
  io:format("PID_block ~p PidMining ~p -> ----------------------------START MINING--------------------------------------- ~n", [PIDManagerBlocks, self()]),
  Nonce = make_ref(),
  PIDManagerTransactions ! {getTransactionsToMine, MyPid, Nonce},
  receive
    {transactionsToMine, Nonce, TransactionsToMine} ->
      case TransactionsToMine of
        [] ->
%%          io:format("~p ->STOP--------------------------------------- ~n", [PIDManagerBlocks]),
          nodeFP:sleep(rand:uniform(5)), %%todo rand time for test
          mining(PIDManagerTransactions, PIDManagerBlocks);
        _ ->
          Nonce2 = make_ref(),
          PIDManagerBlocks ! {get_head, MyPid, Nonce2},
          receive
            {head, Nonce2, Block} ->
%%              io:format("~p ->CONTINUE head----------------------~p ----------------- ~n", [PIDManagerBlocks, {head, Nonce2, Block}]),
              IDPreviousBlock = case Block of
                                  none -> none;
                                  _ -> element(1, Block)
                                end,
              Solution = proof_of_work:solve({IDPreviousBlock, TransactionsToMine}),
              PIDManagerBlocks ! {update, MyPid, {make_ref(), IDPreviousBlock, TransactionsToMine, Solution}}
%%              ,io:format("~p ->CONTINUE SEND++++----------------------~p ----------------- ~n", [PIDManagerBlocks, {make_ref(), IDPreviousBlock, TransactionsToMine, Solution}])
          end
      end
  end.

managerHead(MainPID) ->
  receive
    {pong, Sender, TeacherPID} when Sender /= TeacherPID ->
      do_nothing
  after 60000 ->
    MainPID ! {maybeNoFollowers}
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

%%Controlla che nessuna delle transazioni sia contenuta nella BlockChain rispondendo true se è non sono contenute false altrimenti
checkTransactionsToBlockChain(_, []) -> true;
checkTransactionsToBlockChain(Transactions, [H | T]) ->
  case checkTransactionsToTransactions(Transactions, element(3, H)) of
    true -> checkTransactionsToBlockChain(Transactions, T);
    false -> false
  end.
checkTransactionsToTransactions([], _) -> true;
checkTransactionsToTransactions([H | T], BlockTransactions) ->
  case indexOfTransactionToTransactions(H, BlockTransactions) of
    not_found -> checkTransactionsToTransactions(T, BlockTransactions);
    _ -> false
  end.
indexOfTransactionToTransactions(Item, List) -> indexOfTransactionToTransactions(Item, List, 1).
indexOfTransactionToTransactions(_, [], _) -> not_found;
indexOfTransactionToTransactions(Item, [Item | _], Index) -> Index;
indexOfTransactionToTransactions(Item, [_ | Tl], Index) -> indexOfTransactionToTransactions(Item, Tl, Index + 1).