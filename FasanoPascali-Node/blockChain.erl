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

%% todo testare tutto ciò nella lista sotto:
  %% + update della visione della catena
  %% +- algoritmo di ricostruzione della catena
  %% + mining blocco
  %% se non ricevo blocchi per X tempo ciedo la testa todo non ancora integrato
  %% transazioni ripetute nei blocchi

%%todo non gossipa bene


%% gestisce le transazioni
managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions, TransactionsInBlocks) ->
  io:format("~p ->managerTransactions  ~nPoolTransactions ~p ~n  TransactionsInBlocks~p ~n", [PIDMain, PoolTransactions, TransactionsInBlocks]),
  io:format("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++~n"),
  receive
    {push, Transaction} ->
      case lists:member(Transaction, PoolTransactions) or lists:member(Transaction, TransactionsInBlocks) of
        false ->
          io:format("~p ->push Transaction ~p ~n", [PIDMain, Transaction]),
          PIDGossipingMessage ! {gossipingMessage, {push, Transaction}},%% ritrasmetto agli amici
          NewTransactions = PoolTransactions ++ [Transaction],
          managerTransactions(PIDMain, PIDGossipingMessage, NewTransactions, TransactionsInBlocks);
        true -> do_nothing
      end;
    {pop, Transactions} ->
      managerTransactions(PIDMain, PIDGossipingMessage, PoolTransactions--Transactions, TransactionsInBlocks ++ Transactions);

    {updateTransactions, TransactionsToRemove, TransactionsToAdd} ->
      io:format("~p -> updateTransactions ~n", [PIDMain]),
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


initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, PIDMining, NewBlock, Sender) ->
  io:format("~p -> initRebuildBlockChain NewBlock ----> ~p ~n", [PIDManagerBlock, NewBlock]),
  TempNonceF = make_ref(),
  PIDManagerFriends ! {get_friends, self(), TempNonceF},
  receive
    {friends, TempNonceF, Friends} ->
      FriendsPlusSender = case Sender of
                            pidSelf -> Friends;
                            PIDMining -> Friends;
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
  io:format("~p -> MANAGER BLOCK -> BlockChain~n~p ~n", [PIDMain, BlockChain]),
  receive
    {update, Sender, {IDBlock, IDPreviousBlock, BlockTransactions, Solution}} ->
      Block = {IDBlock, IDPreviousBlock, BlockTransactions, Solution},
      case Sender of
        PIDMining -> case equalsPrevious(IDPreviousBlock, BlockChain) of
                       true ->
                         io:format("~p -> Blocco minato con successo Block->~p ~n", [PIDMain, Block]),
                         PIDManagerTransactions ! {pop, BlockTransactions},
                         PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, Block}},
                         NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
                         managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, BlockChain ++ [Block]);
                       false ->
                         NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
                         managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, BlockChain)
                     end;
        _ ->
          case index_of(IDBlock, BlockChain) of
            not_found ->
              case proof_of_work:check({IDPreviousBlock, BlockTransactions}, Solution) of
                false ->
                  io:format("~p ->proof_of_work:check ------->>>>> FALSE ~n", [PIDMain]),do_nothing;
                true ->
                  io:format("~p ->proof_of_work:check ------->>>>> TRUE ~n", [PIDMain]),
                  case checkTransactionsToBlockChain(BlockTransactions, BlockChain) of
                    false ->
                      io:format("~p ->checkTransactionsToBlockChain ------->>>>> FALSE ~n", [PIDMain]),
                      do_nothing;
                    true ->
                      io:format("~p -> checkTransactionsToBlockChain ------->>>>> TRUE ~n", [PIDMain]),
                      case Sender of
                        pidSelf -> do_nothing;
                        _ ->
                          PIDGossipingMessage ! {gossipingMessage, {update, PIDMain, Block}} %% ritrasmetto agli amici
                      end,
                      case IDPreviousBlock of
                        none when BlockChain == [] ->
                          PIDManagerTransactions ! {pop, BlockTransactions},
                          exit(PIDMining, kill),
                          NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
                          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, [Block]);
                        none -> do_nothing;
                        _ ->
                          case equalsPrevious(IDPreviousBlock, BlockChain) of %% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
                            true ->
                              PIDManagerTransactions ! {pop, BlockTransactions},
                              exit(PIDMining, kill),
                              NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
                              managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, BlockChain ++ [Block]);
                            false -> %%ricostruzione della catena (chiedendo al Sender o agli amici)
                              spawn_link(blockChain, initRebuildBlockChain, [self(), PIDManagerFriends, PIDMining, Block, Sender])
                          end
                      end
                  end
              end;
            _ -> do_nothing
          end
      end;

    {get_previous, Sender, Nonce, IdBlockPrevious} ->
      case index_of(IdBlockPrevious, BlockChain) of
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
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce},
      receive
        {nonce, false, TempNonce} -> do_nothing;
        {nonce, ok, TempNonce} ->
          case Block of
            none -> do_nothing;
            _ -> self() ! {update, pidSelf, Block}
          end
      after 5000 -> self() ! {head, Nonce, Block}
      end;

    {isForkPoint, NewPartBlockChain, Sender, Nonce} ->
      IdPrevious = element(2, lists:nth(1, NewPartBlockChain)),
      NewBlockChain = case IdPrevious of
                        none ->
                          Sender ! {stopRebuild, Nonce},
                          establishLongestChain(PIDManagerTransactions, BlockChain, NewPartBlockChain, 0);
                        _ -> case index_of({IdPrevious, none, none, none}, BlockChain) of
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
          exit(PIDMining, kill),
          NewPIDMining = spawn_link(blockChain, mining, [PIDManagerTransactions, self()]),
          managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, NewPIDMining, NewBlockChain)
      end
  end,
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransactions, PIDGossipingMessage, PIDMining, BlockChain).


%% ritorna la catena più lunga, aggiunge le transazioni della catena scartata e rimuove le transazioni della nuova catena dalla pool transazioni
establishLongestChain(ManagerTransaction, BlockChain, NewBlockChain, PointFork) ->
  LengthBC = lists:length(BlockChain),
  LengthNewBC = lists:length(NewBlockChain),
  if
    LengthNewBC > LengthBC ->
      TransactionsToRemovePool = extractTransaction(lists:sublist(NewBlockChain, PointFork + 1, LengthNewBC - PointFork)),
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

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _) -> not_found;
index_of(Item, [{Item, _, _, _} | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).

equalsPrevious(Item, Block) -> equalsPrevious(check, Item, Block).
equalsPrevious(check, none, []) -> true;
equalsPrevious(check, Item, [{Item, _, _, _} | _]) -> true;
equalsPrevious(check, _, _) -> false.

mining(PIDManagerTransactions, PIDManagerBlocks) ->
  io:format("~p -> ----------------------------START MINING--------------------------------------- ~n", [PIDManagerBlocks]),
  Nonce = make_ref(),
  PIDManagerTransactions ! {getTransactionsToMine, self(), Nonce},
  receive
    {transactionsToMine, Nonce, TransactionsToMine} ->
      case TransactionsToMine of
        [] -> nodeFP:sleep(1), mining(PIDManagerTransactions, PIDManagerBlocks);
        _ ->
          Nonce2 = make_ref(),
          PIDManagerBlocks ! {get_head, self(), Nonce2},
          receive
            {head, Nonce2, Block} ->
              IDPreviousBlock = case Block of
                                  none -> none;
                                  _ -> element(1, Block)
                                end,
              Solution = proof_of_work:solve({IDPreviousBlock, TransactionsToMine}),
              PIDManagerBlocks ! {update, self(), {make_ref(), IDPreviousBlock, TransactionsToMine, Solution}}
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
    true -> checkTransactionsToTransactions(Transactions, T);
    false -> false
  end.

checkTransactionsToTransactions([], _) -> true;
checkTransactionsToTransactions([H | T], BlockTransactions) ->
  case index_of(H, BlockTransactions) of
    not_found -> checkTransactionsToTransactions(T, BlockTransactions);
    _ -> false
  end.


