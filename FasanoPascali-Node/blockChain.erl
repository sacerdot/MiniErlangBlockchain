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
-export([managerTransactions/4, managerBlock/5, initRebuildBlockChain/4]).


%%Blocco= {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni, Soluzione}
%%Soluzione= proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni})
%%proof_of_work:check({IDblocco_precedente,Lista_di_transazioni}, Soluzione)


%% todo testare tutto ciò nella lista sotto:
%%  update della visione della catena
%%  algoritmo di ricostruzione della catena
%%  mining blocco
%%  se non ricevo blocchi per X tempo ciedo la testa


%% gestisce le transazioni
managerTransactions(PIDMain, PIDManagerFriends, PoolTransactions, TransactionsInBlocks) ->
  receive
    {push, Transaction} ->
      case lists:member(Transaction, PoolTransactions) or lists:member(Transaction, TransactionsInBlocks) of
        false ->
          PIDManagerFriends ! {gossipingMessage, {push, Transaction}},%% ritrasmetto agli amici
          NewTransactions = PoolTransactions ++ [Transaction],
          managerTransactions(PIDMain, PIDManagerFriends, NewTransactions, TransactionsInBlocks)
      end;
    {pop, Transactions} ->
      managerTransactions(PIDMain, PIDManagerFriends, PoolTransactions--Transactions, TransactionsInBlocks ++ Transactions);

    {updateTransactions, TransactionsToRemove, TransactionsToAdd} ->
      managerTransactions(PIDMain, PIDManagerFriends, PoolTransactions--TransactionsToRemove ++ TransactionsToAdd,
        TransactionsInBlocks--TransactionsToAdd ++ TransactionsToRemove);

    {getTransactionsToMine, PIDSender, Nonce} ->
      TransactionsChosen = case length(PoolTransactions) of
                             N when N > 10 -> getNRandomTransactions([], PoolTransactions, 10);
                             _ -> PoolTransactions
                           end,
      PIDSender ! {transactionsToMine, Nonce, TransactionsChosen}
  end.


initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, NewBlock, Sender) ->
  TempNonceF = make_ref(),
  PIDManagerFriends ! {get_friends, self(), TempNonceF},
  receive
    {friend, TempNonceF, Friends} ->
      FriendsPlusSender = case Sender of
                            pidSelf -> Friends;
                            managerMining -> Friends;
                            _ -> Friends ++ [Sender]
                          end,
      rebuildBlockChain(PIDManagerBlock, [NewBlock], FriendsPlusSender, 0)
  after 10000 -> initRebuildBlockChain(PIDManagerBlock, PIDManagerFriends, NewBlock, Sender)
  end.

%% InitIndex è necessario per cambiare amico a cui chiedere il precedente se quello con cui stiamo comunicando non risonde più perchè morto o perchè non ha il blocco
rebuildBlockChain(PIDManagerBlock, NewBlockChain, FriendsPlusSender, InitIndex) ->
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
managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, BlockChain) ->
  receive
    {update, Sender, {IDBlock, IDPreviousBlock, BlockTransactions, Solution}} ->
      Block = {IDBlock, IDPreviousBlock, BlockTransactions, Solution},
      case index_of(IDBlock, BlockChain) of
        not_found -> case proof_of_work:check({IDPreviousBlock, BlockTransactions}, Solution) of
                       false -> do_nothing;
                       true ->
                         %% todo kill attore mining

                         case Sender of
                           pidSelf -> do_nothing;
                           true ->PIDManagerFriends ! {gossipingMessage, {update, PIDMain, Block}} %% ritrasmetto agli amici
                         end,
                         case IDPreviousBlock of
                           none when BlockChain == [] ->
                             managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, [Block]);
                           none -> do_nothing;
                           _ ->
                             case equals(IDPreviousBlock, BlockChain) of %% controllo che l'id della testa di BlockChain è uguale a IDPreviousBlock
                               ok ->
                                 managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, BlockChain ++ [Block]);
                               false -> %%ricostruzione della catena (chiedendo al Sender o agli amici)
                                 spawn_link(blockChain, initRebuildBlockChain, [self(), PIDManagerFriends, Block, Sender])
                             end
                         end


                       %% todo start attore mining
                     end;
        N -> do_nothing
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
                          establishLongestChain(PIDManagerTransaction, BlockChain, NewPartBlockChain, 0);
                        _ -> case index_of({IdPrevious, none, none, none}, BlockChain) of
                               not_found ->
                                 Sender ! {not_found, Nonce},
                                 BlockChain;
                               N -> %% trovato punto di fork quindi controlla quale catena è più lunga
                                 Sender ! {stopRebuild, Nonce},
                                 TempNewBlockChain = lists:sublist(BlockChain, N) ++ NewPartBlockChain,
                                 establishLongestChain(PIDManagerTransaction, BlockChain, TempNewBlockChain, 0)
                             end
                      end,
      managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, NewBlockChain)
  end,
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, BlockChain).


%% ritorna la catena più lunga, aggiunge le transazioni della catena scartata e rimuove le transazioni della nuova catena dalla pool transazioni
establishLongestChain(ManagerTransaction, BlockChain, NewBlockChain, PointFork) ->
  LengthBC = lists:length(BlockChain),
  LengthNewBC = lists:length(NewBlockChain),
  if
    LengthNewBC > LengthBC ->
      TransactionsToRemove = extractTransaction(lists:sublist(BlockChain, PointFork + 1, LengthBC - PointFork)),
      TransactionsToAdd = extractTransaction(lists:sublist(NewBlockChain, PointFork + 1, LengthNewBC - PointFork)),
      ManagerTransaction ! {updateTransactions, TransactionsToRemove, TransactionsToAdd},
      NewBlockChain;
    true -> BlockChain
  end.

extractTransaction(BlockChain) -> [todo]



%% todo estrarre e ritornare lista di transazioni
.


index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _) -> not_found;
index_of(Item, [{Item, _, _, _} | _], Index) -> Index;
%%index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).


equals(Item, Block) -> index_of(check, Item, Block).
equals(check, Item, [{Item, _, _, _} | _]) -> ok;
equals(check, Item, [_ | Tl]) -> none.

mining(PIDManagerTransactions, PIDManagerBlocks) ->
  Nonce = make_ref(),
  PIDManagerTransactions ! {getTransactionsToMine, self(), Nonce},
  receive
    {transactionsToMine, Nonce, TransactionsToMine} ->
      Nonce2 = make_ref(),
      PIDManagerBlocks ! {get_head, self(), Nonce2},
      receive
        {head, Nonce2, Block} ->
          IDHeadBlock = element(1, Block),
          Solution = proof_of_work:solve(IDHeadBlock, TransactionsToMine),
          IDNewBlock = make_ref(),
          NewBlock = {IDNewBlock, IDHeadBlock, TransactionsToMine, Solution},
          PIDManagerBlocks ! {update, managerMining, NewBlock}
      end
  end.

managerHead(MainPID) ->
  receive
    {pong, Sender, TeacherPID} when Sender /= TeacherPID ->
      do_nothing
  after 60000 -> MainPID ! {maybeNoFollowers}
  end.


getNRandomTransactions(TransactionsChosen, PoolTransactions, N) ->
  case N of
    N when N =< 0 -> TransactionsChosen;
    _ -> I = rand:uniform(length(PoolTransactions)),
      NewFriend = lists:nth(I, PoolTransactions),
      getNRandomFriend(TransactionsChosen ++ [NewFriend], PoolTransactions -- [NewFriend], N - 1)
  end.
