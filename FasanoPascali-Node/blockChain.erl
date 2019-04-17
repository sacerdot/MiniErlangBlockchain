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
-export([managerTransactions/4, managerBlock/4]).


%% todo testare tutto
%% todo update della visione della catena
%% todo algoritmo di ricostruzione della catena
%% todo mining blocco


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


%%Blocco= {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni, Soluzione}
%%Soluzione= proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni})
%%proof_of_work:check({IDblocco_precedente,Lista_di_transazioni}, Soluzione)

%% gestisce i blocchi
managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, BlockChain) ->
  receive
    {update, Sender, {IDBlock, IDPreviousBlock, BlockTransactions, Solution}} ->
      Block = {IDBlock, IDPreviousBlock, BlockTransactions, Solution},
      case index_of(IDBlock, BlockChain) of
        not_found -> case proof_of_work:check({IDPreviousBlock, BlockTransactions}, Solution) of
                       false ->
                         do_nothing;
                       true ->
                         PIDManagerFriends ! {gossipingMessage, {update, PIDMain, Block}} %% ritrasmetto agli amici
%%            fate update della vostra visione della catena, eventualmente usando
%%            l'algoritmo di ricostruzione della catena (chiedendo al Sender o agli amici) e
%%            decidendo quale è la catena più lunga

                     end;
        N -> do_nothing
      end;

    {get_previous, Sender, Nonce, IdBlockPrevious} ->
      case index_of(IdBlockPrevious, BlockChain) of
        not_found -> do_nothing;
        N -> Sender ! {previous, Nonce, lists:nth(N, BlockChain)}
      end;

    {previous, Nonce, Block} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce},
      receive
        {nonce, false, TempNonce} -> false;
        {nonce, ok, TempNonce} -> ok %% todo

      after 5000 -> self() ! {previous, Nonce, Block}
      end;

    {get_head, Sender, Nonce} ->
      Sender ! {head, Nonce, lists:nth(length(BlockChain), BlockChain)};

    {head, Nonce, Block} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce},
      receive
        {nonce, false, TempNonce} -> false;
        {nonce, ok, TempNonce} -> ok %% todo

      after 5000 -> self() ! {head, Nonce, Block}
      end
  end,
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, BlockChain).


index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _) -> not_found;
index_of(Item, [{Item, _, _, _} | _], Index) -> Index;
%%index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).

mining(PIDManagerTransactions, PIDManagerBlocks) ->
  Nonce = make_ref(),
  PIDManagerTransactions ! {getTransactionsToMine, self(), Nonce},
  receive
    {transactionsToMine, Nonce, TransactionsToMine} ->
      Nonce2 = make_ref(),
      PIDManagerBlocks ! {get_head, self(), Nonce2},
      receive
        {head, Nonce2, Block} ->
          IDHeadBlock = element(1,Block),
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
    _ ->  I = rand:uniform(length(PoolTransactions)),
      NewFriend = lists:nth(I, PoolTransactions),
      getNRandomFriend(TransactionsChosen ++ [NewFriend], PoolTransactions -- [NewFriend], N - 1)
  end.
