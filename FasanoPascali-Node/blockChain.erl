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
      managerTransactions(PIDMain, PIDManagerFriends, PoolTransactions--TransactionsToRemove++TransactionsToAdd,
        TransactionsInBlocks--TransactionsToAdd++TransactionsToRemove)
  end.


%%Blocco= {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni, Soluzione}
%%Soluzione= proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni})
%%proof_of_work:check({IDblocco_precedente,Lista_di_transazioni}, Soluzione)

%% gestisce i blocchi
managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, BlockChain) ->
  receive
    {update, Sender, Block} ->

%%  todo     controllo se lo conosco
%%         Se non lo conosco chiamo proof_of_work:check()
%%         Se è verificato

      PIDManagerFriends ! {gossipingMessage, {update, PIDMain, Block}} %% ritrasmetto agli amici
%%            fate update della vostra visione della catena, eventualmente usando
%%            l'algoritmo di ricostruzione della catena (chiedendo al Sender o agli amici) e
%%            decidendo quale è la catena più lunga
  ;

    {get_previous, Sender, Nonce, IdBlockPrevious} ->
      Block = block_con_IdBlockPrevious,%% todo

      Sender ! {previous, Nonce, Block};

    {previous, Nonce, Block} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce},
      receive
        {nonce, false, TempNonce} -> false;
        {nonce, ok, TempNonce} -> ok %% todo

      after 5000 -> self() ! {previous, Nonce, Block}
      end;

    {get_head, Sender, Nonce} ->
      Block = block_head,%% todo

      Sender ! {head, Nonce, Block};

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
