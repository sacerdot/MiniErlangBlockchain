-module(test).
-export([test/0, minimalTest/0, stressfulTest/0, init/0, newFriendsRequest/5, managerNonce/2,
  managerTransactions/4, managerBlock/4, sendGetFriends/2]).

% This is the teacher node, the one responsible
% for letting new nodes in the blockchain.
%
% It implements the topology maintainance algorithm
%
% It registers itself globally under the name teacher_node

sleep(N) -> receive after N * 1000 -> ok end.

%% todo testare perdita di messaggi e arrivo di messaggi doppi

watch(Main, Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  io:format("ping da ~p a ~p~n", [self(), Node]),
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node}
  end.

init() ->
%%      chiedo amici
  PID = self(),

  %% todo catturare morte dell'attore e riavviarlo per i 4 sottostanti
  %% di conseguenza dopo averlo riavviato va effettuato l'upload dei PID nei vari attori che lo riportano
  ManagerNonce = spawn_link(test, managerNonce, [PID, []]),
  ManagerMessage = spawn_link(test, sendGetFriends, [PID, ManagerNonce]),
  ManagerFriends = spawn_link(test, newFriendsRequest, [PID, [], 0, ManagerNonce, ManagerMessage]),
  ManagerTransaction = spawn_link(test, managerTransactions, [PID, ManagerFriends, [], []]),
  ManagerBlock = spawn_link(test, managerBlock, [PID, ManagerFriends, ManagerNonce, []]),
  loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock).

loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock) ->
  NonceGlobalSend = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  receive
    {friends, NonceGlobalSend, FriendsOfFriend} ->
      ManagerFriends ! {friend, FriendsOfFriend}
  after 10000 -> loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock)
  end,
  loopMain(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock).


loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock) ->
  receive
    {friends, Nonce, FriendsOfFriend} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {checkNonce, Nonce, TempNonce},
      receive
        {nonce, false, TempNonce} -> false;
        {nonce, ok, TempNonce} -> PIDManagerFriends ! {friend, FriendsOfFriend}
      after 5000 -> self() ! {friends, Nonce, FriendsOfFriend}
      end;

    {get_friends, Sender, Nonce} ->
      PIDManagerFriends ! {get_friends, Sender, Nonce};

    {push, Transaction} ->
      PIDManagerTransaction ! {push, Transaction};

    {update, Sender, Block} ->
      PIDManagerBlock ! {update, Sender, Block};


    {get_previous, Mittente, Nonce, Idblocco_precedente} ->
      PIDManagerBlock ! {get_previous, Mittente, Nonce, Idblocco_precedente};

    {get_head, Mittente, Nonce} ->
      PIDManagerBlock ! {get_head, Mittente, Nonce};

    {ping, Sender, Ref} ->
      Sender ! {pong, Ref}
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock).


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


%% gestisce lo storage dei Nonce e il loro controllo
managerNonce(PIDMain, Nonces) ->
  receive
    {updateNonce, Nonce} ->
      io:format("~p -> updateNonce Nonce ~p~n", [PIDMain, Nonce]),
      managerNonce(PIDMain, Nonces ++ [Nonce]);
    {checkNonce, Nonce, TempNonce} ->
      case lists:member(Nonce, Nonces) of
        true ->
          io:format("~p -> checkNonce Nonce ~p OK ++++++++++-->~n", [PIDMain, Nonce]),
          PIDMain ! {nonce, ok, TempNonce},
          managerNonce(PIDMain, Nonces--[Nonce]);
        false ->
          io:format("~p -> checkNonce Nonce ~p False -------->~n", [PIDMain, Nonce]),
          PIDMain ! {nonce, false, TempNonce},
          managerNonce(PIDMain, Nonces)
      end
  end.

%% InitFriends è inserito per evitare di spawn nodi monitor già esistenti
extractNewFriends(FriendsOfFriend, MyFriendsAndI, InitFriends) ->
  FilterFriends = FriendsOfFriend--MyFriendsAndI,
  if
    length(FilterFriends) < 1 -> lists:sublist(MyFriendsAndI, length(MyFriendsAndI) - 1)-- InitFriends;
    true ->
      R = rand:uniform(length(FilterFriends)),
      if
        (length(MyFriendsAndI) - 1) < 3 ->
          extractNewFriends(FilterFriends, [lists:nth(R, FilterFriends)] ++ MyFriendsAndI, InitFriends);
        true -> lists:sublist(MyFriendsAndI, length(MyFriendsAndI) - 1) -- InitFriends
      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Step: 0-> Skip; 1-> 1° richiesta; 2-> 2° richiesta sleep; 3-> chiedo al nodo prof.
newFriendsRequest(PIDMain, Friends, Step, ManagerNonce, ManagerMessage) ->
  io:format("~p ->Start newFriendsRequest Friends: ~p Step: ~p~n", [PIDMain, Friends, Step]),
  MyPid = self(),
  case Step of
    0 -> ok; %% Skip
    3 -> ManagerMessage ! {global}; %% richiesta verso il nodo prof
    _ -> %% caso 1 e 2 ovvero 1° e 2° richiesta verso i miei amici
      case length(Friends) of
        0 -> ManagerMessage ! {global};
        1 -> ManagerMessage ! {lists:nth(1, Friends), Step};
        2 -> ManagerMessage ! {lists:nth(Step, Friends), Step};%% Step possibili a questo livello solo 1 e 2
        _ -> ok
      end
  end,
  receive
    {friend, FriendsOfFriend} ->
      io:format("~p -> Friend receive Friends: ~p Step: ~p~n", [PIDMain, FriendsOfFriend, Step]),
      NewFriends = extractNewFriends(FriendsOfFriend, Friends ++ [PIDMain], Friends),
      [spawn(fun() -> watch(MyPid, X) end) || X <- NewFriends],
      NewTotalFriends = Friends ++ NewFriends,
      if
        length(NewTotalFriends) >= 3 ->
          io:format("~p ->+++++ 1 ~p ~p~n", [PIDMain, Step, length(NewTotalFriends)]),
          newFriendsRequest(PIDMain, NewTotalFriends, 0, ManagerNonce, ManagerMessage);
        Step == 3 ->
          io:format("~p ->+++++ 2 ~p~n", [PIDMain, Step]),
          newFriendsRequest(PIDMain, NewTotalFriends, 1, ManagerNonce, ManagerMessage);
        true ->
          io:format("~p ->++++++ 3 ~p~n", [PIDMain, Step]),
          newFriendsRequest(PIDMain, NewTotalFriends, Step + 1, ManagerNonce, ManagerMessage)
      end;

    {dead, Node} ->
      io:format("~p -> Dead node ~p~n", [PIDMain, Node]),
      FriendsLess = Friends -- [Node],
      newFriendsRequest(PIDMain, FriendsLess, 1, ManagerNonce, ManagerMessage);

    {get_friends, Sender, Nonce} ->
      io:format("~p -> get_friends from node ~p~n", [PIDMain, Sender]),
      Sender ! {friends, Nonce, Friends};

    {gossipingMessage, Message} ->
      io:format("~p -> gossipingMessage : transaction ~p~n", [PIDMain, Message]),
      [F ! Message || F <- Friends]
  end,
  if
    length(Friends) >= 3 -> newFriendsRequest(PIDMain, Friends, 0, ManagerNonce, ManagerMessage);
    true -> newFriendsRequest(PIDMain, Friends, Step, ManagerNonce, ManagerMessage)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendGetFriends(PIDMain, ManagerNonce) ->
  TempNonce = make_ref(),
  ManagerNonce ! {updateNonce, TempNonce},
  receive
    {global} -> global:send(teacher_node, {get_friends, PIDMain, TempNonce}), sleep(5);
    {Receive, Step} when Step==2 -> sleep(5), Receive ! {get_friends, PIDMain, TempNonce};
    {Receive, Step} -> Receive ! {get_friends, PIDMain, TempNonce}
  end,
  flushMailBox(),
  sendGetFriends(PIDMain, ManagerNonce).

flushMailBox() ->
  receive _ -> flushMailBox()
  after 0 -> ok
  end.


sendMaybeWrongMessages(PidRecevier, Message, IsErrorActivated) ->
  if
    IsErrorActivated == false ->
      PidRecevier ! Message;
    true ->
      RandomNumber = rand:uniform(10),
      case RandomNumber of
        1 -> do_nothing;
        2 -> PidRecevier ! Message, PidRecevier ! Message;
        _ -> PidRecevier ! Message
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
  spawn(teacher_node, main, []),
  sleep(3),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []).

minimalTest() ->
  spawn(teacher_node, main, []),
  sleep(3),
  spawn(test, init, []),
  sleep(11),
  TempPid = spawn(test, init, []),
  sleep(5),
  TempPid1 = spawn(test, init, []),
  TempPid2 = spawn(test, init, []),
  sleep(20),
  exit(TempPid, kill),
  exit(TempPid1, kill),
  sleep(5),
  exit(TempPid2, kill).

stressfulTest() ->
  test(),
  stressfulTestLoop().

stressfulTestLoop() ->
  sleep(3),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  spawn(test, init, []),
  stressfulTestLoop().


%% c(test). test:test().  exit(<0.71.0>, kill). spawn(test, init, []).
%% test:minimalTest().
%% exit(<0.68.0>, kill).
%% exit(<0.69.0>, kill).
%% exit(<0.70.0>, kill).
%% spawn(test, init, []).
