-module(test).
-export([test/0, minimalTest/0, stressfulTest/0, init/0, newFriendsRequest/4, managerNonce/2,
  managerTransaction/3, managerBlock/4]).

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
  NonceGlobalSend = make_ref(),
  global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),

  %% todo catturare morte dell'attore e riavviarlo per i 4 sottostanti
  %% di conseguenza dopo averlo riavviato va effettuato l'upload dei PID nei vari attori che lo riportano
  ManagerNonce = spawn_link(test, managerNonce, [PID, []]),
  ManagerFriends = spawn_link(test, newFriendsRequest, [PID, [], 0, ManagerNonce]),
  ManagerTransaction = spawn_link(test, managerTransaction, [PID, ManagerFriends, []]),
  Block0= {none, none, [], proof_of_work:solve({none, []})},
  ManagerBlock = spawn_link(test, managerBlock, [PID, ManagerFriends, ManagerNonce, [Block0]]),
  receive
    {friends, NonceGlobalSend, ListFriends} ->
      ManagerFriends ! {friend, ListFriends}
  end,%%todo loop init
  loopMain(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock).


loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock) ->
  receive
    {friends, Nonce, ListFriends} ->
      PIDManagerNonce ! {checkNonce, Nonce},
      receive
        {nonce, false} -> false;
        {nonce, ok} -> PIDManagerFriends ! {friend, ListFriends}
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
managerTransaction(PIDMain, PIDManagerFriends, ListTransaction) ->
  receive
    {push, Transaction} ->
      case lists:member(Transaction, ListTransaction) of
        false ->
          PIDManagerFriends ! {gossipingMessage, {push, Transaction}},%% ritrasmetto agli amici
          NewListTransaction = ListTransaction ++ [Transaction],
          managerTransaction(PIDMain, PIDManagerFriends, NewListTransaction)
      end;
    {pop, Transaction} -> managerTransaction(PIDMain, PIDManagerFriends, ListTransaction--[Transaction])
  end.
%%  ,managerTransaction(PIDMain, PIDManagerFriends, ListTransaction)



%%Blocco= {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni, Soluzione}
%%Soluzione= proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni})
%%proof_of_work:check({IDblocco_precedente,Lista_di_transazioni}, Soluzione)

%% gestisce i blocchi
managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, BlockChain) ->
  receive
    {update, Sender, Block} ->

%%       controllo se lo conosco
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
      PIDManagerNonce ! {checkNonce, Nonce},
      receive
        {nonce, false} -> false;
        {nonce, ok} -> ok %% todo

      end;

    {get_head, Sender, Nonce} ->
      Block = block_head,%% todo
      Sender ! {head, Nonce, Block};

    {head, Nonce, Block} ->
      PIDManagerNonce ! {checkNonce, Nonce},
      receive
        {nonce, false} -> false;
        {nonce, ok} -> ok %% todo

      end
  end,
  managerBlock(PIDMain, PIDManagerFriends, PIDManagerNonce, BlockChain).


%% gestisce lo storage dei Nonce e il loro controllo
managerNonce(PIDMain, ListNonce) ->
  receive
    {updateNonce, Nonce} ->
      io:format("~p -> updateNonce Nonce ~p~n", [PIDMain, Nonce]),
      managerNonce(PIDMain, ListNonce ++ [Nonce]);
    {checkNonce, Nonce} ->
      case lists:member(Nonce, ListNonce) of
        true ->
          io:format("~p -> checkNonce Nonce ~p OK ++++++++++-->~n", [PIDMain, Nonce]),
          PIDMain ! {nonce, ok},
          managerNonce(PIDMain, ListNonce--[Nonce]);
        false ->
          io:format("~p -> checkNonce Nonce ~p False -------->~n", [PIDMain, Nonce]),
          PIDMain ! {nonce, false},
          managerNonce(PIDMain, ListNonce)
      end
  end.

%% InitFriends è inserito per evitare di spawn nodi monitor già esistenti
extractNewFriends(ListFriends, MyListFriends, InitFriends) ->
  FilterListFriends = lists:filter(fun(Elem) -> not lists:member(Elem, MyListFriends) end, ListFriends),
  LfilterFriends = length(FilterListFriends),
  if
    LfilterFriends < 1 -> lists:sublist(MyListFriends, length(MyListFriends) - 1)-- InitFriends;
    true ->
      R = rand:uniform(length(FilterListFriends)),
      Llist3Friends = length(MyListFriends) - 1,
      if
        Llist3Friends < 3 ->
          extractNewFriends(FilterListFriends, [lists:nth(R, FilterListFriends)] ++ MyListFriends, InitFriends);
        true -> lists:sublist(MyListFriends, length(MyListFriends) - 1) -- InitFriends
      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Step: 0-> Skip; 1-> 1° richiesta; 2-> 2° richiesta sleep; 3-> chiedo al nodo prof.
newFriendsRequest(PIDMain, Friends, Step, ManagerNonce) ->
  io:format("~p ->Start newFriendsRequest Friends: ~p Step: ~p~n", [PIDMain, Friends, Step]),
  MyPid = self(),
  case Step of
    0 -> ok; %% Skip
    3 -> sendGetFriends(PIDMain, global, ManagerNonce); %% richiesta verso il nodo prof
    _ -> %% caso 1 e 2 ovvero 1° e 2° richiesta verso i miei amici
      if
        Step == 2 -> sleep(20);
        true -> ok
      end,
      case length(Friends) of
        0 -> sendGetFriends(PIDMain, global, ManagerNonce);
        1 -> sendGetFriends(PIDMain, lists:nth(1, Friends), ManagerNonce);
        2 -> sendGetFriends(PIDMain, lists:nth(Step, Friends), ManagerNonce);%% Step possibili a questo livello solo 1 e 2
        _ -> ok
      end
  end,
  receive
    {friend, ListFriends} ->
      io:format("~p -> Friend receive Friends: ~p Step: ~p~n", [PIDMain, ListFriends, Step]),
      NewFriends = extractNewFriends(ListFriends, Friends ++ [PIDMain], Friends),
      [spawn(fun() -> watch(MyPid, X) end) || X <- NewFriends],
      NewListFriends = Friends ++ NewFriends,
      if
        length(NewListFriends) >= 3 ->
          io:format("~p ->+++++ 1 ~p ~p~n", [PIDMain, Step, length(NewListFriends)]),
          newFriendsRequest(PIDMain, NewListFriends, 0, ManagerNonce);
        Step == 3 ->
          io:format("~p ->+++++ 2 ~p~n", [PIDMain, Step]),
          newFriendsRequest(PIDMain, NewListFriends, 1, ManagerNonce);
        true ->
          io:format("~p ->++++++ 3 ~p~n", [PIDMain, Step]),
          newFriendsRequest(PIDMain, NewListFriends, Step + 1, ManagerNonce)
      end;

    {dead, Node} ->
      io:format("~p -> Dead node ~p~n", [PIDMain, Node]),
      FriendsLess = Friends -- [Node],
      newFriendsRequest(PIDMain, FriendsLess, 1, ManagerNonce);

    {get_friends, Sender, Nonce} ->
      io:format("~p -> get_friends from node ~p~n", [PIDMain, Sender]),
      Sender ! {friends, Nonce, Friends};

    {gossipingMessage, Message} ->
      io:format("~p -> gossipingMessage : transaction ~p~n", [PIDMain, Message]),
      [F ! Message || F <- Friends]
  end,
  if
    length(Friends) >= 3 -> newFriendsRequest(PIDMain, Friends, 0, ManagerNonce);
    true -> newFriendsRequest(PIDMain, Friends, Step, ManagerNonce)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendGetFriends(PIDMain, Receive, ManagerNonce) ->
  TempNonce = make_ref(),
  ManagerNonce ! {updateNonce, TempNonce},
  if
    Receive == global -> global:send(teacher_node, {get_friends, PIDMain, TempNonce});
    true -> Receive ! {get_friends, PIDMain, TempNonce}
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

%% c(test). test:test().  exit(<0.71.0>, kill). spawn(test, init, []).
%% exit(<0.68.0>, kill).
%% exit(<0.69.0>, kill).
%% exit(<0.70.0>, kill).
%% spawn(test, init, []).



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










