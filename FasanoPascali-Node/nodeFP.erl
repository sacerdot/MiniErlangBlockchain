%%%-------------------------------------------------------------------
%%% @author andrea
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. apr 2019 14.43
%%%-------------------------------------------------------------------
-module(nodeFP).
-author("andrea").
-export([init/0, sleep/1]).


sleep(N) -> receive after N * 1000 -> ok end.

%% todo testare perdita di messaggi e arrivo di messaggi doppi

init() ->
%%      chiedo amici
  PID = self(),

  %% todo catturare morte dell'attore e riavviarlo per i 4 sottostanti
  %% di conseguenza dopo averlo riavviato va effettuato l'upload dei PID nei vari attori che lo riportano
  ManagerNonce = spawn_link(topologyFP, managerNonce, [PID, []]),
  ManagerMessage = spawn_link(topologyFP, sendGetFriends, [PID, ManagerNonce]),
  ManagerFriends = spawn_link(topologyFP, newFriendsRequest, [PID, [], 0, ManagerNonce, ManagerMessage]),
  ManagerTransaction = spawn_link(blockChain, managerTransactions, [PID, ManagerFriends, [], []]),
  ManagerBlock = spawn_link(blockChain, managerBlock, [PID, ManagerFriends, ManagerNonce, ManagerTransaction, []]),
  %%attore che si occupa di tenere aggiornata la mia blockchain in caso non abbiamo amici (si cerca di capire se non abbiamo amici quando non riceviamo ping per tot tempo)
  ManagerHead = spawn_link(blockChain, managerHead, [PID]),
  loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead).

loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead) ->
  NonceGlobalSend = make_ref(),
  TeacherPID = global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  receive
    {friends, NonceGlobalSend, FriendsOfFriend} ->
      ManagerFriends ! {friend, FriendsOfFriend}
  after 10000 -> loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead)
  end,
  loopMain(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead, TeacherPID).


loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID) ->
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
      PIDManagerHead ! {pong, Sender, TeacherPID},
      Sender ! {pong, Ref};

    {maybeNoFollowers} ->
      todo %%todo
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID).