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
%% todo integrare perdita di messaggi e invio di messaggi doppi

init() ->
  PID = self(),

  %% todo catturare morte dell'attore e riavviarlo per i 4 sottostanti
  %% di conseguenza dopo averlo riavviato va effettuato l'upload dei PID nei vari attori che lo riportano
  ManagerNonce = spawn_link(topologyFP, managerNonce, [PID, []]),
  ManagerMessage = spawn_link(topologyFP, sendGetFriends, [PID, ManagerNonce]),
  PIDGossipingMessage = spawn_link(topologyFP, gossipingMessage, [[]]),
  ManagerFriends = spawn_link(topologyFP, newFriendsRequest, [PID, [], 0, ManagerNonce, ManagerMessage, PIDGossipingMessage]),
  ManagerTransaction = spawn_link(blockChain, managerTransactions, [PID, PIDGossipingMessage, [], []]),
  ManagerBlock = spawn_link(blockChain, initManagerBlock, [PID, ManagerFriends, ManagerNonce, ManagerTransaction, PIDGossipingMessage]),
  %%attore che si occupa di tenere aggiornata la mia blockchain in caso non abbiamo amici (si cerca di capire se non abbiamo amici quando non riceviamo ping per tot tempo)
  ManagerHead = spawn_link(blockChain, managerHead, [PID]),
  loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead).

loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead) ->
%%      chiedo amici al teacher_node
  NonceGlobalSend = make_ref(),
  ManagerNonce ! {updateNonce, NonceGlobalSend},
  TeacherPID = global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  receive
    {friends, NonceGlobalSend, FriendsOfFriend} ->
      ManagerFriends ! {friends, NonceGlobalSend, FriendsOfFriend}
  after 10000 -> loopInit(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead)
  end,
  loopMain(ManagerFriends, ManagerNonce, ManagerTransaction, ManagerBlock, ManagerHead, TeacherPID).

loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID) ->
  receive
    {friends, Nonce, FriendsOfFriend} ->
      PIDManagerFriends ! {friends, Nonce, FriendsOfFriend};

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
      Sender ! {pong, Ref},
      PIDManagerHead ! {pong, Sender, TeacherPID};

    {maybeNoFollowers} ->
      io:format("~p -> +++++++++++++++++++NoFollowers----------------------------- ~n", [self()]),
      TempNonce = make_ref(),
      PIDManagerNonce ! {updateNonce, TempNonce},
      PIDManagerFriends ! {sendMessageRandFriend, {get_head, PIDManagerBlock, TempNonce}}
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID).




























