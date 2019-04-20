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
  PIDManagerNonce = spawn_link(topologyFP, managerNonce, [[]]),
  PIDManagerMessage = spawn_link(topologyFP, sendGetFriends, [PID, PIDManagerNonce]),
  PIDGossipingMessage = spawn_link(topologyFP, gossipingMessage, [[]]),
  PIDManagerFriends = spawn_link(topologyFP, newFriendsRequest, [PID, [], 0, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage]),
  PIDManagerTransaction = spawn_link(blockChain, managerTransactions, [PID, PIDGossipingMessage, [], []]),
  PIDManagerBlock = spawn_link(blockChain, initManagerBlock, [PID, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDGossipingMessage]),
  %%attore che si occupa di tenere aggiornata la mia blockchain in caso non abbiamo amici (si cerca di capire se non abbiamo amici quando non riceviamo ping per tot tempo)
  PIDManagerHead = spawn_link(blockChain, managerHead, [PID]),
  loopInit(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead).

loopInit(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead) ->
%%      chiedo amici al teacher_node
  NonceGlobalSend = make_ref(),
  PIDManagerNonce ! {updateNonce, NonceGlobalSend},%% serve perchè controllo il Nonce bel managerFriends
  TeacherPID = global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  receive
    {friends, NonceGlobalSend, FriendsOfFriend} ->
      PIDManagerFriends ! {friends, NonceGlobalSend, FriendsOfFriend}
  after 10000 -> loopInit(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead)
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID).

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
      io:format("~p -> +++++++++++++++++++++++++++++NoFollowers----------------------------- ~n", [self()]),
      TempNonce = make_ref(),
      PIDManagerNonce ! {updateNonce, TempNonce},
      PIDManagerFriends ! {sendMessageRandFriend, {get_head, PIDManagerBlock, TempNonce}}
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID).




























