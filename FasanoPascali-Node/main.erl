-module(main).
-export([watchMain/0, main/0, sleep/1]).

sleep(N) -> receive after N * 1000 -> ok end.

%%link verso main in modo tale da catturarne la morte improvvisa e farlo ripartire da zero
watchMain() ->
  process_flag(trap_exit, true),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Kill ~p Main ~p~n", [Pid, Reason]),
      spawn(main, main, []),
      exit(self(), kill)
  end.

%% fa partire tutti gli attori ausiliari
main() ->
  compile:file(topologyFP),
  compile:file(blockChain),
  spawn_link(main, watchMain, []),
  PID = self(),
  PIDManagerNonce = spawn_link(topologyFP, managerNonce, [[]]),
  PIDManagerMessage = spawn_link(topologyFP, sendGetFriends, [PID, PIDManagerNonce]),
  PIDGossipingMessage = spawn_link(topologyFP, gossipingMessage, [[]]),
  PIDManagerFriends = spawn_link(topologyFP, newFriendsRequest, [PID, [], 0, PIDManagerNonce, PIDManagerMessage, PIDGossipingMessage]),
  PIDManagerTransaction = spawn_link(blockChain, managerTransactions, [PID, PIDGossipingMessage, [], []]),
  PIDManagerBlock = spawn_link(blockChain, initManagerBlock, [PID, PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDGossipingMessage]),
  %%attore che si occupa di tenere aggiornata la mia blockchain in caso non abbiamo amici (si cerca di capire se non abbiamo amici quando non riceviamo ping per tot tempo)
  PIDManagerHead = spawn_link(blockChain, managerHead, [PID]),
  loopInit(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead).

%%chiedo amici al teacher_node e se non ricevo risposta itero finchè il nodo docente non risponde con la lista di amici
loopInit(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead) ->
  NonceGlobalSend = make_ref(),
  PIDManagerNonce ! {updateNonce, NonceGlobalSend},%% serve perchè controllo il Nonce bel managerFriends
  TeacherPID = global:send(teacher_node, {get_friends, self(), NonceGlobalSend}),
  receive
    {friends, NonceGlobalSend, FriendsOfFriend} ->
      PIDManagerFriends ! {friends, NonceGlobalSend, FriendsOfFriend}
  after 10000 -> loopInit(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead)
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID).

%%loopMain è semplicemente uno scheduler di messaggi verso gli attori che li gestiscono
loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID) ->
%%  process_flag(trap_exit, true),
  receive
    {friends, Nonce, FriendsOfFriend} ->
      PIDManagerFriends ! {friends, Nonce, FriendsOfFriend};

    {get_friends, Sender, Nonce} ->
      PIDManagerFriends ! {get_friends, Sender, Nonce};

    {push, Transaction} ->
      PIDManagerTransaction ! {push, Transaction};

    {update, Sender, Block} ->
      PIDManagerBlock ! {update, Sender, Block},
      PIDManagerHead ! {follower};

    {get_previous, Mittente, Nonce, Idblocco_precedente} ->
      PIDManagerBlock ! {get_previous, Mittente, Nonce, Idblocco_precedente};

    {get_head, Mittente, Nonce} ->
      PIDManagerBlock ! {get_head, Mittente, Nonce};

    {ping, Sender, Ref} ->
      Sender ! {pong, Ref};

    {maybeNoFollowers} ->
      TempNonce = make_ref(),
      PIDManagerNonce ! {updateNonce, TempNonce},
      PIDManagerFriends ! {sendMessageRandFriend, {get_head, PIDManagerBlock, TempNonce}}

%%    {'EXIT', Pid, Reason} ->
%%      io:format("Kill ~p Reason ~p~n", [Pid, Reason]),
%%      spawn(main, main, []),
%%      exit(self(), kill)
  end,
  loopMain(PIDManagerFriends, PIDManagerNonce, PIDManagerTransaction, PIDManagerBlock, PIDManagerHead, TeacherPID).