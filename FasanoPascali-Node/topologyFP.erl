%%%-------------------------------------------------------------------
%%% @author andrea
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. apr 2019 14.47
%%%-------------------------------------------------------------------
-module(topologyFP).
-author("andrea").
-export([watch/2, managerNonce/2, newFriendsRequest/5, sendGetFriends/2, sendMaybeWrongMessages/3]).


watch(Main, Node) ->
  nodeFP:sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  io:format("ping da ~p a ~p~n", [self(), Node]),
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node}
  end.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Step: 0-> Skip; 1-> 1° richiesta; 2-> 2° richiesta nodeFP:sleep; 3-> chiedo al nodo prof.
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

%% InitFriends è stato inserito per evitare di spawn nodi monitor già esistenti e quindi far ritornare solo i nuovi amici
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

sendGetFriends(PIDMain, ManagerNonce) ->
  TempNonce = make_ref(),
  ManagerNonce ! {updateNonce, TempNonce},
  receive
    {global} -> global:send(teacher_node, {get_friends, PIDMain, TempNonce}), nodeFP:sleep(5);
    {Receive, Step} when Step==2 -> nodeFP:sleep(5), Receive ! {get_friends, PIDMain, TempNonce};
    {Receive, _} -> Receive ! {get_friends, PIDMain, TempNonce}
  end,
  flushMailBox(),
  sendGetFriends(PIDMain, ManagerNonce).

flushMailBox() ->
  receive _ -> flushMailBox()
  after 0 -> ok
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
