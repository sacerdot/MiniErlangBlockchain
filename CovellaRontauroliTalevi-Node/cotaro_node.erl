-module(cotaro_node).
-export([loop/2, test_nodes/0]).

-define(NumberOfFriendsRequired, 3).

sleep(N) -> receive after N*1000 -> ok end.

loop(MyFriends, State) ->
    case MyFriends of
        [] ->   io:format("~p require to teacher_node new friends ~n", [self()]),
                global:send(teacher_node, {get_friends, self(), make_ref()});
        _ ->    io:format("~p has at least one friend ~n", [self()])
    end,
    receive

        {ping, Mittente, Nonce} ->  Mittente ! {pong, Nonce},
                                    io:format("~p has received ping request, sending pong~n", [self()]),
                                    loop(MyFriends, State) ;


        {dead, Node} -> io:format("For ~p the node ~p is dead~n",[self(), Node]),
                        MyFriendsUpdated = MyFriends -- [Node],
                        askFriends(MyFriendsUpdated),
                        loop(MyFriendsUpdated, State);


        {friends, _, OtherFriends} ->   io:format("~p receive friend list ~n", [self()]),
                                        NewFriends = OtherFriends -- (MyFriends ++ [self()]),
                                        MyNewListOfFriends = addFriends(MyFriends, NewFriends),
                                        loop(MyNewListOfFriends, State);


        {get_friends, Sender, Nonce} -> io:format("~p send friend list to ~p ~n", [self(), Sender]),
                                        Sender ! {friends, Nonce, MyFriends},
                                        loop(MyFriends, State)

    end.



askFriends([]) ->   io:format("~p require friends to teacher node~n", [self()]),
                    global:send(teacher_node, {get_friends, self(), make_ref()});
askFriends(MyFriends) ->    io:format("~p require friends to another node~n", [self()]),
                            Nonce = make_ref(),
                            lists:nth(rand:uniform(length(MyFriends)), MyFriends) ! {get_friends, self(), Nonce}.


addFriends(MyFriends, []) -> MyFriends;
addFriends([], OtherFriends) ->
    NewFriend = lists:nth(rand:uniform(length(OtherFriends)), OtherFriends),
    io:format("~p add a new friend ~p~n",[self(), NewFriend]),
    Self = self(),
    spawn(fun () -> watch(Self,NewFriend) end),
    addFriends([NewFriend], OtherFriends -- [NewFriend]);
addFriends(MyFriends, OtherFriends) ->
    case length(MyFriends) < ?NumberOfFriendsRequired of
        true -> NewFriend = lists:nth(rand:uniform(length(OtherFriends)), OtherFriends),
                io:format("~p add a new friend ~p~n",[self(), NewFriend]),
                Self = self(),
                spawn(fun () -> watch(Self,NewFriend) end),
                addFriends( MyFriends ++ [NewFriend], OtherFriends -- [NewFriend]);
        false -> io:format("~p friend list = ~w ~n", [self(), MyFriends]),
                 MyFriends
    end.


watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 -> Main ! {dead, Node}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_nodes() ->
	T = spawn(teacher_node, main, []),
	sleep(5),
	M1 = spawn(?MODULE, loop, [[],[]]),
	M2 = spawn(?MODULE, loop, [[],[]]),
	M3 = spawn(?MODULE, loop, [[],[]]),
	M4 = spawn(?MODULE, loop, [[],[]]),
	io:format("Finito di spawnare 4 nodi + teacher_node~n"),
    sleep(15),
    exit(M2, manually_kill),
	sleep(15),
	Ref = make_ref(),
	M1 ! {get_friends, self(), Ref},
	receive
		{friends, Ref, Lista_amici} -> io:fwrite("Amici di M1 ~w~n",[Lista_amici])
	end.
