-module(cotaro_node).
-export([loop/2, test_nodes/0]).

sleep(N) -> receive after N*1000 -> ok end.

%watcher che controlla periodicamente che il nodo sia vivo
watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 10000 -> Main ! {dead, Node}
  end.


loop(Nodes, State) ->
  receive
		{ping, Watcher, WatcherRef} -> Watcher ! {pong, WatcherRef} ,
							   io:format("~p has received ping request, sending pong~n", [self()])
  after 2000 -> global:send(teacher_node, {get_friends, self(), make_ref()})
  end,
  receive

	%{ping, Sender, Nonce} -> Sender ! {pong, Nonce};

    {get_friends, Sender, Nonce} ->
       Sender ! {friends, Nonce, Nodes},
       loop(Nodes, State) ;
	%%se uno dei nodi muore, manda a se stesso un messaggio "get_more_friends" e torna in loop
    {dead, Node} ->
       io:format("Dead node ~p~n",[Node]),
	   Ref = make_ref(),
	   self() ! {get_more_friends, Ref, Nodes -- [Node]},
       loop(Nodes -- [Node], State);

	{get_more_friends, Ref, CurrentNodes} -> getMoreFriends(self(), Ref, CurrentNodes);

	{friends, Ref, AmiciAltrui} -> AmiciDiversi = AmiciAltrui -- [self()|Nodes], %elimino dalla lista ottenuta me stesso e gli amici che già ho
								   Nuovo_Amico = case length(AmiciDiversi) > 0 of %se la lista ottenuta non è vuota, scelgo un amico a caso
															
													true -> io:format("~p: AmiciDiversi: ~p~n", [self(), AmiciDiversi]),
															F = lists:nth(rand:uniform(length(AmiciDiversi)), AmiciDiversi),
															io:format("L'attore ~p sta aggiungendo un amico ~p.~n", [self(), F]),
															spawn(fun () -> watch(self(),F) end), %spawno il watcher per il nuovo amico
															F;

													false -> []
								   end,
								   loop([Nuovo_Amico|Nodes], State)

    after 2000 ->
	   case length(Nodes) < 3 of
		   true -> Ref = make_ref(),
				   self() ! {get_more_friends, Ref, Nodes},
				   io:format("L'attore ~p sta mandando a se stesso una get_more_friends", [self()]);
		   false -> ok
	   end,
	   loop(Nodes, State)
	end.

%% se abbiamo ancora amici, chiediamo la lista di amici ad un nostro amico, altrimenti la chiediamo al teacher_node
getMoreFriends(PID, Nonce, Nodes) ->
	io:format("L'attore ~p ha fatto chiamata a getMoreFriends.~n", [PID]),
	case length(Nodes) > 0 of
		true -> lists:nth(rand:uniform(length(Nodes)), Nodes) ! {get_friends, PID, Nonce};
		false -> global:send(teacher_node, {get_friends, PID, Nonce})
	end.


test_nodes() ->
	T = spawn(teacher_node, main, []),
	sleep(5),
	M1 = spawn(?MODULE, loop, [[],[]]),
	M2 = spawn(?MODULE, loop, [[],[]]),
	M3 = spawn(?MODULE, loop, [[],[]]),
	M4 = spawn(?MODULE, loop, [[],[]]),
	io:format("Finito di spawnare 4 nodi + teacher_node~n"),
	sleep(2),
	Ref = make_ref(),
	M1 ! {get_friends, self(), Ref},
	receive
		{friends, Ref, Lista_amici} -> io:fwrite("Amici di M1 ~w~n",[Lista_amici])
	end.
