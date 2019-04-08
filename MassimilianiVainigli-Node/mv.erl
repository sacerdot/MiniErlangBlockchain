%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------
-module(mv).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-export([watch/2, check_nodes/2, main_node/0, test/0]).


sleep(N) -> receive after N*1000 -> ok end.



%% riferisce a Main della vita di Node
watch(Main,Node) ->
  %io:format(" ~p -> ~p  ~n", [Main, Node]),
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 -> io:format("Sono ~p ed e' morto ~p ~n ~n ", [Main, Node]), Main ! {dead, Node}
  end.




%% attore che si occupa di mantere la topologia della rete
check_nodes(Parent, List_friends) ->

  io:format("Sono ~p e ho ~p amici ~n", [Parent, List_friends]),

  %% se non ho amici chiedo al teacher_node
  if
    length(List_friends) =:= 0 ->
      Ref = make_ref(),
      teacher_node ! {get_friends, Parent, Ref},
      receive
        {friends, Ref, New_nodes} ->
          Nodes_without_me = New_nodes -- [Parent],
          if length(Nodes_without_me) > 0 ->
            New_friend = lists:nth(rand:uniform(length(Nodes_without_me)), Nodes_without_me),
            io:format("Sono ~p Trovato ~p ~n", [Parent, New_friend]),
            spawn(fun() ->watch(Parent, New_friend) end),
            check_nodes(Parent, [New_friend]);
            true -> check_nodes(Parent, List_friends)
          end
      after 3000 -> check_nodes(Parent, List_friends)
      end;
    true -> ok
  end,


  %% se ho un numero di amici compreso tra [1,2] mando la richiesta a un amico casuale di passarmi la sua lista amici
  if
    ((length(List_friends) < 3) and  (length(List_friends) > 0)) ->
      Random_friend =  lists:nth(rand:uniform(length(List_friends)), List_friends),
      Random_friend !  {get_friends, Parent, make_ref()};
    true -> ok
  end,



  %% gestione dei messaggi che arrivano
  receive

    {ping, Sender, Ref2} -> io:format("Sono vivo ~n"),
      Sender ! {pong, Ref2},
      check_nodes(Parent, List_friends);


    {dead, Node} ->
      io:format("E' morto ~p~n",[Node]),
      check_nodes(Parent, List_friends -- [Node]);

    {get_friends, Node, _} ->
      %% se ho meno di 3 amici e la richiesta mi arriva da un nodo che non è nella lista lo aggiungo
      case lists:member(Node,List_friends)  of
        false ->
          if length(List_friends) < 3 ->
            spawn(fun() ->watch(Parent, Node) end),
            check_nodes(Parent, List_friends ++ [Node]);
            true -> ok
          end;
        true ->
          ok,
          Node ! {friends, make_ref(), List_friends},
          check_nodes(Parent, List_friends)
      end;

    {friends, _, Nodes} ->
      %% trovo i nodi che non sono nella mia lista e che non sono io
      Possible_friends = ((Nodes -- [Parent]) -- List_friends),

      %% add_friends gestisce l'aggiunta di amici scelti casualmente
      %% fino a quando finisce Possible_friends oppure la lista Lists_friends arriva a 3
      NewFriends = add_friends(List_friends, Possible_friends, Parent),

      check_nodes(Parent, NewFriends);


    {list_of_friends, Nonce} ->
      Parent ! {List_friends, Nonce},
      check_nodes(Parent, List_friends)

  end.




%% gestione dell'aggiunta di elementi da una lista all'altra
add_friends(List1, [], _) -> List1;
add_friends(List1, List2, Parent) ->
  if
    length(List1) >= 3 -> List1;
    true ->
      RandomElement = lists:nth(rand:uniform(length(List2)), List2),
      spawn(fun() ->watch(Parent, RandomElement) end),
      add_friends(List1++[RandomElement], List2--[RandomElement], Parent)
  end.




%% nodo principale
main_node() ->
  check_nodes(self(),[]).



test() ->
  spawn(teacher_node, main, []), % teacher node
  sleep(2), % waiting teacher
  PID = spawn(fun() -> main_node() end),
  spawn(fun() -> main_node() end),
  spawn(fun() -> main_node() end),
  spawn(fun() -> main_node() end),
  spawn(fun() -> main_node() end),
  spawn(fun() -> main_node() end),
  sleep(5),
  exit(PID, kill),
  sleep(1000).

