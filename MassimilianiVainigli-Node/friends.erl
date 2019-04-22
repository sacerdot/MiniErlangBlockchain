-module(friends).
-export([check_nodes/4]).
-import(support, [sleep/1, filter_a/2]).

%% riferisce a Main della vita di Node
watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 ->  Main ! {dead, Node}
  end.


%% attore che si occupa di mantere la topologia della rete
% List_friends = lista di amici
% Attempts = tentativi falliti di aggiungere amici contattando un amico, arrivato a 10 si chiede al nodo professore
% List_Nonces = lista di identificativi dei messaggi che ci si aspetta di ricevere
check_nodes(Parent, List_friends, Attempts, List_Nonces) ->

  MyPid = self(),
  %io:format("Sono ~p e ho ~p amici attempts ~p List_nonces ~p ~n", [Parent, List_friends, Attempts, List_Nonces]),


  %%se mi sono bloccato
  if  (Attempts=:=10) ->
    Ref1 = make_ref(),
    teacher_node ! {get_friends, Parent, Ref1},
    receive
      {friends, Ref1, New_nodes1} ->
        Nodes = filter_a(New_nodes1, List_friends) -- [Parent],
        if length(Nodes) > 0 ->
          New_friend = lists:nth(rand:uniform(length(Nodes)), Nodes),

          spawn(fun() ->watch(MyPid, New_friend) end),
          Parent ! {update_friends, [New_friend]},
          check_nodes(Parent, [New_friend],0, List_Nonces);
          true -> ok
        end
    after 2000 ->  check_nodes(Parent, List_friends,Attempts, List_Nonces)
    end;
    true -> ok
  end,


  %% se non ho amici chiedo al teacher_node
  if
    (length(List_friends) =:= 0)->
      Ref = make_ref(),
      teacher_node ! {get_friends, Parent, Ref},
      receive
        {friends, Ref, New_nodes} ->
          Nodes_without_me = New_nodes -- [Parent],
          if length(Nodes_without_me) > 0 ->
            New_friend1 = lists:nth(rand:uniform(length(Nodes_without_me)), Nodes_without_me),
            spawn(fun() ->watch(MyPid, New_friend1) end),
            Parent ! {update_friends, [New_friend1]},
            check_nodes(Parent, [New_friend1],Attempts, List_Nonces);
            true -> ok
          end
      after 2000 -> check_nodes(Parent, List_friends, Attempts, List_Nonces)
      end;
    true -> ok
  end,


  % NewREF = nuovo riferimento che mandiamo a un amico per avere la sua lista
  % Ogni volta che verrà fatta la chiamata ricorsiva NewRef verrà aggiunta a List_nonces
  NEWREF = if((length(List_friends) < 3) and (length(List_friends) > 0) and (length(List_Nonces) < 10 ) ) -> make_ref(); true -> [] end,
  % se ho un numero di amici compreso tra [1,2] mando la richiesta a un amico casuale di passarmi la sua lista amici
  if
    ((length(List_friends) < 3) and (length(List_friends) > 0) and (length(List_Nonces) < 10 ) ) ->
      Random_friend =  lists:nth(rand:uniform(length(List_friends)), List_friends),
      Random_friend !  {get_friends, Parent, NEWREF};
    true -> ok
  end,



  %% gestione dei messaggi che arrivano
  receive

    {ping, Sender, Ref2} ->
      Sender ! {pong, Ref2};


    {dead, Node} ->
      Parent ! {update_friends, List_friends -- [Node]},
      check_nodes(Parent, List_friends -- [Node], Attempts,addRef(List_Nonces,[NEWREF]));


    {friends, Ref4, Nodes1} ->
      RefisInMyList = lists:member(Ref4, List_Nonces ++ [NEWREF]),
      if(RefisInMyList == false) ->   check_nodes(Parent, List_friends, Attempts, addRef(List_Nonces,[NEWREF]) -- [Ref4]); true->ok end,

      %% trovo i nodi che non sono nella mia lista e che non sono io
      Possible_friends = ((Nodes1 -- [Parent]) -- List_friends),

      if(length(Possible_friends) == 0) -> check_nodes(Parent, List_friends, Attempts + 1,addRef(List_Nonces,[NEWREF]) -- [Ref4]);
        true ->

          %% add_friends gestisce l'aggiunta di amici scelti casualmente
          %% fino a quando finisce Possible_friends oppure la lista Lists_friends arriva a 3
          NewFriends = add_friends(List_friends, Possible_friends, Parent),
          Parent ! {update_friends, NewFriends},
          check_nodes(Parent, NewFriends, Attempts, addRef(List_Nonces,[NEWREF])  -- [Ref4])
      end;



    {get_friends, Node, Ref3} ->

      Node ! {friends, Ref3, List_friends},

      %% se ho meno di 3 amici e la richiesta mi arriva da un nodo che non è nella lista lo aggiungo
      case lists:member(Node,List_friends)  of
        false ->
          if length(List_friends) < 3 ->
            spawn(fun() ->watch(MyPid, Node) end),
            Parent ! {update_friends, List_friends ++ [Node]},
            check_nodes(Parent, List_friends ++ [Node], Attempts, addRef(List_Nonces,[NEWREF]));
            true -> ok
          end;
        true ->ok
      end;


    {list_of_friends, Nonce} ->
      Parent ! {List_friends, Nonce};

    {stampa} ->
      io:format("Sono ~p e ho ~p amici, tentativi: ~p ~n", [Parent, List_friends, Attempts])

  after 1000 -> check_nodes(Parent, List_friends, Attempts, addRef(List_Nonces,[NEWREF]) )


  end,
  check_nodes(Parent, List_friends, Attempts, addRef(List_Nonces,[NEWREF]))
.



%% funzione ausiliaria
addRef(List, List2) ->  if(List2 == [[]] ) -> List; true -> List ++ List2 end.


%% gestione dell'aggiunta di elementi da una lista all'altra
add_friends(List1, [], _) -> List1;
add_friends(List1, List2, Parent) ->
  MyPid = self(),
  if
    length(List1) >= 3 -> List1;
    true ->
      RandomElement = lists:nth(rand:uniform(length(List2)), List2),
      spawn(fun() ->watch(MyPid, RandomElement) end),
      add_friends(List1++[RandomElement], List2--[RandomElement], Parent)
  end.