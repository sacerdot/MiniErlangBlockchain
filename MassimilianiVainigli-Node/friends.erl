-module(friends).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-import(support, [sleep/1, filter_a/2, send_msg/2, addRef/2, add_friends/3, watch/2]).
-export([check_nodes/4]).

% Attore che si occupa di mantere la topologia della rete
% @param Parent = attore manager
% @param List_friends = lista di amici
% @param Attempts = tentativi falliti di aggiungere amici contattando un amico, arrivato a 10 chiede al nodo professore un nuovo amico
% @param List_Nonces = lista di identificativi dei messaggi che ci si aspetta di ricevere
check_nodes(Parent, List_friends, Attempts, List_Nonces) ->
  sleep(1),
  MyPid = self(),


  %%%%% Tentativi di aggiungere nuovi amici %%%%%


  %%se i miei ammici non mi suggeriscono più amici che posso aggiungere
  case Attempts=:=10 of
    true ->
      Ref1 = make_ref(),
      send_msg(teacher_node, {get_friends, Parent, Ref1}),
      receive
        {friends, Ref1, New_nodes1} ->
          %considero i nodi che posso aggiungere tra gli amici
          Nodes = filter_a(New_nodes1, List_friends) -- [Parent],
          if length(Nodes) > 0 ->
            %New_friend sarà uno di questi nodi scelto a caso
            New_friend = lists:nth(rand:uniform(length(Nodes)), Nodes),
            %aggiungo un amico e invio la nuova lista amici a Parant (Manager)
            spawn(fun() ->watch(MyPid, New_friend) end),
            Parent ! {update_friends, [New_friend]},
            check_nodes(Parent, [New_friend],0, List_Nonces);
            true -> ok
          end
      after 2000 ->  check_nodes(Parent, List_friends,Attempts, List_Nonces)
      end;
    false -> ok
  end,

  %% se non ho amici chiedo al teacher_node
  case length(List_friends) =:= 0 of
    true ->
      Ref = make_ref(),
      send_msg(teacher_node, {get_friends, Parent, Ref}),
      receive
        {friends, Ref, New_nodes} ->
          Nodes_without_me = New_nodes -- [Parent],
          if length(Nodes_without_me) > 0 ->
            %aggiungo un amico e lo comunico a manager
            New_friend1 = lists:nth(rand:uniform(length(Nodes_without_me)), Nodes_without_me),
            spawn(fun() ->watch(MyPid, New_friend1) end),
            Parent ! {update_friends, [New_friend1]},
            check_nodes(Parent, [New_friend1],Attempts, List_Nonces);
            true -> ok
          end
      after 2000 -> check_nodes(Parent, List_friends, Attempts, List_Nonces)
      end;
    false -> ok
  end,

  % NewREF = nuovo riferimento che mandiamo a un amico per avere la sua lista
  % Ogni volta che verrà fatta la chiamata ricorsiva NewRef verrà aggiunta a List_nonces
  NEWREF = if((length(List_friends) < 3) and (length(List_friends) > 0) ) -> make_ref(); true -> [] end,

  % se ho un numero di amici compreso tra [1,2] mando la richiesta a un amico casuale di passarmi la sua lista amici
  case (length(List_friends) < 3) and (length(List_friends) > 0) of
    true ->
       Random_friend =  lists:nth(rand:uniform(length(List_friends)), List_friends),
       send_msg(Random_friend,  {get_friends, Parent, NEWREF});
    false -> ok
  end,


  %%%%% Gestione dei messaggi che arrivano %%%%%


  receive
    %% messaggio ricevuto per verificare che l'attore sia ancora vivo, si risponde con Pong
    {ping, Sender, Ref2} ->
      send_msg(Sender, {pong, Ref2} );

    %% messaggio ricevuto per notificare la morte di un amico
    {dead, Node} ->
      % invio la lista aggiornata degli amici al manager
      Parent ! {update_friends, List_friends -- [Node]},
      check_nodes(Parent, List_friends -- [Node], Attempts, addRef(List_Nonces,[NEWREF]));

    %% un attore mi ha inviato la sua lista degli amici
    {friends, Ref4, Nodes1} ->

      RefisInMyList = lists:member(Ref4, List_Nonces ++ [NEWREF]),
      % se la reference ricevuta non è nella mia lista scarto il messaggio, altrimenti continuo
      case RefisInMyList of
        true-> ok;
        false -> check_nodes(Parent, List_friends, Attempts, addRef(List_Nonces,[NEWREF]) -- [Ref4])
      end,

      %% trovo i nodi che non sono nella mia lista e che non sono io
      Possible_friends = ((Nodes1 -- [Parent]) -- List_friends),

      %% se nella lista ricevuta non ci sono possibili amici da aggiungere scarto il messaggio e amento il numero ti tentativi fatti a vuoto
      case length(Possible_friends) == 0 of
        true -> check_nodes(Parent, List_friends, Attempts + 1,addRef(List_Nonces,[NEWREF]) -- [Ref4]);
        false ->
          % add_friends gestisce l'aggiunta di amici scelti casualmente
          % fino a quando finisce Possible_friends oppure la lista Lists_friends arriva a 3
          NewFriends = add_friends(List_friends, Possible_friends, Parent),
          % aggiorno il parent
          Parent ! {update_friends, NewFriends},
          check_nodes(Parent, NewFriends, Attempts, addRef(List_Nonces,[NEWREF])  -- [Ref4])
      end;


    %% un nodo mi chiede la mia lista amici
    {get_friends, Node, Ref3} ->

      % gli mando subito la mia lista amici
      send_msg(Node , {friends, Ref3, List_friends}),

      % se ho meno di 3 amici e la richiesta mi arriva da un nodo che non è nella lista lo aggiungo
      case lists:member(Node,List_friends) of
        true -> ok;
        false ->
          case length(List_friends) < 3 of
            true ->
              spawn(fun() ->watch(MyPid, Node) end),
              Parent ! {update_friends, List_friends ++ [Node]},
              check_nodes(Parent, List_friends ++ [Node], Attempts, addRef(List_Nonces,[NEWREF]));
            false -> ok
          end
      end;

    % il parent mi chiede la lista amici
    {list_of_friends, Nonce} ->
      Parent ! {List_friends, Nonce};

    % debug
    {stampa} ->
      io:format("Sono ~p e ho ~p amici, tentativi: ~p ~n", [Parent, List_friends, Attempts])

  after 1000 -> check_nodes(Parent, List_friends, Attempts, addRef(List_Nonces,[NEWREF]) )
  end,

  check_nodes(Parent, List_friends, Attempts, addRef(List_Nonces,[NEWREF]))
.



