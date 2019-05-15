-module(ch_rec_handler).
-import(proof_of_work , [check/2]).
-export([chain_reconstruction_actor/5]).

% Codice dell'attore che ha il compito di ricostruire la catena
chain_reconstruction_actor(PID_bl_handler, PID_get_previous, Sender, Block, Bl_list) ->
  % io:format("Attore ricostruzione catena: ~p~n", [self()]),
  Blocks_to_add = chain_reconstruction(PID_get_previous, Sender, Bl_list, [Block]),
  case length(Blocks_to_add) of
    0 ->
      exit(fail);
    _ -> 
      % ho ricostruito una catena valida
      case isLonger(Bl_list, Blocks_to_add) of
        true ->
          % io:format("La catena ricostruita e' più corta~n~p~n", [Blocks_to_add]),
          exit(fail);
        Index ->
          % io:format("La catena ricostruita e' più lunga~n"),
          % Index è l'indice del primo blocco da sostituire
          Tr_bl_to_delete = lists:flatten([Tr || {_,_,Tr,_} <- lists:sublist(Bl_list, 1, Index)]),
          Tr_bl_to_add = lists:flatten([Tr || {_,_,Tr,_} <- Blocks_to_add]),
          New_bl_list = Blocks_to_add ++ Bl_list -- lists:sublist(Bl_list, 1, Index),
          % io:format("Catena ricostruita:~n~p~n", [New_bl_list]),
          PID_bl_handler ! {new_chain, New_bl_list, Tr_bl_to_add, Tr_bl_to_delete}
      end
  end.

% ricostruzione catena: dal blocco sconosciuto che ho ricevuto al blocco con 
% ID_preious_bl di un blocco della mia catena
chain_reconstruction(PID_get_previous, Sender, Bl_list, Blocks_to_add_list) ->
  % richiedo al Sender il blocco che non conosco
  {_, ID_blocco_sconosciuto, _, _} = lists:last(Blocks_to_add_list),
  Nonce = make_ref(),
  Self = self(),
  Sender ! {get_previous, Self, Nonce, ID_blocco_sconosciuto},
  % io:format("ch_rec_handler ~p in attesa del blocco sconosciuto...~n~p~n", [Self, ID_blocco_sconosciuto]),
  receive
    {previous, _, {ID_bl, ID_previous_bl, Bl_tr, Solution}} ->
      % verifico di aver ricevuto il blocco corretto che aspettavo
      case (check({ID_previous_bl, Bl_tr}, Solution)) and (ID_bl =:= ID_blocco_sconosciuto) of
        true ->
          % il blocco arrivato è quello che stavo aspettando
          PID_get_previous ! {block_added, {ID_bl, ID_previous_bl, Bl_tr, Solution}},
          % verifico se conosco il blocco precedente di quello arrivato
          case ID_previous_bl =:= none of
            true ->
              % ho ricostruito un'intera lista, restituisco il risultato
              Blocks_to_add_list ++ [{ID_bl, ID_previous_bl, Bl_tr, Solution}];
            false ->
              case bl_handler:find_BlockE(ID_previous_bl, Bl_list) of
                false ->
                  % non conosco neanche il blocco precedente. Aggiungo il blocco
                  % in coda e chiedo il precedente
                  chain_reconstruction(PID_get_previous, Sender, Bl_list, Blocks_to_add_list ++ [{ID_bl, ID_previous_bl, Bl_tr, Solution}]);
                _ ->
                  % conosco il bl precedente. ggiungo il blocco in coda e 
                  % restituisco il risultato
                  Blocks_to_add_list ++ [{ID_bl, ID_previous_bl, Bl_tr, Solution}]
              end
          end;
        false ->
          []
      end
  after 10000 -> []
  end.




% ======================== ricerca della catena più lunga =====================

% input: blocco, lista. Output: indice del blocco nella lista
index_of(_, [], _) -> not_found;
index_of(ID, [ID | _], Index) -> Index;
index_of(ID, [_|Tail], Index) -> index_of(ID, Tail, Index+1).

% Funzione che controlla quale catena è più lunga tra quella ricostruita e Bl_list
% (fino al blocco in comune). Output: true se Bl_list è più lunga, indice del
% primo blocco da sostituire se Bl_list è più corta.
isLonger(Bl_list, Bl_to_add_list) ->
  % recupero il blocco in comune alle due liste
  case lists:last(Bl_to_add_list) of
    % la catena ricostruita termina con none (le due catene non hanno blocchi in
    % comune) e Bl_list è più lunga
    {_, none, _, _} when length(Bl_list) > length(Bl_to_add_list) ->
      true;
    % la catena ricostruita termina con none (le due catene non hanno blocchi in
    % comune) e la catena ricostruita è più lunga di Bl_list
    {_, none, _, _} ->
      length(Bl_list);
    % la catena ricostruita termina con un blocco. Se il suo predecessore è un
    % blocco di Bl_list, esso è comune alle due catene
    {_, ID_blocco, _, _} ->
      case bl_handler:find_BlockE(ID_blocco, Bl_list) of
        % non c'è un blocco in comune: Bl_list è più lunga
        false -> true;
        % trovato blocco comune: si determina il suo indice e si confontano le lunghezze
        Block ->
          Index = index_of(Block, Bl_list, 1),
          case Index-1 > length(Bl_to_add_list) of
            true -> 
              % Bl_list più lunga
              true;
            false ->
              % Bl_list più corta: Index-1 = indice del 1o blocco da sostituire
              Index-1
          end
      end
  end.