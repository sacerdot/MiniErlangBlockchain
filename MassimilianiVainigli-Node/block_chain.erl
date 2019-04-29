-module(blockChain).
-import(support, [all_elements_are_different/2, index_of_block/2, get_first_elements/2, send_msg/2]).
-export([mining/3, block_chain/3]).



%% attore che fa mining del blocco e lo manda al padre
mining(IDblocco_precedente,Lista_di_transazioni, Manager) ->
  Soluzione = proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni}),
  Manager ! {newblock, IDblocco_precedente, Lista_di_transazioni, Soluzione}.



%% Attore che gestisce la catena di blocchi e manda update al manager
%  NewList = lista di blocchi aggiornata, inserita nei parametri per semplificare update
block_chain (Manager, ListBlocks, NewList) ->


  % se le due liste sono diverse mando update al manager e mi riaggiorno
  if(ListBlocks =/= NewList) ->
    Manager! {updateChain, NewList},
    block_chain(Manager, NewList, NewList);
    true -> ok
  end,


  receive

  % messaggio ricevuto dell'attore che fa mining
    {newblock, IDBlocco, Lista_di_transazioni, Soluzione} ->
      BlockTransactions = lists:map(fun(Block) -> element(3, Block) end, ListBlocks),

      % Se ci sono elementi comuni non aggiungo il blocco
      DiffElement = all_elements_are_different(BlockTransactions, Lista_di_transazioni),
      if(DiffElement==false) -> block_chain(Manager, ListBlocks, NewList); true->ok end,

      IDblocco_precedente = if length(ListBlocks) > 0 -> element(1,  lists:nth(length(ListBlocks), ListBlocks)); true -> 0  end,

      NEWID = make_ref(),

      if( (IDBlocco =:= IDblocco_precedente) or (length(ListBlocks) =:= 0)) ->
        block_chain(Manager, ListBlocks, ListBlocks ++ [{NEWID, IDblocco_precedente, Lista_di_transazioni, Soluzione}]);
        true -> block_chain(Manager, ListBlocks, NewList)
      end;


  % messaggio che indica la scoperta di un nuovo blocco
    {update, Sender, Blocco } ->
      IsBlockKnown = lists:any(fun(E) -> E == Blocco end, ListBlocks),
      if
        IsBlockKnown -> ok;
      % blocco sconosciuto
        true ->

          Myblocco_precedente = if length(ListBlocks) > 0 -> element(1,  lists:nth(length(ListBlocks), ListBlocks)); true -> 0 end,
          IDblocco_precedente = element(2, Blocco),
          Lista_di_transazioni = element(3, Blocco),
          Soluzione =  element(4, Blocco),
          Correct = proof_of_work:check({IDblocco_precedente, Lista_di_transazioni}, Soluzione),
          if
            Correct and (IDblocco_precedente =:= Myblocco_precedente) ->
              % devo ricostruire la catena
              ricostruzioneCatena(Manager, ListBlocks, [Blocco], Sender, 0);
            true -> ok
          end
      end,
      block_chain(Manager, ListBlocks, NewList)

  end.



% chiedo al mittente del messaggio i suoi blocchi finche non ne trovo che abbia lo stesso ID di un mio blocco
% a quel punto mantengo la prima parte della mia catena (fino alla biforcazione) e ci aggiungo
% la catena più lunga tra quella che ho chisto al mittende e la rimanente parte della mia
% Attempts = tentavi in cui non si riceve la risposta richiesta dal nodo
% arrivati a 10 ipotizziamo che il Sender sia morto
% altrimenti continuiamo a chiedere blocchi
ricostruzioneCatena(Manager, ListBlocks, SenderList, Sender, Attempts) ->

  if(Attempts == 10 ) -> block_chain(Manager, ListBlocks, ListBlocks); true -> ok end,

  % il primo blocco è quello nuovo che devo ancora esaminare
  NuovoBlocco = lists:nth(1, SenderList),
  IsBlockKnown = lists:any(fun(E) -> E == SenderList end, ListBlocks),



  % se il blocco è conosciuto o sono arrivato al primo blocco del mittente
  % devo ricostruire la catena
  if (IsBlockKnown) or  (element(2,NuovoBlocco) =:= 0) ->

    % posizione del nuovo blocco nella mia catena
    Index = index_of_block(ListBlocks, NuovoBlocco ),
    LenSender = length(SenderList) + length(ListBlocks ) - Index,
    LenMyList = length(ListBlocks),

    LongerList = if(LenSender > LenMyList) -> get_first_elements(ListBlocks,Index) ++ SenderList; true -> ListBlocks  end,
    block_chain(Manager, ListBlocks, LongerList);


  %% altrimenti chiedo un altro blocco al mittente
    true ->
      Nonce = make_ref(),
      Sender ! {get_previous, self(), Nonce, element(2,NuovoBlocco)} ,

      receive
        {previous, Nonce, Blocco} ->

          if(length(Blocco) =:= 0 ) ->
            if(length(SenderList) > length(ListBlocks)) ->
              block_chain(Manager, ListBlocks, SenderList);
              true -> block_chain(Manager, ListBlocks, ListBlocks)
            end;

            true ->
              ricostruzioneCatena(Manager, ListBlocks, Blocco ++ SenderList, Sender, 0)
          end

      after 20 ->
        ricostruzioneCatena(Manager, ListBlocks, SenderList, Sender, Attempts + 1)
      end
  end
.