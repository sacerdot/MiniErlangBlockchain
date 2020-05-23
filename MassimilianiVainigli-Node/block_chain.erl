%%%-------------------------------------------------------------------
%%% @author Lorenzo Massimiliani, Lorenzo Vainigli
%%%-------------------------------------------------------------------
-module(block_chain).
-author("Lorenzo Massimiliani, Lorenzo Vainigli").
-import(support, [all_elements_are_different/2, index_of_block/2, get_first_elements/2, send_msg/2, trasform_to_list/1]).
-export([mining/3, block_chain/3]).


% Attore che fa mining del blocco e lo manda al padre
mining(IDblocco_precedente,Lista_di_transazioni, Manager) ->
  Soluzione = proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni}),
  Manager ! {newblock, IDblocco_precedente, Lista_di_transazioni, Soluzione}.


% Attore che gestisce la catena di blocchi e manda update al manager
% @param ListBlocks = lista di blocchi che possiede il manager
% @param NewList = lista di blocchi aggiornata, inserita nei parametri per semplificare update
block_chain (Manager, ListBlocks, NewList) ->

  % se le due liste sono diverse mando update al manager e mi riaggiorno
  case ListBlocks =/= NewList of
    true ->
      Manager! {updateChain, NewList},
      block_chain(Manager, NewList, NewList);
    false -> ok
  end,


  receive

    % Messaggio ricevuto dell'attore che fa mining
    {newblock, IDBlocco, Lista_di_transazioni, Soluzione} ->

      %BlockTransactions conterrà l'elenco di tutte le transazioni presenti nella block chain
      BlockTransactions = lists:map(fun(Block) -> element(3, Block) end, ListBlocks),

      % Se ci sono transazioni comuni tra quelle del blocco minato e quelle nella block chain non aggiungo il nuovo blocco
      case all_elements_are_different(BlockTransactions, Lista_di_transazioni) of
        true -> ok;
        false -> block_chain(Manager, ListBlocks, NewList)
      end,

      % risalgo al id dell'ultimo blocco
      IDblocco_precedente = if length(ListBlocks) > 0 -> element(1,  lists:nth(length(ListBlocks), ListBlocks)); true -> 0  end,

      NEWID = make_ref(),
      % se i due id coincidono significa che il blocco che ho ricevuto è proprio quello che mi aspetto
      % altrimenti accetto il nuovo blocco anche se la mia block chain è vuota
      case (IDBlocco =:= IDblocco_precedente) or (length(ListBlocks) =:= 0) of
        true -> block_chain(Manager, ListBlocks, ListBlocks ++ [{NEWID, IDblocco_precedente, Lista_di_transazioni, Soluzione}]);
        false -> block_chain(Manager, ListBlocks, NewList)
      end;

    % Messaggio proveniente da un amico che ha aggiunto un nuovo blocco
    {update, Sender, Blocco } ->

      % se il blocco è nella mia lista non faccio niente
      IsBlockKnown = lists:any(fun(E) -> E == Blocco end, ListBlocks),
      case IsBlockKnown of
        true -> ok;
        false ->
          % blocco sconosciuto
          % estraggo i campi e verifico che il blocco sia corretto
          IDblocco_precedente = element(2, Blocco),
          Lista_di_transazioni = element(3, Blocco),
          Soluzione =  element(4, Blocco),
          Correct = proof_of_work:check({IDblocco_precedente, Lista_di_transazioni}, Soluzione),

          % estraggo l'Id del mio ultimo blocco, se la lista è vuota prendo 0
          IDultimo_mio_blocco= if length(ListBlocks) > 0 -> element(1,  lists:nth(length(ListBlocks), ListBlocks)); true -> 0 end,

          if
            % se il blocco è corretto e la mia block chain è vuota il blocco ricevuto sarà il primo mio blocco della catena
            Correct and (IDultimo_mio_blocco == 0) ->
              block_chain(Manager, ListBlocks, [Blocco]);

            % se il blocco è corretto e la mia block chain non è vuota avvio l'algoritmo di ricostruzione della catena
            Correct   ->
              % avvio algoritmo di ricostruzione della catena
              ricostruzioneCatena(Manager, ListBlocks, [Blocco], Sender, 0);

            true -> ok
          end
      end,
      block_chain(Manager, ListBlocks, NewList)

  end.



% chiedo al mittente del messaggio i suoi blocchi finche non ne trovo uno che abbia lo stesso ID di un mio blocco
% a quel punto mantengo la prima parte della mia catena (fino alla biforcazione) e ci aggiungo
% la catena più lunga tra quella che ho chisto al mittende e la rimanente parte della mia
% Attempts = tentavi in cui non si riceve la risposta  richiesta dal nodo
% arrivati a 10 ipotizziamo che il Sender sia morto
% altrimenti continuiamo a chiedere blocchi

% @param ListBlocks = mia blockchain
% @param SenderList = blocchi che mi ha inviato fino a questo momento il sender
ricostruzioneCatena(Manager, ListBlocks, SenderList, Sender, Attempts) ->

  % probabilmente il mittente è morto mi comporto come se non avessi ricevuto per messaggio quel blocco
  if(Attempts == 10 ) -> block_chain(Manager, ListBlocks, ListBlocks); true -> ok end,

  % il primo blocco è quello nuovo che devo ancora esaminare
  NuovoBlocco = lists:nth(1, SenderList),
  IsBlockKnown = lists:any(fun(E) -> E == SenderList end, ListBlocks),


  % se il blocco è conosciuto o sono arrivato al primo blocco del mittente devo ricostruire la catena

  LenMyList = length(ListBlocks),
  LenSend = length(SenderList),
  % longer2 conterra la catena piu lunga
  Longer2 = case LenMyList >= LenSend of true -> ListBlocks; false -> SenderList  end,

  % se l'ultimo blocco ricevuto del mittente è il suo primo blocco
  % quindi non abbiamo trovato blocchi in comune
  % la catena più lunga tra le due diventerà la nuova blockchain
  if ((element(2,NuovoBlocco) =:= 0) or (element(2,NuovoBlocco) =:= none)) -> block_chain(Manager, ListBlocks, Longer2);

    IsBlockKnown  ->
      % Index = posizione del nuovo blocco nella mia catena
      Index = index_of_block(ListBlocks, NuovoBlocco ),
      % LenSender sara data dai blocchi nuovi ricevuti dal sender + la lunghezza della catena comune al sender e alla mia
      LenSender = length(SenderList) + Index,

      % se la lista del sender è maggiore della mia
      % la mia nuova catena sarà data dalla parte della catena che possiedo (e ritengo essere comune)
      % piu i blocchi nuovi ricevuti dal sender
      LongerList = if(LenSender > LenMyList) -> get_first_elements(ListBlocks,Index ) ++ SenderList; true -> ListBlocks  end,
      block_chain(Manager, ListBlocks, LongerList);

    %% altrimenti chiedo un altro blocco al mittente
    true ->
      Nonce = make_ref(),
      send_msg(Sender , {get_previous, self(), Nonce, element(2,NuovoBlocco)}) ,

      receive
        % messaggio del sender che contiene il blocco che ho richiesto
        {previous, Nonce, Blocco} ->
          IsPresent = lists:member(Blocco, ListBlocks),

          % se lo possiedo non mi interessa
          case IsPresent of
            true -> ricostruzioneCatena(Manager, ListBlocks,SenderList, Sender, 0);
            false ->
              case length(Blocco) =:= 0 of
                true ->
                  % posso prendere la catena piu lunga
                  case length(SenderList) > length(ListBlocks) of
                    true -> block_chain(Manager, ListBlocks, SenderList);
                    false -> block_chain(Manager, ListBlocks, ListBlocks)
                  end;
                false ->
                  % altrimenti aggiungo il blocco a quelli gia ricevuti e continuo con la ricostruzione della catena
                  Blocco2 = case is_list(Blocco) of true -> Blocco; false ->[Blocco] end,
                  ricostruzioneCatena(Manager, ListBlocks, Blocco2 ++ SenderList, Sender, 0)
          end
          end

      % non ho ricevuto risposta, aumento il contatore dei tentativi e riprovo
      after 20 ->
        ricostruzioneCatena(Manager, ListBlocks, SenderList, Sender, Attempts + 1)
      end
  end
.