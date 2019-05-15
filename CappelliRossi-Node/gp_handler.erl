-module(gp_handler).
-export([gp_actor/1]).

% attore get_previous_handler:
% gestore dei messaggi get_previous a cui non si riesce a dare immediatamente
% risposta perché non è conosciuto il blocco richiesto.
% mailbox = lista di tuple {Mittente, Nonce, Id_blocco_richiesto}
gp_actor(Mailbox) ->
  % io:format("gp_handler ~p start with mailbox ~p~n",[self(), Mailbox]),
  
  receive
  
    % ricezione di messaggi get_previous:
    % il blocco richiesto non è contenuto nella blockchain -> inserisco la
    % richiesta in coda alla Mailbox
    {get_previous, Mittente, Nonce, ID_blocco_precedente} ->
      % io:format("gph riceve get_previous~n"),
      gp_actor(Mailbox ++ [{Mittente,Nonce,ID_blocco_precedente}]);

    % ricezione di messaggi block_added:
    % è stato aggiunto un blocco alla blockchain -> verifico se qualcuno stava
    % aspettando di ricevere quel blocco e in caso affermativo lo invio a lui
    {block_added, {IDb, IDpb, Tr, Sol}} ->
      % io:format("gph riceve block_added: "),
      case find_BlockE_MB(IDb, Mailbox) of
        false ->
          % io:format("nessuno aspettava il blocco~n"),
          gp_actor(Mailbox);
        {Mittente, Nonce, ID_blocco} ->
          % io:format("blocco inviato a chi lo attendeva~n"),
          Mittente ! {previous, Nonce, {IDb, IDpb, Tr, Sol}},
          gp_actor(Mailbox -- [{Mittente, Nonce, ID_blocco}])
      end

  end.




% ====== ricerca di un blocco nella Mailbox del get_previous_handler ==========

% caso: ricerca di un blocco nella mailbox vuota
find_BlockR_MB(_, Mailbox) when length(Mailbox) =:= 0 ->
  false;
% caso: ricerca di un blocco nella Mailbox non vuota
find_BlockR_MB(ID_block_to_find, Mailbox) ->
  [{Mittente, Nonce, ID_first_block} | Tail]  = Mailbox,
  case ID_block_to_find =:= ID_first_block of
    true ->
      {Mittente, Nonce, ID_first_block};
    false ->
      find_BlockR_MB(ID_block_to_find, Tail)
  end.
% funzione con eccezioni
find_BlockE_MB(ID_Block_to_find, Mailbox) ->
  try
    find_BlockR_MB(ID_Block_to_find, Mailbox)
  catch
    El_Mailbox -> El_Mailbox
  end.
