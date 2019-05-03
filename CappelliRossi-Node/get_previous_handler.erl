-module(get_previous_handler).
-export([get_previous_handler_main/1, find_BlockE/2]).

% ======== ricerca di un blocco nella blockchain dato il suo ID ===============
%                 (funzione ricorsiva con eccezioni)

% caso: ricerca di un blocco nella blockchain vuota
find_BlockR(_, BlockChain) when length(BlockChain) =:= 0 ->
  false;
% caso: ricerca di un blocco nella blockchain non vuota
find_BlockR(ID_Block_to_find, BlockChain) ->
  [{ID_first_block, ID_pb, Tr, Sol} | Tail]  = BlockChain,
  case ID_Block_to_find =:= ID_first_block of
    true ->
      {ID_first_block, ID_pb, Tr, Sol};
    false ->
      find_BlockR(ID_Block_to_find, Tail)
  end.

% funzione con eccezioni
find_BlockE(ID_Block_to_find, BlockChain) ->
  try
    find_BlockR(ID_Block_to_find, BlockChain)
  catch
    Blocco -> Blocco
  end.

% ====== ricerca di un blocco nella Mailbox del get_previous_handler ==========
%                 (funzione ricorsiva con eccezioni)

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

% ============ MAIN DELL'HANDLER DEI MESSAGGI get_previous ====================
%       (gestore dei messaggi get_previous a cui non si riesce a dare
%    immediatamente risposta perché non è conosciuto il blocco richiesto)

% INPUT: mailbox = lista di tuple {Mittente, Nonce, Id_blocco_richiesto}
get_previous_handler_main(Mailbox) ->
  %%io:format("hanlder start with mailbox ~p~n",[Mailbox]),
  receive
    % ricezione di messaggi get_previous:
    % il blocco richiesto non è contenuto nella blockchain -> inserisco la
    % richiesta in coda alla Mailbox
    {get_previous, Mittente, Nonce, ID_blocco_precedente} ->
      %io:format("hanlder receive get_previous~n"),
      %io:format("Mailbox ~p~n~n",[Mailbox ++ [{Mittente,Nonce,ID_blocco_precedente}]]),
      get_previous_handler_main(Mailbox ++ [{Mittente,Nonce,ID_blocco_precedente}]);

    % ricezione di messaggi block_added:
    % è stato aggiunto un blocco alla blockchain -> verifico se qualcuno stava
    % aspettando di ricevere quel blocco e in caso affermativo lo invio a lui
    {block_added, {IDb, IDpb, Tr, Sol}} ->
      %io:format("hanlder receive block_added~n"),
      case find_BlockE_MB(IDb, Mailbox) of
        false ->
          %io:format("Nessuno aspettava il blocco ricevuto~n~n"),
          get_previous_handler_main(Mailbox);
        {Mittente, Nonce, ID_blocco} ->
          %io:format("Blocco inviato a chi lo attenede~n~n"),
          Mittente ! {previous, Nonce, {IDb, IDpb, Tr, Sol}},
          get_previous_handler_main(Mailbox -- [{Mittente, Nonce, ID_blocco}])
      end

  end.
