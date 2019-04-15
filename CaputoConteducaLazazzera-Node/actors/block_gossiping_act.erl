-module(block_gossiping_act).
-export([blockGossiping/4]).


%! behavior dell'attore che ha il compito di :
%! - fare gossiping del Blocco ricevuto (chiedendo a PidRoot la lista di amici)
%!   > se il blocco è conoscito nulla, altrimenti
%!  il blocco nella lista di blocchi conosciuti
%! - Avvisa PidRestore di avviare la fase di ricostruzione
blockGossiping(PidRoot,PidB,PidRestore,Blocks) ->
    receive
        % PidB mi ha notificato l'arrivo di un nuovo blocco
        {updateLocal,Sender,Blocco} -> 
            case lists:member(Blocco,Blocks) of
                true -> 
                    % se lo conosco non faccio nulla
                    blockGossiping(PidRoot,PidB,PidRestore,Blocks);
                false ->
                    % se non lo conosco:
                    % avvio il gossiping (chiedo a PidRoot gli amichetti) 
                    spawn(fun() -> sendBlock(PidRoot,Blocco) end), 
                    % avvio PidRestore di avviare la ricostruzione
                    PidRestore ! {newBlock, Sender,Blocco},
                    % aggiungo il blocco alla lista dei blocchi
                    blockGossiping(PidRoot,PidB,PidRestore,[Blocco]++Blocks)
            end;
        {updateMinedBlock, Blocco} ->
            % è stato creato un nuovo blocco
            spawn(fun() -> sendBlock(PidRoot,Blocco) end),    
            blockGossiping(PidRoot,PidB,PidRestore,[Blocco]++Blocks)
    end.

sendBlock(PidRoot,Blocco) ->
        % chiedo gli amici a PidRoot
        PidRoot ! {checkFriendsList, self()},
        receive 
            {myFriendsList, FriendsList} ->
                % gossiping del nuovo blocco a nome di PidRoot
                [ X ! {update,PidRoot, Blocco} || X <- FriendsList] 
        end.