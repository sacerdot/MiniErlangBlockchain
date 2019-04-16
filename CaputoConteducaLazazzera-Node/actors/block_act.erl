-module(block_act).
-import (miner_act , [start_M_act/2]).
-import (chain_tools , [validityBlock/2,buildInitChain/1,reconstructing/3,searchBlock/2,checkBlock/1]).
-import (block_gossiping_act , [blockGossiping/4,test/0]).
-import (utils , [sendMessage/2]).

-export([start_B_act/1]).

% TODO : CHIEDERE A SENDER O AMICI IL BLOCCO PRECEDENTE

%! behavior dell'attore che ricostruisce la catena e la notifica a PidB
chainRestore(PidB,Chain) -> 
    %? Caso 0 (EASY):
    %?     Chain è vuota. Inserisco il Blocco direttamente

    %? Caso 1 (EASY):
    %?     Blocco va inserito in cima alla mia catena --> ottimo
    %?     ID_Prev di Blocco è la testa di Chain 

    %? Caso 2 (SUPER-EASY):
    %?     ID_Prev di Blocco non è la testa ma un blocco più 
    %?     vecchio della catena --> lo scarto

    %? Caso 3 (HARD):
    %?     Se ID_Prev di Blocco non fa parte della mia catena 
    %?     chiedo al Sender di invarmi il Blocco ID_Prev in modo
    %?     da capire il punto della biforcazione
    %?     trovato il punto di biforcazione (conosco l'ID_Prev)
    %?     confronto le catene
    receive  
        {updateChain, NewChain} ->
            % aggiorno la visione della catena a seguito di un nuovo blocco minato da me
            chainRestore(PidB,NewChain);
        {newBlock,Sender,Blocco} -> 
            case checkBlock(Blocco) of
                true ->  % blocco valido
                    try 
                        reconstructing(Blocco,Chain,Sender)            
                    catch
                        {done,NewChain,NewTMined} -> 
                            PidB ! {getPidMiner},
                            receive 
                                {pidMiner, PidM} ->
                                    % killo il miner
                                    % io:format("Miner killed (~p)~n",[PidM]),
                                    exit(PidM,kill)
                            end,
                            PidB !  {updateMyChain,{NewTMined,NewChain}},
                            chainRestore(PidB,NewChain);
                        discarded -> 
                            % Il blocco ricevuto è stato scartato
                            chainRestore(PidB,Chain)
                    end;
                false ->
                    chainRestore(PidB,Chain)
            end

    end.

compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining) ->
    receive
        {getPidMiner} ->
            % ReconstructrAct mi ha chiesto il pidM per fare kill 
            PidRestore ! {pidMiner, PidM},
            compute(PidRoot,PidRestore,PidBlockG,none,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {printTM} ->
            io:format("[~p] Le Transazioni da minare sono: ~p~n",[PidRoot,T_ToMine]),
            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {printC} ->
            io:format("[~p] CATENA (~p): ~p~n",[PidRoot,length(Chain),Chain]),
            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {minerReady,PidM} when length(T_ToMine) =/= 0 ->
                % vengono rimosse da T_ToMine quando il miner ha finito
                A = T_ToMine --T_Mined --T_In_Mining,
                New_T_In_Mining = lists:sublist(A,10),
                % io:format("~n Invio New T a ~p LengthChain:~p~n",[PidM, length(Chain)]),
                case length(Chain) =:= 0 of
                    true -> 
                        PidM ! {createBlock, none, New_T_In_Mining};
                    false ->
                        [{ID_Head,_,_,_}| _] = Chain,
                        PidM ! {createBlock, ID_Head, New_T_In_Mining}
                end,
                compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},New_T_In_Mining);

        %transazione ricevuta da PidT: controllo se non è stata già inserita in qualche blocco
        {pushLocal, Transazione} -> 
            case lists:member(Transazione,T_Mined) of
                true -> 
                    % già stata minata
                    compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
                false ->
                    case lists:member(Transazione,T_ToMine) of
                        true ->
                            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
                        false ->
                            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{[Transazione]++T_ToMine,T_Mined},T_In_Mining)
                    end
            end;
        % blocco ricevuto da Root: controllo se devo fare gossiping
        {updateLocal,Sender,Blocco}->
            % quando mi arriva un blocco controllo le T al suo interno:
            % se ci sono transazioni già minate o T in T_In_Mining non lo accetto 
            % ma tengo in considerazione quelle
            % transazioni che non ho (nè in T_ToMine ne in T_Mined)
            {NewToMine, NewTMined, Reconstr} = validityBlock({T_ToMine,T_Mined},Blocco), %in chain_tools.erl
            % mando a PidBlockG il blocco che verifica se fare gossiping
            % avviare la fase di ricostruzione della catena
            PidBlockG ! {updateLocal,Sender,Blocco,Reconstr},
            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{NewToMine,NewTMined},T_In_Mining);
        {get_previous, Mittente, Nonce, ID_Blocco} ->
            % il seguente attore cerca nella catena il blocco con ID_Blocco
            % e se lo trova risponde, altrimenti no
            spawn(fun()-> 
                case searchBlock(ID_Blocco,Chain) of
                    none -> nothing_to_send;
                    Blocco -> 
                        sendMessage(Mittente,{previous, Nonce, Blocco})
                end 
            end),
            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {get_head, Mittente, Nonce} ->
            spawn(fun() ->
                % se Chain è Empty non mando nulla
                case length(Chain) =:= 0 of
                    true -> nothing_to_send;
                    false -> 
                        [Head | _] = Chain, 
                        sendMessage(Mittente , {head,Nonce,Head})
                end 
            end),
            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
            
        {updateMyChain,{TMinedFromUpdate,NewChain}} -> 
                %* ReconstructAct mi ha notificato la nuova catena e le nuove transazioni inserite
                % io:format("~n~n[~p] My new chain -> ~p ~n(Length -> ~p)~n",[PidRoot,NewChain,length(NewChain)]),
                
                % Le Transazioni su cui stava lavorando il Miner 
                % sono ancora nella lista delle transazioni da minare
                % TMinedFromUpdate sono le T del blocco accettato dall'update
                % perciò aggiorno la lista di T_Mined 
                New_T_Mined = TMinedFromUpdate ++ T_Mined,
                % ed eventualmente la lista di T_ToMine
                New_T_ToMine = T_ToMine -- New_T_Mined,
                % creo un nuovo miner
                PidB = self(),                
                NewPidM = spawn(fun() -> start_M_act(PidRoot,PidB) end),
                % io:format("[~p] New Miner created (~p)~n",[PidRoot,NewPidM]),
                compute(
                    PidRoot,PidRestore,PidBlockG,
                    NewPidM,
                    NewChain,{New_T_ToMine,New_T_Mined},[]);
        {miningFinished, Blocco} -> % il miner ha terminato
            {_,ID_Prev_Block_Mined,T_In_Mined_Block,_} = Blocco,

            case length(Chain) =:= 0 of
                true ->
                    % ID_PREV_BLOCK deve essere none
                    case ID_Prev_Block_Mined =:= none of
                        true -> 
                            % aggiorno la lista di Transazioni minate 
                            New_T_Mined = T_In_Mined_Block ++ T_Mined,
                            New_T_ToMine = T_ToMine -- T_In_Mined_Block,
                            PidRestore ! {updateChain, [Blocco] ++ Chain}, %avverto il ricostruttore della nuova visione
                            PidBlockG ! {updateMinedBlock, Blocco}, % gossiping del blocco
                            compute(PidRoot,PidRestore,PidBlockG,PidM,[Blocco] ++ Chain,{New_T_ToMine,New_T_Mined},[]);
                        false -> 
                            % per qualche motivo il blocco minato non coincide più con la testa della catena
                            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine--T_Mined,T_Mined},[])                                
                    end;
                false -> 
                    [{ID_Head,_,_,_} | _ ] = Chain,
                    case ID_Head =:= ID_Prev_Block_Mined of
                        true ->
                            % aggiorno la lista di Transazioni minate 
                            New_T_Mined = T_In_Mined_Block ++ T_Mined,
                            New_T_ToMine = T_ToMine -- T_In_Mined_Block,
                            %avverto il ricostruttore della nuova visione
                            PidRestore ! {updateChain, [Blocco] ++ Chain},
                            PidBlockG ! {updateMinedBlock, Blocco}, % gossiping del blocco
                            compute(PidRoot,PidRestore,PidBlockG,PidM,[Blocco] ++ Chain,{New_T_ToMine,New_T_Mined},[]);
                        false ->
                            % per qualche motivo il blocco minato non coincide più con la testa della catena
                            compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine--T_Mined,T_Mined},[])
                    end
                end;

        lastcase -> nothing_to_do
    end.

sleep(N) -> receive after N*1000 -> ok end.

start_B_act(PidRoot) -> 
    sleep(10),
    PidB = self(),
    T_ToMine = [],
    {Chain,T_Mined} = buildInitChain(PidRoot),
    % io:format("[~p] Catena: ~p, Transazioni: ~p~n",[PidRoot,Chain,T_Mined]),
    % ottenuta la catena prendo tutte le transazioni all'interno e le inserisco nella
    % lista di transazione già minate -> T_Mined
    % Attore per la ricostruzione della catena
    PidM = spawn(fun() -> start_M_act(PidRoot,PidB) end),
    PidRestore = spawn(fun() -> chainRestore(PidB,Chain) end),
    % Attore per il gossiping 
    sleep(1),
    PidBlockG = spawn(fun() -> 
        % lista blocchi conosciuti (inizialmente è la catena)
        blockGossiping(PidRoot,PidB,PidRestore,Chain)
    end),
    compute(PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},[]).