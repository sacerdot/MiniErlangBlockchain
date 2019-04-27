-module(block_act).
-import (miner_act , [start_M_act/2]).
-import (chain_tools , [buildInitChain/1,searchBlock/2]).
-import (block_gossiping_act , [blockGossiping/4,test/0]).
-import (reconstruct_act , [chainRestore/3]).

-import (utils , [sendMessage/2,sleep/1]).

-export([start_B_act/2]).


compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining) ->
    receive
        {getPidMiner} ->
            % ReconstructrAct mi ha chiesto il pidM per fare kill 
            spawn(fun()-> PidRestore ! {pidMiner, PidM} end),
            compute(NameNode,PidRoot,PidRestore,PidBlockG,none,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {printTM} ->
            io:format("[~p,~p] Le Transazioni da minare sono: ~p~n",[NameNode,PidRoot,T_ToMine]),
            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {printC} ->
            io:format("[~p,~p] CATENA(~p): ~p~n",[NameNode,PidRoot,length(Chain),Chain]),
            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {minerReady,PidM} when length(T_ToMine) =/= 0  ->
                % vengono rimosse da T_ToMine quando il miner ha finito
                New_T_ToMine = T_ToMine -- T_Mined, % rimuove eventualmente le T già minate
                New_T_In_Mining = lists:sublist(New_T_ToMine,10),
                case length(New_T_ToMine) =:= 0 of
                    true -> self() ! {minerReady,PidM};
                    false -> 
                        spawn(
                            fun() -> 
                                case length(Chain) =:= 0 of
                                    true -> 
                                        PidM ! {createBlock, none, New_T_In_Mining};
                                    false ->
                                        [{ID_Head,_,_,_}| _] = Chain,
                                        PidM ! {createBlock, ID_Head, New_T_In_Mining}
                                end
                        end)
                end,
                compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{New_T_ToMine,T_Mined},New_T_In_Mining);

        %transazione ricevuta da PidT: controllo se non è stata già inserita in qualche blocco
        {pushLocal, Transazione} -> 
            case lists:member(Transazione,T_Mined) or lists:member(Transazione,T_ToMine)  of
                true -> 
                    % già stata minata oppure già inserita in T_ToMine
                    compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
                false ->
                    compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{[Transazione]++T_ToMine,T_Mined},T_In_Mining)
            end;
        % blocco ricevuto da Root: controllo se devo fare gossiping
        {updateLocal,Sender,Blocco}->
            % quando mi arriva un blocco controllo le T al suo interno:
            % se ci sono transazioni già minate o T in T_In_Mining non lo accetto 
            % ma tengo in considerazione quelle
            % transazioni che non ho (nè in T_ToMine ne in T_Mined)
            % mando a PidBlockG il blocco che verifica se fare gossiping
            % avviare la fase di ricostruzione della catena
            PidBlockG ! {updateLocal,Sender,Blocco},
            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {get_previous, Mittente, Nonce, ID_Blocco} ->
            % il seguente attore cerca nella catena il blocco con ID_Blocco
            % e se lo trova risponde, altrimenti no
            spawn(fun()-> 
                Res = searchBlock(ID_Blocco,Chain),
                case Res =:= none of
                    true -> nothing_to_send;
                    false -> sendMessage(Mittente,{previous, Nonce, Res})
                end
            end),
            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
        {get_head, Mittente, Nonce} ->
            spawn(fun() ->
                % se Chain è Empty non mando nulla
                case length(Chain) =:= 0 of
                    true -> 
                        sendMessage(Mittente , {head,Nonce,none});
                    false -> 
                        [Head | _] = Chain, 
                        sendMessage(Mittente , {head,Nonce,Head})
                end 
            end),
            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},T_In_Mining);
            
        {updateMyChain,{TMinedFromUpdate,NewChain}} -> 
                %* ReconstructAct mi ha notificato la nuova catena e le nuove transazioni inserite
                %* e quindi PidM sicuramente non esiste più
                
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
                compute(NameNode,
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
                            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,[Blocco] ++ Chain,{New_T_ToMine,New_T_Mined},[]);
                        false -> 
                            % per qualche motivo il blocco minato non coincide più con la testa della catena
                            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine--T_Mined,T_Mined},[])                                
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
                            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,[Blocco] ++ Chain,{New_T_ToMine,New_T_Mined},[]);
                        false ->
                            % per qualche motivo il blocco minato non coincide più con la testa della catena
                            compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine--T_Mined,T_Mined},[])
                    end
                end
    end.


start_B_act(NameNode,PidRoot) -> 
    sleep(10),
    PidB = self(),
    T_ToMine = [],
    {Chain,T_Mined} = buildInitChain(PidRoot),
    % io:format("[~p] Catena: ~p, Transazioni: ~p~n",[PidRoot,Chain,T_Mined]),
    % ottenuta la catena prendo tutte le transazioni all'interno e le inserisco nella
    % lista di transazione già minate -> T_Mined
    % Attore per la ricostruzione della catena
    PidM = spawn(fun() -> start_M_act(PidRoot,PidB) end),
    PidRestore = spawn(fun() -> chainRestore(PidRoot,PidB,Chain) end),
    % Attore per il gossiping 
    sleep(1),
    PidBlockG = spawn(fun() -> 
        % lista blocchi conosciuti (inizialmente è la catena)
        blockGossiping(PidRoot,PidB,PidRestore,Chain)
    end),
    compute(NameNode,PidRoot,PidRestore,PidBlockG,PidM,Chain,{T_ToMine,T_Mined},[]).