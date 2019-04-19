-module(reconstruct_act).
-import (chain_tools , [reconstructing/4,checkBlock/1]).
-export([chainRestore/3]).

%! behavior dell'attore che ricostruisce la catena e la notifica a PidB
chainRestore(PidRoot,PidB,Chain) -> 
    receive  
        {updateChain, NewChain} ->
            % aggiorno la visione della catena a seguito di un nuovo blocco minato da me
            chainRestore(PidRoot,PidB,NewChain);
        {newBlock,Sender,Blocco} -> 
            case checkBlock(Blocco) of
                true ->  
                    % blocco valido
                    try 
                        reconstructing(PidRoot,Blocco,Chain,Sender)            
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
                            chainRestore(PidRoot,PidB,NewChain);
                        discarded -> 
                            % Il blocco ricevuto è stato scartato
                            chainRestore(PidRoot,PidB,Chain)
                    end;
                false ->
                    chainRestore(PidRoot,PidB,Chain)
            end

    end.

