-module(miner_act).
-export([start_M_act/2]).
-import (proof_of_work , [solve/1,check/2]).

-on_load(load_module_act/0).

load_module_act() ->
    compile:file('proof_of_work.erl'), 
    ok. 

% sleep(N) -> receive after N*1000 -> ok end.

% riceviamo da PidBlock e mandiamo a PidRoot per notificare la creazione di un nuovo blocco

start_M_act(PidRoot, PidBlock) -> 
    io:format("[~p]: sono l'attore Miner di ~p~n",[self(),PidRoot]),
    % sleep(20),
    PidBlock ! {minerReady},
    receive 
        {createBlock, ID_blocco_prev, Transactions} ->
            io:format("Sto minando il BLOCCO: ~p ~n",[Transactions]),
            Solution = proof_of_work:solve({ID_blocco_prev, Transactions}),
            B = {make_ref(), ID_blocco_prev, Transactions, Solution},
            % dopo aver minato chiedo la lista di amici per poi notificare B_Act di fare 
            % gossiping del nuovo blocco minato
            PidRoot ! {checkList, self()},
            receive 
                {myList, FriendsList} ->
                    PidBlock ! {updateMyBlockLocal, B, FriendsList},
                    start_M_act(PidRoot,PidBlock)
            end
        after 5000 ->
            % se non ricevo msg createBlock per minare mi rilooppo 
        start_M_act(PidRoot, PidBlock) 
    end,
    start_M_act(PidRoot, PidBlock).