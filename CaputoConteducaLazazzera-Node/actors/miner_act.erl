-module(miner_act).
-export([start_M_act/2]).
-import (proof_of_work , [solve/1,check/2]).
sleep(N) -> receive after N*1000 -> ok end.

compute(PidRoot,PidBlock) ->
    % io:format("[~p]: sono l'attore Miner di ~p~n",[self(),PidRoot]),
    PidBlock ! {minerReady,self()},
    % mi metto in attesa di un nuovo set di transazioni da inserire nel blocco
    receive 
    {createBlock, ID_blocco_prev, Transactions} ->
        % io:format("-> [~p] Sto minando il BLOCCO: ~p~n",[PidRoot,Transactions]),
        Solution = proof_of_work:solve({ID_blocco_prev, Transactions}),
        % Matteo VERSION
        % OrderTrans = lists:sort(Transactions),
        % Nums = lists:flatmap(fun(X)-> {R, _}=X, returnLastNum(R) end, OrderTrans),
        % B = {Nums, ID_blocco_prev, Transactions, Solution},

        % Prof Version
        B = {make_ref(), ID_blocco_prev, Transactions, Solution},
        PidBlock ! {miningFinished, B},
        compute(PidRoot,PidBlock)
    end.

returnLastNum(Ref) ->
    [RefString] = io_lib:format("~p", [Ref]),
    {match, L} = re:run(RefString, "#Ref<(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)>",[{capture, all_but_first, list}]),
    lists:nth(length(L),L).
    


start_M_act(PidRoot,PidBlock) -> 
    sleep(2),
    compute(PidRoot,PidBlock).   