-module(block_act).
-import (miner_act , [start_M_act/2]).
-import (proof_of_work , [solve/1,check/2]).
-export([start_B_act/1]).
-on_load(load_module_act/0).

load_module_act() ->
    compile:file('proof_of_work.erl'), 
    compile:file('actors/miner_act.erl'), 
    ok. 

%%%%%%%%  behavior dell'attore che costruisce i blocchi %%%%%%%%
% Blocco = {ID_Blocco, ID_Prev, Lista_transazioni, Soluzione}

% Ricerca Blocco tramite ID
searchBlock(ID,[{ID_Blocco,none,Lista_transazioni,Soluzione}]) -> 
    case ID =:= ID_Blocco of
        true -> [{ID_Blocco,none,Lista_transazioni, Soluzione}];
        false -> none
    end;
searchBlock(ID,[H | T]) -> 
    {ID_Blocco, _, _, _} = H,
    case ID =:= ID_Blocco of
        true -> H;
        false -> 
            searchBlock(ID,T)
    end.
    
% Index = 1 inizialmente
indexBlock(B,[H|T], Index) ->
    case B =:= H of
        true -> Index;
        false -> indexBlock(B,T,Index+1)
    end.


% getHead([H|_]) -> H; 
% getHead(_) -> none.
compute(PidRoot, PidMiner,Catena,ListT) -> 
    
    receive
        {get_chain} ->
                io:format("[~p] La mia catena è: ~p~n",[PidRoot,Catena]),
                compute(PidRoot, PidMiner, Catena, ListT);        
        {minerReady} -> 
            TransactionsToMine = lists:sublist(ListT,10),
            % Check: se la catena è vuota
            ID_blocco_prev = case length(Catena) > 0 of
                true -> 
                    [{_,IDPrev,_,_}|_] = Catena,
                    IDPrev;
                false -> none
            end,
            
            case length(TransactionsToMine) =:= 0 of
                true ->
                    compute(PidRoot, PidMiner, Catena, ListT);
                false ->
                    PidMiner ! {createBlock, ID_blocco_prev , TransactionsToMine},
                    receive
                        {updateMyBlockLocal, B, FriendsList} -> 
                            sendAll(B, Catena, FriendsList),
                            compute(PidRoot, PidMiner, [B] ++ Catena, ListT -- TransactionsToMine)
                    end
            end;
        {new_transaction,T} -> 
            % ricevo una possibile transazione da inserire
            % se mi arriva vuol dire che non ce l'ho
            compute(PidRoot, PidMiner,Catena,[T] ++ ListT);
        {updateLocal, B, FriendsList} -> 
            {_,ID_prev,ListTransBlock,Solution} = B,
            case check({ID_prev, ListTransBlock}, Solution) of
                true ->
                    % io:format("~nSOLUZIONE OKKKKK~n"),
                    %VERIFICARE CATENA + LUNGA
                    sendAll(B, Catena, FriendsList), % gossip del blocco
                    NewCatena = verifyCatena(Catena, FriendsList, FriendsList, [B]),
                    case length(NewCatena) > length(Catena) of
                        true ->
                            ListTNewBlocks = lists:flatmap(fun(A)->{_,X}=A, X end,NewCatena),
                            compute(PidRoot, PidMiner, NewCatena , ListT -- ListTNewBlocks); %rimozione di T già presenti in un blocco 
                        false ->
                            compute(PidRoot, PidMiner, NewCatena, ListT)
                    end;
                false -> 
                    % io:format("~nSOLUZIONE NOOOOOOOOO~n"),
                    compute(PidRoot, PidMiner, Catena, ListT)
            end;
      
        {get_previousLocal, Mittente, Nonce, Idblocco_precedente} ->
            % TODO: ricerca blocco con IDblocco_precedente
            % se non ce l'ho rispondo con none
            Blocco = searchBlock(Idblocco_precedente,Catena), 
            Mittente ! {previous, Nonce, Blocco},
            compute(PidRoot, PidMiner, Catena, ListT);
        {get_headLocal, Mittente,Nonce} ->
            case length(Catena) =:= 0 of
                true -> 
                    Mittente ! {head, Nonce, none},
                    compute(PidRoot, PidMiner, Catena, ListT);
                false -> 
                    [H | _] = Catena,
                    Mittente ! {head, Nonce, H},                
                    compute(PidRoot, PidMiner, Catena, ListT)
            end
    end.

start_B_act(PidRoot) -> 
    % creazione attore che mina
    PidBlock = self(),
    PidMiner = spawn(fun() -> start_M_act(PidRoot,PidBlock) end),
    % io:format("[~p]: sono l'attore Blocco di ~p~n",[self(),PidRoot]),
    compute(PidRoot,PidMiner,[], []). 

verifyCatena(Catena, FriendsList, FriendsListToAsk, CatenaB) ->
    [Head | _ ] = Catena,
    B = lists:nth(length(CatenaB),CatenaB),
    {_,ID_prev_block,_,_} = B, %pattern matching per prendere l'id del blocco precedente al blocco corrente
    {ID_head,_,_,_} = Head,
    IdsBlocksInCatena = lists:map(fun(A)->{X,_,_,_}=A, X end,Catena),
    io:format("~p: id prev amico    ~p: our id ~n",[ID_prev_block, ID_head]),
    case lists:member(ID_prev_block, IdsBlocksInCatena) of
        true -> % caso in cui il precedente del blocco(con relativi eventuali blocchi annessi) che dovrei aggiungere si trova nella mia lista
            case ID_prev_block =:= ID_head of
                true -> 
                    % caso buono: il blocco nuovo viene aggiunto in testa
                    CatenaB ++ Catena;
                false -> 
                    % devo trovare il punto in cui agganciare il nuovo blocco
                    I = indexBlock(ID_prev_block, IdsBlocksInCatena, 1),
                    CatenaConfrTail = lists:sublist(Catena,I-1),
                    CatenaConfrHead = Catena -- CatenaConfrTail,
                    case length(CatenaB) > length(CatenaConfrTail) of
                        true ->
                            CatenaB ++ CatenaConfrHead;
                        false ->
                            case length(CatenaB) =:= length(CatenaConfrTail) of
                                true ->
                                    case rand:uniform(2) of 
                                        1 -> Catena;
                                        2 -> CatenaB ++ CatenaConfrHead
                                    end;
                                false ->
                                    Catena
                            end
                    end
            end;
        false -> 
            % caso in cui il precedente del blocco(con relativi eventuali blocchi annessi) 
            % che dovrei aggiungere NON si trova nella mia lista
            case ID_prev_block =/= none of
                true -> 
                    RandomFriend = lists:nth(rand:uniform(length(FriendsList)),FriendsList),
                    RandomFriend ! {get_previous, self(), make_ref(), ID_prev_block},
                    receive 
                        {previousLocal, none} when length(FriendsListToAsk) =:= 0 -> 
                           % Quando la lista di amici a cui posso chiedere diventa vuota, 
                           % richiedo nuovamente a tutti gli amici
                            verifyCatena(Catena, FriendsList, FriendsList, CatenaB);
                        {previousLocal, none} when length(FriendsListToAsk) =/= 0 -> 
                            verifyCatena(Catena, FriendsList, FriendsList -- [RandomFriend], CatenaB);
                        {previousLocal, B_prev} ->
                            verifyCatena(Catena, FriendsList, FriendsList, CatenaB++B_prev)
                        
                    after 5000 -> 
                        verifyCatena(Catena, FriendsList, FriendsList -- [RandomFriend], CatenaB)
                    end;
                false -> 
                    Catena
            end
    end.

sendAll(B, Catena, FriendsList) ->
    case lists:member(B,Catena) of
        true -> not_send_block; % gossiping della nuova transaction
        false -> [ X ! {update, B} || X <- FriendsList]
    end.
