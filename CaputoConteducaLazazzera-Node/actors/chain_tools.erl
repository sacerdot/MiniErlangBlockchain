-module(chain_tools).
-export([buildInitChain/1,reconstructing/4,searchBlock/2,checkBlock/1,validityBlock/2]).
-import (proof_of_work , [solve/1,check/2]).
-import (utils , [sendMessage/2,sleep/1]).
-define(TIMEOUT_TO_ASK, 5000).


%%%%%%%%% * PUBLIC *%%%%%%%%
buildInitChain(PidRoot) ->
    % io:format("[~p] Sto costruendo una catena iniziale...~n",[PidRoot]),
    PidRoot ! {checkFriendsList, self()}, % chiedo la lista di amici a Root
    FList = receive 
        {myFriendsList, FriendsList} -> 
            % io:format("[~p] HO QUESTI AMICI : ~p~n",[PidRoot,FriendsList]),
            FriendsList 
    end,
    % partendo dalla lista di amici chiedo una catena iniziale
    askChainFriends(FList).

reconstructing(PidRoot,Blocco, Chain,Sender) -> 
    {_,ID_Prev,ListT,_} = Blocco,
    %? Caso 0: Chain Empty 
    case length(Chain) =:= 0 of
        true -> 
            % io:format("~n ! Caso0: ok ~n"),
            throw({done,[Blocco],ListT});
        false -> continue
    end,

    %? Caso 1: Testa di Chain punto di attacco 
    [{ID_MyBlockHead, _, _,_} | _] = Chain,
    case ID_MyBlockHead =:= ID_Prev of
        true -> 
            % io:format("~nCaso1: ok~n~n"),
            throw({done,[Blocco] ++ Chain, ListT});
        false -> 
            % io:format("~nQUI ci sono allora vado in caso 2 o 3~n"),
            continue
    end,

    case searchBlock(ID_Prev,Chain) of
        none ->
            %? Caso 3: Chain non contiene ID_Prev
            % io:format("~nCaso3: ok~n~n"),
            PidRoot ! {checkFriendsList, self()},
            FList = receive
                {myFriendsList, FriendList } -> FriendList -- [Sender]
            end,
            searching([], Chain, Blocco, Sender, FList);
        _ -> 
            % io:format("~nCaso2: ok~n~n"),
            %? Caso 2: ID_Prev fa parte di Chain e quindi Scarto Blocco in quanto vecchio
            throw(discarded)
    end.

   


% ritorna il blocco con ID = ID_Search 
% se non esiste -> none
searchBlock(ID_Search,Chain) ->
    case length(Chain) =:= 0 of
        true -> none;
        false ->
            [H|T] = Chain,
            {ID_Current,_,_,_} = H,
            case ID_Current =:= ID_Search of
                true -> H;
                false -> searchBlock(ID_Search,T)
            end
    end.

% se le TinBlock sono contenute in Mined o in Mining rifiuto il blocco
% ma salvo le T che no ho minato in ToMine
validityBlock({ToMine,TMined},{_,_,TinBlock,_}) -> 
    % quali sono le T non minate in Blocco arrivato?
    NotMined = TinBlock -- TMined,    
    if 
        length(TinBlock) > length(NotMined) ->
            % allora ho gia minato qualche T in Blocco arrivato
            % perciò rifiuto il Blocco        
            NewToMine = (ToMine -- NotMined) ++ NotMined, % per evitare eventuali duplicati
            % TMined non viene aggiornata in quanto il blocco non viene accettato
            {NewToMine, TMined, false};
        length(TinBlock) =:= length(NotMined) ->
            % nel blocco ci sono T che non ho minato ma non posso minarle in quanto
            % presenti nel blocco arrivato
            % aggiorno lista di T da minare e già minate
            NewToMine = ToMine -- TinBlock,
            NewTMined = TMined ++ TinBlock,
            {NewToMine, NewTMined, true};
        true ->
            % mai nella vita, però...
            {ToMine, TMined, true}
end.


checkBlock({_, ID_Prev,ListT,Solution}) -> 
        check({ID_Prev,ListT},Solution).

%%%%%%%% * PRIVATE *%%%%%%%%

% cerco il punto in comune tra otherChain e MyChain
% Blocco è il punto potenziale della biforcazione
% Quando trovo un blocco in comune cerco la posizione di quel Blocco 
% in MyChain e lo sommo alla OtherChain in modo da poter scegliere la catena più lunga
otherChainFinished({_,none,_,_},OtherChain,MyChain) -> 
    case length(MyChain) > length(OtherChain) of
        true -> 
            % io:format("~n[Ultimo blocco] OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (MyChain è maggiore)~n",[OtherChain,MyChain]),
            throw(discarded);
        false ->
            NewT = lists:flatmap(fun(A)->{_,_,X,_}=A, X end,OtherChain),
            case length(MyChain) =:= length(OtherChain) of
                    true -> 
                        case rand:uniform(2) of
                            1 -> 
                                % io:format("~n[Ultimo blocco]OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (Random Stessa lunghezza)~n",[OtherChain,MyChain]),                        
                                throw(discarded);
                            2 -> 
                                % io:format("~n[Ultimo blocco]OtherChain: ~p~nMyChain: ~p~nScelta: Scarto MyChain (Random Stessa lunghezza)~n",[OtherChain,MyChain]),                        
                                throw({done,OtherChain,NewT})
                        end;
                    false ->
                        % la tua catena è maggiore
                        % io:format("~n[Ultimo blocco]OtherChain: ~p~nMyChain: ~p~nScelta: Scarto MyChain (OtherChain è maggiore)~n",[OtherChain,MyChain]),                        
                        throw({done,OtherChain,NewT})

                end
    end;
otherChainFinished(_,_,_) -> none.

searching(OtherChain, MyChain, Blocco, Sender,FList) -> 

    % se Blocco ha come id_prev none mi fermo e decido qui cosa fare:
    % [Blocco] ++ OtherChain == MyChain random
    % altrimenti scarto la più corta
    otherChainFinished(Blocco,OtherChain ++ [Blocco], MyChain),

    {ID,_,_,_} = Blocco,
    case searchBlock(ID,MyChain) of
        none ->
            % chiedo il precedenti di Blocco a Sender
            PrevBlock = searchPrevious(Blocco,Sender,FList),
            % continuo la ricerca cercando PrevBlock
            searching(OtherChain ++ [Blocco], MyChain,PrevBlock,Sender,FList);
        B -> % B è il blocco in comune --> biforcazione trovata
     
            PositionCommonBlock = indexBlock(B,MyChain,1),
            LengthMyChainToConfr = PositionCommonBlock - 1,
            LengthOtherChainToConfr = length(OtherChain),

            case LengthOtherChainToConfr > LengthMyChainToConfr of
                true ->
                    otherChainAccepted(MyChain,OtherChain,PositionCommonBlock);
                false-> 
                    % Se hanno la stessa dim
                    case LengthOtherChainToConfr =:= LengthMyChainToConfr of
                        true -> 
                            case rand:uniform(2) of
                                1 -> 
                                    % io:format("~n[Blocco trovato] OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (MyChain è maggiore)~n",[OtherChain,MyChain]),           
                                    throw(discarded);
                                2 -> otherChainAccepted(MyChain,OtherChain,PositionCommonBlock)
                            end;
                        false ->
                            % io:format("~n[Blocco trovato] OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (MyChain è maggiore)~n",[OtherChain,MyChain]),           
                            throw(discarded) % scarto il blocco e rimane la catena mia
                    end
            end
    end.    

otherChainAccepted(MyChain,OtherChain,PositionCommonBlock) ->
    % io:format("~n[Blocco Trovato] OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (MyChain è maggiore)~n",[OtherChain,MyChain]),
    ChainCommon = lists:nthtail(PositionCommonBlock,MyChain),
    % tutte le transizione nella parte che sto inserendo
    T_in_Other_Chain = lists:flatmap(fun(A)->{_,_,X,_}=A, X end,OtherChain),
    NewChain = OtherChain ++ ChainCommon,
    throw({done,NewChain, T_in_Other_Chain}).
        

% Dato il Blocco e la Catena restituisce la posizione di quel Blocco
% Blocco = Head --> 1
% Blocco = Tail --> length(Catena)
indexBlock(B,[H|T], Index) ->
    case B =:= H of
        true -> Index;
        false -> indexBlock(B,T,Index+1)
    end.

% Dato il blocco ritorna il blocco precedente
searchPrevious({_,ID_Prev,_,_},Sender,FList) ->
    %! Bloccante max 5 secondi
    Nonce = make_ref(),
    sendMessage(Sender, {get_previous,self(),ID_Prev}),
    receive 
        {previous,Nonce,Blocco} -> 
            case checkBlock(Blocco) of
                true -> Blocco;
                false -> throw(discarded)
            end
    after 
        % se non mi risponde scarto il blocco ricevuto
        ?TIMEOUT_TO_ASK -> 
            searchPreviousToFriends(ID_Prev,FList)
    end.

% se non ho più amici allora non so più a chi chidere il prev
searchPreviousToFriends(_,[]) -> throw(discarded);
searchPreviousToFriends(ID_Prev,FList) -> 
    [Friend | _] = FList,
    Nonce = make_ref(),
    sendMessage(Friend, {get_previous,self(),ID_Prev}),
    receive 
        {previous,Nonce,Blocco} -> 
            case checkBlock(Blocco) of
                true -> Blocco;
                false -> throw(discarded)
            end
    after 
        % se non mi risponde scarto il blocco ricevuto
        ?TIMEOUT_TO_ASK -> 
            searchPreviousToFriends(ID_Prev,FList--[Friend])
    end.

% costruisco la catena tenendo conto delle transazione all'interno dei blocchi
getRestChain(Friend,Chain,TList,ID_Prev_Current) ->
    case ID_Prev_Current =:= none of
        % genero un Eccezione e mi fermo in quanto la catena che sto percorrendo è terminata
        true -> 
            throw({chain,{Chain,TList}});             
        false -> continue
    end,    

    % E' una chiusura quindi Nonce come non può essere utilizzato
    Ref = make_ref(),
    sendMessage(Friend, {get_previous, self(), Ref, ID_Prev_Current}),
    % io:format("[~p] Chiedo il previous da ~p~n",[self(),Friend]),
    receive
        {previous, Ref, PrevBlock} ->
            % io:format("[~p] Arrivato il previous da ~p~n",[self(),Friend]),
            % ho ricevuto il precedente
            {_,ID_blocco_prev,Transaction,_} = PrevBlock,
            getRestChain(Friend,Chain ++ [PrevBlock],Transaction++TList,ID_blocco_prev)
    after ?TIMEOUT_TO_ASK -> throw(none)
    end.

% chiede a Friend una catena entro un N secondi 
getChain(Friend) -> 

    Nonce = make_ref(),
    sendMessage( Friend , { get_head, self(), Nonce }),
    % io:format("[~p] Chiedo la testa a ~p~n",[self(),Friend]),
    receive 
        {head, Nonce, Blocco} -> 
            % io:format("[~p] Arrivata la testa da ~p~n",[self(),Friend]),
            % ho ricevuto la testa di una catena e continuo a chiedere il resto a Friend
            {_,ID_Prev,Transaction,_} = Blocco,
            getRestChain(Friend,[Blocco],[Transaction],ID_Prev)
        after ?TIMEOUT_TO_ASK -> throw(none)
    end.

askChainFriends(FList) ->
    case length(FList) =:= 0 of
        true -> {[],[]}; % se non ho amici allora catena è vuota e non ci sono transazioni già minate
        false -> 
            Friend = lists:nth(rand:uniform(length(FList)),FList),
            try 
                getChain(Friend) 
            catch
                % se la catena è vuota oppure 
                % Friend non ha risposto entro il timeout
                none -> askChainFriends(FList -- [Friend]); % chiedo ad un altro amico
                % appena viene trovata una catena 
                {chain, ChainAndTransaction} -> ChainAndTransaction
            end
    end.


