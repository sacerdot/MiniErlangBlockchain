-module(chain_tools).
-export([buildInitChain/1,reconstructing/4,searchBlock/2,checkBlock/1]).
% -import (proof_of_work , [solve/1,check/2]).
-import (utils , [sendMessage/2,sleep/1]).
-define(TIMEOUT_TO_ASK, 5000).


%%%%%%%%% * PUBLIC *%%%%%%%%
buildInitChain(PidRoot) ->
    PidRoot ! {checkFriendsList, self()}, % chiedo la lista di amici a Root
    FList = receive 
        {myFriendsList, FriendsList} -> 
            FriendsList 
    end,
    % partendo dalla lista di amici chiedo una catena iniziale
    askChainFriends(FList).

reconstructing(PidRoot,Blocco, Chain,Sender) -> 
    {_,ID_Prev,ListT,_} = Blocco,

    %? Caso 0:
    %?     Chain è vuota. Inserisco il Blocco direttamente
    case length(Chain) =:= 0 of
        true -> 
            throw({done,[Blocco],ListT});
        false -> continue
    end,

    %? Caso 1:
    %?     Blocco va inserito in cima alla mia catena --> ottimo
    %?     ID_Prev di Blocco è la testa di Chain 
    [{ID_MyBlockHead, _, _,_} | _] = Chain,
    case ID_MyBlockHead =:= ID_Prev of
        true -> 
            throw({done,[Blocco] ++ Chain, ListT});
        false -> 
            continue
    end,

    case searchBlock(ID_Prev,Chain) of
        none ->
             %? Caso 3 (HARD):
            %?     Se ID_Prev di Blocco non fa parte della mia catena 
            %?     chiedo al Sender (ed eventualmente ai miei amici)
            %?     di invarmi il Blocco ID_Prev in modo
            %?     da capire il punto della biforcazione.
            %?     Trovato il punto di biforcazione (conosco l'ID_Prev)
            %?     confronto le catene (in caso di parità teniamo la nostra :) )
            PidRoot ! {checkFriendsList, self()},
            FList = receive
                {myFriendsList, FriendList } -> FriendList -- [Sender]
            end,
            searching([], Chain, Blocco, Sender, FList);
        _ -> 
            %? Caso 2:
            %?     ID_Prev di Blocco non è la testa ma un blocco più 
            %?     vecchio della catena --> lo scarto
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

checkBlock({_, ID_Prev,ListT,Solution}) -> 
    proof_of_work:check({ID_Prev,ListT},Solution);
% per evitare blocchi di una forma diversa
checkBlock(_) -> 
    false.

%%%%%%%% * PRIVATE *%%%%%%%%
% cerco il punto in comune tra otherChain e MyChain
% Blocco è il punto potenziale della biforcazione
% Quando trovo un blocco in comune cerco la posizione di quel Blocco 
% in MyChain e lo sommo alla OtherChain in modo da poter scegliere la catena più lunga
otherChainFinished({_,none,_,_},OtherChain,MyChain) -> 
    case length(MyChain) >= length(OtherChain) of
        true -> 
            throw(discarded);
        false ->
            % A parità di lughezze rimaniamo con la nostra catena
            NewT = lists:flatmap(fun(A)->{_,_,X,_}=A, X end,OtherChain),
            throw({done,OtherChain,NewT})
    end;
otherChainFinished(_,_,_) -> none.

checkIfContainsTrans(ChainCommon,OtherChain) ->
    TInCommonChain = lists:flatmap(fun(A)->{_,_,X,_}=A, X end,ChainCommon),
    TInOtherChain = lists:flatmap(fun(A)->{_,_,X,_}=A, X end,OtherChain),
    SetTInCommonChain = sets:from_list(TInCommonChain),
    SetTInOtherChain = sets:from_list(TInOtherChain),
    Intersection = sets:intersection(SetTInCommonChain,SetTInOtherChain),
    case sets:size(Intersection) of
        0 -> false;
        _ -> true
    end.
% se l'altra catena è maggiore della nostra e c'è almeno una T in otherChain 
% che è già nella parte comune allora scarto l'altra, altrimenti la accetto e aggiorno T mined
% se OtherChain è < stretto della mia accetto la mia
searching(OtherChain, MyChain, Blocco, Sender,FList) -> 

    % se Blocco ha come id_prev none mi fermo e decido qui cosa fare:
    % [Blocco] ++ OtherChain == MyChain random
    % altrimenti scarto la più corta
    otherChainFinished(Blocco,OtherChain ++ [Blocco], MyChain),

    {ID,_,_,_} = Blocco,
    case searchBlock(ID,MyChain) of
        none ->
            % chiedo il precedenti di Blocco a Sender
            {_,ID_Prev,_,_} = Blocco,
            % metto in cima alla lista Sender in modo da chiedere eventualmente a tutti 
            % ma prima chiedo a Sender
            PrevBlock = searchPrevious(ID_Prev,[Sender]++FList),
            % continuo la ricerca cercando PrevBlock
            searching(OtherChain ++ [Blocco], MyChain,PrevBlock,Sender,FList);
        B -> % B è il blocco in comune --> biforcazione trovata
            PositionCommonBlock = indexBlock(B,MyChain,1),
            ChainCommon = lists:nthtail(PositionCommonBlock-1, MyChain),
            LengthMyChainToConfr = PositionCommonBlock - 1,
            LengthOtherChainToConfr = length(OtherChain),

            case LengthOtherChainToConfr > LengthMyChainToConfr of
                true ->
                    case checkIfContainsTrans(ChainCommon,OtherChain) of 
                        true -> 
                            throw(discarded); % scarto il blocco e rimane la catena mia
                        false ->
                            otherChainAccepted(OtherChain,ChainCommon)
                    end;
                false-> 
                    % CI FIDIAMO DELLA NOSTRA CATENA ANCHE A PARITA' DI LUNGHEZZA OLTRE A QUANDO SIAMO MAGGIORI
                    throw(discarded) % scarto il blocco e rimane la catena mia
            end
    end.    

otherChainAccepted(OtherChain,ChainCommon) ->
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

% se non ho più amici allora non so più a chi chidere il prev
searchPrevious(_,[]) -> throw(discarded);
% Il primo a cui chiedo è colui che mi ha mandato il blocco
searchPrevious(ID_Prev,FList) -> 
    [Friend | _] = FList,
    Nonce = make_ref(),
    sendMessage(Friend, {get_previous,self(),Nonce,ID_Prev}),
    receive 
        % {previous,Nonce,none} -> 
        %     searchPrevious(ID_Prev,FList--[Friend]);               
        {previous,Nonce,Blocco} -> 
            case checkBlock(Blocco) of
                true -> 
                    Blocco;
                false -> 
                    searchPrevious(ID_Prev,FList--[Friend])
            end
    after 
        ?TIMEOUT_TO_ASK -> 
            searchPrevious(ID_Prev,FList--[Friend])
    end.

% costruisco la catena tenendo conto delle transazione all'interno dei blocchi
%! Friend -> amico corrente a cui sto chiedendo il blocco 
%! FListToAsk -> amici a cui posso chiedere se Friend non risponde o non ha il Blocco
%!              se Friend non risponde allora viene scelto un nuovo amico e va rimosso da questa lista
%!              se invece Friend riponde dicendo che ha il blocco allora FListToAsk = FList -- [Friend] 
%! FList -> (rimane invarianta) lista totale di amici, serve per ricominciare le eventuali richieste dei blocchi succ
getRestChain({Friend,FListToAsk,FList},Chain,TList,ID_Prev_Current) ->
    case ID_Prev_Current =:= none of
        % genero un Eccezione e mi fermo in quanto la catena che sto percorrendo è terminata
        true -> 
            throw({chain,{Chain,TList}});             
        false -> continue
    end,    

    % E' una chiusura quindi Nonce come non può essere utilizzato
    Ref = make_ref(),
    sendMessage(Friend, {get_previous, self(), Ref, ID_Prev_Current}),
    receive
        {previous, Ref, PrevBlock} ->
            {_,ID_blocco_prev,Transaction,_} = PrevBlock,
            % se Friend ce l'ha continuo a chiedere a lui
            getRestChain({Friend,FList --[Friend],FList},Chain ++ [PrevBlock],Transaction++TList,ID_blocco_prev)
    after ?TIMEOUT_TO_ASK -> 
        % se dopo N secondi non mi risp e posso chidere a qualcun altro chiedo altrimenti catena vuota
        case length(FListToAsk) =:= 0 of
                true -> throw(none);
                false -> 
                    % scelgo un friend possibile da FListToAsk e resetto la lista di amici a cui posso chiedere 
                    % rimuovendo l'amico scelto
                    AnotherFriend= lists:nth(rand:uniform(length(FListToAsk)),FListToAsk),
                    getRestChain({AnotherFriend,FList--[AnotherFriend],FList},Chain,TList,ID_Prev_Current)
        end
    end.

% chiede a Friend una catena entro un N secondi 
getChain(Friend,FListToAsk,FList) -> 
    Nonce = make_ref(),
    sendMessage( Friend , { get_head, self(), Nonce }),
    receive 
        {head, Nonce, none} -> 
            % se Friend non ha la testa chiedo ad una altro
            % ma se non ho più amici a cui chiedere allora catena vuota
            case length(FListToAsk) =:= 0 of
                true -> throw(none);
                false ->
                    AnotherFriend = lists:nth(rand:uniform(length(FListToAsk)),FListToAsk),
                    getChain(AnotherFriend,FListToAsk--[AnotherFriend],FList)
            end;
        {head, Nonce, Blocco} -> 
            % ho ricevuto la testa di una catena e continuo a chiedere il resto a Friend
            {_,ID_Prev,Transaction,_} = Blocco,
            getRestChain({Friend,FList--[Friend],FList},[Blocco],[Transaction],ID_Prev)
        after
            ?TIMEOUT_TO_ASK -> 
                % se dopo N secondi non mi risp e posso chidere a qualcun altro chiedo altrimenti catena vuota
                case length(FListToAsk) =:= 0 of
                        true -> throw(none);
                        false ->
                            AnotherFriend = lists:nth(rand:uniform(length(FListToAsk)),FListToAsk),
                            getChain(AnotherFriend,FListToAsk--[AnotherFriend],FList)
                end
    end.

askChainFriends(FList) ->
    case length(FList) =:= 0 of
        true -> {[],[]}; % se non ho amici allora catena è vuota e non ci sono transazioni già minate
        false -> 
            Friend = lists:nth(rand:uniform(length(FList)),FList),
            try 
                getChain(Friend,FList--[Friend],FList) 
            catch
                % se non so più a chi chiedere sollevo "none" e quindi catena vuota e nessuna TMined
                none -> {[],[]};
                % quando arrivo all'ultimo blocco della catena (idPrev =none ) restituisco
                {chain, ChainAndTransaction} -> ChainAndTransaction
            end
    end.



% !!!Appunti: Se scegliamo random in caso di catena di lunghezza uguale
% Se hanno la stessa dim
% case LengthOtherChainToConfr =:= LengthMyChainToConfr of
%     true -> 
%         case rand:uniform(2) of
%             1 -> 
%                 % io:format("~n[Blocco trovato] OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (MyChain è maggiore)~n",[OtherChain,MyChain]),           
%                 throw(discarded);
%             2 -> otherChainAccepted(OtherChain,ChainCommon)

%         end;
%     false ->
        % io:format("~n[Blocco trovato] OtherChain: ~p~nMyChain: ~p~nScelta: Scarto OtherChain (MyChain è maggiore)~n",[OtherChain,MyChain]),           
        % throw(discarded) % scarto il blocco e rimane la catena mia

% end
% !!!Appunti: Se riceviamo none come risposta ad una get_previuous
% {previous, Ref, none} -> 
%             % se Friend non ha il blocco richiesto chiedo ad un altro amico
%             % ma se non so più a chi chiedere allora restituisco {[],[]} caso raro
%             case length(FListToAsk) =:= 0 of
%                 true -> throw(none);
%                 false -> 
%                     AnotherFriend = lists:nth(rand:uniform(length(FListToAsk)),FListToAsk),
%                     getRestChain({AnotherFriend,FListToAsk--[AnotherFriend],FList},Chain,TList,ID_Prev_Current)
%                 end;