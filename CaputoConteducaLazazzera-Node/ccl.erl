-module(ccl).
-export([main/0,start/2, watchFriends/2,checkList/1]).


sleep(N) -> receive after N*1000 -> ok end.
% Node è il nodo da monitorare
% Main è il nodo che vuole essere avvisato della morte di Node
% il watcher ogni 5 secondi fa la richiesta di 'esistenza' a Node 
% e se dopo 2s non risponde comunichiamo che non è piu vivo
watch(Main,Node) ->
    %io:format("[~p]: Sono il watcher creato da ~p e guardo ~p~n",[self(),Main,Node]),
    sleep(5),
    Ref = make_ref(),
    Node ! {ping, self(), Ref},
    receive
        {pong, Ref} -> watch(Main,Node)
    after 2000 -> 
        %io:format("[~p]: Sto mandando a ~p che ~p è morto ~n",[self(),Main,Node]),
        Main ! {dead, Node}
    end.


loop(FriendsList, NameNode) -> 
    receive
        {checkList, CheckAct} ->
                CheckAct ! {myList, FriendsList},
                loop(FriendsList,NameNode);

        {print} -> 
            io:format("[~p, ~p]: ha questi amici: ~p~n",[self(),NameNode,FriendsList]),
            loop(FriendsList,NameNode); 
        % Mantenimento Topologia
        {ping, Mittente, Nonce} -> % sono vivo
            Mittente ! {pong, Nonce},
            loop(FriendsList,NameNode);    
            
        
        {get_friends, Mittente, Nonce} -> % qualcuno mi ha chiesto la lista di amici
            % FriendsList ++ Mittente,
            Mittente ! {friends, Nonce, FriendsList}, 
            % se Mittente non è nella FList lo aggiungo se ho meno di 3 amici
            case lists:member(Mittente, FriendsList) of
                    true -> loop(FriendsList,NameNode);
                    false -> 
                        case length(FriendsList) < 3 of 
                            true -> 
                                %usato questo Self per preservare il pid
                                Self = self(),
                                % metto un watch su Mittente
                                spawn(
                                    fun()-> 
                                        %io:format("BBB [~p] ha ~p ~n",[Self,Mittente]),
                                        watch(Self,Mittente) 
                                    end
                                ),
                                loop([Mittente | FriendsList],NameNode);
                            false -> 
                                loop(FriendsList,NameNode)
                        end
            end;
        {friends, _, Lista_di_amici} -> 
            % ho chiesto di mandarmi amici in seguito 
            % ad un messaggio di morte di un nodo amico oppure
            % sono ancora in fase di inizializzazione
            % dalla lista ricevuta rimuovo me stesso e gli eventuali
            % elementi che già avevo    
           
            ListTemp = ((Lista_di_amici -- [self()]) -- FriendsList),            
            case length(ListTemp) =:= 0 of
                true -> 
                    % non ho ricevuto amici "utili"
                    loop(FriendsList,NameNode);
                false -> 
                    NewItems = addNodes(length(FriendsList), ListTemp),
                    % TODO: per ogni i in NewItems metto un attore per fare check 
                    watchFriends(NewItems,self()),
                    NewList = NewItems ++ FriendsList,
                    % io:format("[~p, ~p]: ha come amici ~p~n",[self(),NameNode,NewList]),

                    loop(NewList,NameNode)
            end;
        {dead, Node} ->
            io:format("[~p, ~p]: ~p è morto. Richiedo un altro amico~n",[self(),NameNode,Node]),
            NewList =  FriendsList -- [Node],
            loop(NewList,NameNode)            
    end.


addNodes(LengthFList,ListTemp) ->
    case LengthFList + length(ListTemp) > 3 of
        true ->                        
            addNodes(LengthFList, (ListTemp -- [lists:nth(rand:uniform(length(ListTemp)),ListTemp)]));
        false -> 
            ListTemp
    end.


%%%%%%%%  behavior dell'attore che controlla gli amici di PID %%%%%%%%
checkList(Pid) -> 
    sleep(6),
    Pid ! {checkList,self()},
    receive
        {myList, List} -> 
            requireFriends(List,Pid),
            checkList(Pid)
    end.

requireFriends(FList,Mittente) -> 
        case length(FList) < 3 of
            true ->
                case length(FList) =:= 0  of
                    true -> 
                        %io:format("~p chiede al prof ~n",[Mittente]),
                        askProf(Mittente);
                    false ->
                        %io:format("~p chiede agli amici ~n",[Mittente]),
                        askToFriends(FList,Mittente)
                end;
            false -> ok
        end.
askProf(Mittente) ->
    global:send(teacher_node,{get_friends,Mittente,make_ref()}).    
askToFriends(FList, Mittente) ->  
    Ref = make_ref(),
    PidFriendReq = lists:nth(rand:uniform(length(FList)),FList), 
    % mando a PidFriendReq la richiesta di avere amici
    PidFriendReq ! {get_friends, Mittente, Ref}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




start(NameNode,FirstTime) -> 
    % attore delegato al check degli amici 
    case FirstTime of
        true -> 
            spawn_link(?MODULE,checkList,[self()]);
        false -> none
    end,   
    % si sblocca solo quando ricevo almeno
    receive
        {checkList, CheckAct} -> 
            CheckAct ! {myList, []},
            start(NameNode,false);           
        {friends, _, L}  -> 
            L1 = L -- [self()], % rimuovo me stesso
            case length(L1) =:= 0 of
                % se non trovo nessuno ricomincio da start e 
                % quindi mando una richiesta di amici a teacher node    
                true ->
                    start(NameNode,false);
                false -> 
                    % ne prendo solo 3 (o minore da L1)
                    FList = 
                        case length(L1) =< 3 of
                            true -> L1;
                            false -> lists:sublist(L1,3)
                        end,
                    io:format("[~p, ~p]: ha trovato ~p amici(~p)~n",[self(),NameNode,length(FList),FList]),
                    watchFriends(FList,self()),
                    loop(FList,NameNode)
        end
    end.

watchFriends(NewItems,Main) ->
    [spawn(fun()-> 
        %io:format("AAA [~p] ha ~p ~n",[Main,NewItems]),
        watch(Main,X) end) || X<-NewItems].


main() ->  
    TIME = 4,
    spawn(teacher_node,main,[]), % teacher_node
    sleep(2),% waiting prof
    
    N1 = spawn(?MODULE,start,["N1",true]),
    io:format("~p -> ~p~n",["N1",N1]),
    sleep(TIME),
    
    N2 = spawn(?MODULE,start,["N2",true]),
    io:format("~p -> ~p~n",["N2",N2]),
    sleep(TIME),

    N3 = spawn(?MODULE,start,["N3",true]),
    io:format("~p -> ~p~n",["N3",N3]),
    sleep(TIME),
    
    N4 = spawn(?MODULE,start,["N4",true]),
    io:format("~p -> ~p~n",["N4",N4]),
    sleep(TIME),
    
    N5 = spawn(?MODULE,start,["N5",true]),
    io:format("~p -> ~p~n",["N5",N5]).








