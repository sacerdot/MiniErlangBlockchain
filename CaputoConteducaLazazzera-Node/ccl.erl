-module(ccl).
-import (check_act , [checkList/1]).
-import (transaction_act , [start_T_act/2]).
-import (block_act , [start_B_act/1]).


-export([main/0,start/1]).

-on_load(load_module_act/0).

load_module_act() ->
    compile:file('teacher_node.erl'), 
    compile:file('actors/check_act.erl'), % attore che controlla la tipologia
    compile:file('actors/transaction_act'), % attore gestore delle transazioni
    compile:file('actors/block_act.erl'), 
    ok.  


sleep(N) -> receive after N*1000 -> ok end.
% Node è il nodo da monitorare
% Main è il nodo che vuole essere avvisato della morte di Node
% il watcher ogni 5s fa ping a Node 
% e se dopo 2s non risponde allora non è piu vivo
watch(Main,Node) ->
    sleep(5),
    Ref = make_ref(),
    Node ! {ping, self(), Ref},
    receive
        {pong, Ref} -> watch(Main,Node)
    after 2000 -> 
        Main ! {dead, Node}
    end.


loop(FriendsList, NameNode,PidT,PidB) -> 
    receive
        {checkList, CheckAct} ->
            CheckAct ! {myList, FriendsList},
            loop(FriendsList,NameNode,PidT,PidB);
        {printT} ->
            PidT ! {printT},
            loop(FriendsList, NameNode,PidT,PidB);
        {print} -> 
            io:format("[~p, ~p]: ha questi amici: ~p~n",[self(),NameNode,FriendsList]),
            loop(FriendsList,NameNode,PidT,PidB); 
        {get_chain} ->
                PidB ! {get_chain},
                loop(FriendsList,NameNode,PidT,PidB);

        %%%%%%% Mantenimento Topologia %%%%%%%
        {ping, Mittente, Nonce} -> % sono vivo
            Mittente ! {pong, Nonce},
            loop(FriendsList,NameNode,PidT,PidB);                
    
        {get_friends, Mittente, Nonce} -> % qualcuno mi ha chiesto la lista di amici
            Mittente ! {friends, Nonce, FriendsList}, 
            % se Mittente non è nella FList lo aggiungo se ho meno di 3 amici
            case lists:member(Mittente, FriendsList) of
                    true -> loop(FriendsList,NameNode,PidT, PidB);
                    false -> 
                        case length(FriendsList) < 3 of 
                            true -> 
                                Self = self(), % per preservare il Pid-Root
                                % metto un watch su Mittente
                                spawn(fun()-> watch(Self,Mittente) end),
                                % io:format("[~p, ~p] ha un nuovo amico: ~p (in seguito ad una get_friends)~n",[self(),NameNode,Mittente]),
                                loop([Mittente | FriendsList],NameNode,PidT,PidB);
                            false -> % non ho bisogno di altri amici
                                loop(FriendsList,NameNode,PidT,PidB)
                        end
            end;
        {friends, _, Lista_di_amici} -> 
            ListTemp = ((Lista_di_amici -- [self()]) -- FriendsList),            
            case length(ListTemp) =:= 0 of
                true -> % non ho ricevuto amici "utili"
                    loop(FriendsList,NameNode,PidT,PidB);
                false -> 
                    % prendo solo n amici per arrivare a 3
                    NewFriends = addNodes(length(FriendsList), ListTemp),
                    % io:format("[~p, ~p] ha ~p nuovi amici: ~p~n",[self(),NameNode,length(NewFriends),NewFriends]),
                    watchFriends(NewFriends,self()), % per amico un watcher 
                    NewList = NewFriends ++ FriendsList,
                    loop(NewList,NameNode,PidT,PidB)
            end;
        {dead, Node} ->
            % io:format("[~p, ~p]: ~p è morto~n",[self(),NameNode,Node]),
            NewList =  FriendsList -- [Node],
            loop(NewList,NameNode,PidT,PidB);
        %%%%%%%%%%%% GESTIONE TRANSAZIONI %%%%%%%%%%%%%
        % Push locale di una transazione all'attore delegato che provvede a fare gossiping di Transazione
        {push, Transazione} -> 
            % io:format("[~p, ~p]: Ho ricevuto la transazione ~p~n",[self(),NameNode,Transazione]),
            PidT ! {pushLocal, Transazione, FriendsList},
            loop(FriendsList,NameNode,PidT,PidB);
        
    

        %%%%%%%%%%%% GESTIONE BLOCCHI %%%%%%%%%%%%%%%%%%
        % Update locale di un blocco all'attore delegato
        {update, Blocco} ->
            % io:format("[~p, ~p]: Ho ricevuto l'update del blocco ~p~n",[self(),NameNode,Blocco]),
            PidB ! {updateLocal, Blocco, FriendsList},
            loop(FriendsList,NameNode,PidT,PidB);
        {updateMyBlock, Blocco} ->
            % io:format("[~p, ~p]: Ho creato il blocco ~p~n",[self(),NameNode,Blocco]),
            PidB ! {updateMyBlockLocal, Blocco, FriendsList},
            loop(FriendsList,NameNode,PidT,PidB);
        
        % B_Act ha fatto una richiesta che viene riceveuta dall'attore root che 
        % manderà la risposta a B_Act 
        {previous, _, Blocco} ->
            PidB ! {previousLocal,Blocco},
            loop(FriendsList,NameNode,PidT,PidB);
        {get_previous, Mittente, Nonce, Idblocco_precedente} ->
            PidB ! {get_previousLocal, Mittente, Nonce, Idblocco_precedente},
            % TODO: B_Act manda a Mittente il blocco_procedente
            loop(FriendsList,NameNode,PidT,PidB);
        {get_head,Mittente, Nonce} ->
            PidB ! {get_headLocal, Mittente,Nonce},
            loop(FriendsList,NameNode,PidT,PidB);
        {'EXIT',_,_} -> 
            % io:format("[~p, ~p]: ~p è morto con causa ~p~n",[self(),NameNode,Pid,Reason]),
            loop(FriendsList,NameNode,PidT,PidB)
    end.


% aggiungo nodi fino a quando non raggiungo 3
addNodes(LengthFList,ListTemp) ->
    % todo: selezione random di amici. Ne prendo |LengthFList - Len(ListTem)|
    case LengthFList + length(ListTemp) > 3 of
        true ->                        
            addNodes(LengthFList, (ListTemp -- [lists:nth(rand:uniform(length(ListTemp)),ListTemp)]));
        false -> 
            ListTemp
    end.


start(NameNode) -> 
    Self = self(),
    process_flag(trap_exit, true), % deve essere posto prima di fare le spawn
    spawn_link(fun() -> checkList(Self) end), % attore delegato al check degli amici 
    PidB = spawn_link(fun() -> start_B_act(Self) end), % attore delegato alla gestione dei blocchi
    PidT = spawn_link(fun() -> start_T_act(Self,PidB) end), % attore delegato al gestione delle transazioni
    loop([],NameNode, PidT, PidB).
    
watchFriends(NewItems,Main) -> [spawn(fun()-> watch(Main,X) end) || X<-NewItems].

main() ->  
    TIME = 4,
    spawn(teacher_node,main,[]), % teacher_node
    sleep(1),
    
    N1 = spawn(?MODULE,start,["N1"]),
    io:format("~p -> ~p~n",["N1",N1]),
    sleep(TIME),
    
    N2 = spawn(?MODULE,start,["N2"]),
    io:format("~p -> ~p~n",["N2",N2]),
    sleep(TIME),

    N3 = spawn(?MODULE,start,["N3"]),
    io:format("~p -> ~p~n",["N3",N3]),
    sleep(TIME),
    
    N4 = spawn(?MODULE,start,["N4"]),
    io:format("~p -> ~p~n",["N4",N4]),
    sleep(TIME),
    
    N5 = spawn(?MODULE,start,["N5"]),
    io:format("~p -> ~p~n",["N5",N5]),

    io:format("Testing Transaction...~n"),
    sleep(5),
    io:format("Send Transaction~n",[]),
    Payload1 = {"Ho comprato il pane"},
    N1 ! {push, {make_ref(), Payload1}},

    sleep(3),
    io:format("Send Transaction~n",[]),
    Payload2 = {"Ho comprato il pesce"},
    N1 ! {push, {make_ref(), Payload2}},
    
    sleep(3),
    io:format("Send Transaction~n",[]),
    Payload3 = {"Ho comprato il latte"},
    N2 ! {push, {make_ref(), Payload3}},

    sleep(3),
    io:format("Send Transaction~n",[]),
    Payload4 = {"Ho comprato la carne"},
    N3 ! {push, {make_ref(), Payload4}},
    
    sleep(3),
    io:format("Send Transaction~n",[]),
    Payload5 = {"Ho comprato il pesto"},
    N4 ! {push, {make_ref(), Payload5}},
    
    sleep(3),
    io:format("Send Transaction~n",[]),
    Payload6 = {"Ho comprato il succo"},
    N4 ! {push, {make_ref(), Payload6}}.
