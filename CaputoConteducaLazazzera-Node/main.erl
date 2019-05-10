-module(main).
-import (check_act , [start_C_act/1]).
-import (transaction_act , [start_T_act/2]).
-import (block_act , [start_B_act/2]).
-import (utils , [sendMessage/2,sleep/1,watch/2,watchFriends/2]).
-export([main/0,start/1,easy/0,hard/0]).
-on_load(load_module_act/0).

%|------------INIT--------------------|
%| net_adm:ping('teacher_node@yyy').  |
%|net_adm:ping('teacher@anto-P553UJ').|
%| main:main().                       |
%|------------------------------------|


load_module_act() ->
    compile:file('actors/check_act.erl'), % attore che controlla la tipologia
    compile:file('actors/transaction_act'), % attore gestore delle transazioni
    compile:file('actors/block_act.erl'),
    compile:file('actors/block_gossiping_act.erl'), 
    compile:file('actors/chain_tools.erl'), 
    compile:file('actors/miner_act.erl'), 
    compile:file('actors/reconstruct_act.erl'), 
    compile:file('utils.erl'),
    % compile:file('../teacher_node.erl'),
    % compile:file('proof_of_work.erl'),
    ok.  

% 

main() -> 
    {Pid,Ref} = spawn_monitor(fun() -> start("CCL") end),
    receive
        {'DOWN',Ref,process,Pid,_} -> 
            io:format("CCL is dead :( Restarting...~n"), 
            % Diamo il tempo a teacher_node di aggiornare la lista dei vivi
            sleep(8),
            main()
    end.

start(NameNode) ->
    io:format("CCL is born :) ~n"), 
    Self = self(),
    % process_flag(trap_exit, true), % deve essere posto prima di fare le spawn
    PidC = spawn_link(fun() -> start_C_act(Self) end), % attore delegato al check degli amici 
    PidB = spawn_link(fun() -> start_B_act(NameNode,Self) end), % attore delegato alla gestione dei blocchi
    PidT = spawn_link(fun() -> start_T_act(Self,PidB) end), % attore delegato al gestione delle transazioni
    % spawn(fun() -> watchRoot(Self) end),
    loop([],NameNode, PidT, PidB, PidC, []).

%! ----- Behavior di Root Act ---------------- 
loop(FriendsList, NameNode,PidT,PidB,PidC,Nonces) -> 
    
    receive 
        %! DEBUG 
        {addNewNonce, Nonce} ->
            loop(FriendsList,NameNode,PidT,PidB,PidC,Nonces++[Nonce]);
        {removeNonce, Nonce} ->
            loop(FriendsList,NameNode,PidT,PidB,PidC,Nonces--[Nonce]);
        {checkFriendsList, CheckAct} ->
            CheckAct ! {myFriendsList, FriendsList},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);
        {printT} ->
            % lista transazioni arrivate
            PidT ! {printT},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);
        {printTM} ->
            % lista transazioni da minare
            PidB ! {printTM},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);  
        {print} -> 
            io:format("[~p, ~p]: ha questi amici: ~p~n",[self(),NameNode, FriendsList]),
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces); 
        {printC} ->
            PidB ! {printC},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);

        %!%%%%%% Mantenimento Topologia %%%%%%%
        {ping, Mittente, Nonce} -> % sono vivo
            Mittente ! {pong, Nonce},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);                
    
        {get_friends, Mittente, Nonce} -> % qualcuno mi ha chiesto la lista di amici
            Msg = {friends, Nonce, FriendsList},
            sendMessage(Mittente, Msg),
            % se Mittente non è nella FList lo aggiungo se ho meno di 3 amici
            case lists:member(Mittente, FriendsList) of
                    true -> loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);
                    false -> 
                        case length(FriendsList) < 3 of 
                            true -> 
                                Self = self(), % per preservare il Pid-Root
                                % metto un watch su Mittente
                                spawn(fun()-> watch(Self, Mittente) end),
                                % io:format("[~p, ~p] ha un nuovo amico: ~p (in seguito ad una get_friends)~n",[self(),NameNode,Mittente]),
                                loop([Mittente | FriendsList], NameNode, PidT, PidB, PidC, Nonces);
                            false -> % non ho bisogno di altri amici
                                loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces)
                        end
            end;
        {friends, Nonce, Lista_di_amici} -> 
            %se ho il Nonce elaboro il mess
            case lists:member(Nonce, Nonces) of 
                true ->
                    %io:format("~p stava in ~p ~n",[Nonce, Nonces]),
                    NewNonces = Nonces--[Nonce],
                    ListTemp = ((Lista_di_amici -- [self()]) -- FriendsList),            
                    case length(ListTemp) =:= 0 of
                        true -> % non ho ricevuto amici "utili"
                            loop(FriendsList, NameNode, PidT, PidB, PidC, NewNonces);
                        false -> 
                            % prendo solo n amici per arrivare a 3
                            NewFriends = addNodes(length(FriendsList), ListTemp),
                            % io:format("[~p, ~p] ha ~p nuovi amici: ~p~n",[self(),NameNode,length(NewFriends),NewFriends]),
                            watchFriends(NewFriends,self()), % per amico un watcher 
                            NewList = NewFriends ++ FriendsList,
                            loop(NewList, NameNode, PidT, PidB, PidC, NewNonces)
                    end;
                false -> 
                    %io:format("~p NON stava in ~p ~n",[Nonce, Nonces]),
                    loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces)
            end;
        {dead, Node} ->
            NewList =  FriendsList -- [Node],
            loop(NewList, NameNode, PidT, PidB, PidC, Nonces);
        %!%%%%%%%%%%% GESTIONE TRANSAZIONI %%%%%%%%%%%%%
        % Push locale di una transazione all'attore delegato che provvede a fare gossiping di Transazione
        {push, Transazione} -> 
            PidT ! {pushLocal, Transazione, FriendsList}, % si occupa di fare gossiping di T
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);

        %!%%%%%%%%%%% GESTIONE BLOCCHI %%%%%%%%%%%%%%%%%%
        % sender mi ha mandato questo blocco da rigirare
        {update, Sender, Blocco} ->
            PidB ! {updateLocal,Sender,Blocco},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);

        %!%%%%%%%%%%%% RICHIESTA INFO SULLA MIA CATENA %%%%%%%%%%%%%%
        {get_previous, Mittente, Nonce, Idblocco_precedente} ->
            % inoltro il messaggio a PidB
            PidB ! {get_previous, Mittente, Nonce, Idblocco_precedente},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces);
        {get_head, Mittente, Nonce} ->
            % inoltro il messaggio a PidB
            PidB ! {get_head, Mittente,Nonce},
            loop(FriendsList, NameNode, PidT, PidB, PidC, Nonces)
    end.


% aggiungo nodi fino a quando non raggiungo 3
addNodes(LengthFList,ListTemp) ->
    case LengthFList + length(ListTemp) > 3 of
        true ->                        
            addNodes(LengthFList, (ListTemp -- [lists:nth(rand:uniform(length(ListTemp)),ListTemp)]));
        false -> 
            ListTemp
    end.
% Versione con watcher per la morte di root
% watchRoot(PidRoot) -> 
%     sleep(10),
%     Ref = make_ref(),
%     PidRoot ! {ping, self(), Ref},
%     receive
%         {pong, Ref} -> watchRoot(PidRoot)
%     after 5000 -> 
%         main()
%     end.





% ------ TESTING CODE ----------
sendT(Dest,Payload) ->
    Dest ! {push, {make_ref(), Payload}},
    io:format("> Send ~p to ~p~n",[Payload,Dest]).
easy() ->
     io:format("Easy Version~n"),
    TIME = 2,
    TIME_TO_TRANS = 3,
    spawn(teacher_node,main,[]), % teacher_node
    sleep(1),
    
    N1 = spawn(?MODULE,start,["N1"]),
    io:format("~p -> ~p~n",["N1",N1]),
    sleep(TIME),
    
    N2 = spawn(?MODULE,start,["N2"]),
    io:format("~p -> ~p~n",["N2",N2]),
    sleep(TIME),

    sleep(5),
    io:format("Testing Transaction...~n"),
    sendT(N1,"Ho comprato il pane"),
    sleep(TIME_TO_TRANS),
    sendT(N1,"Ho comprato il pesce"),
    sleep(TIME_TO_TRANS),
    sendT(N2,"Ho comprato il latte"),
    sleep(TIME_TO_TRANS),
    sendT(N2,"Ho comprato il formaggio"),
    sleep(TIME_TO_TRANS*5),
    sendT(N2,"Ho comprato il pesto"),
    sendT(N2,"Ho comprato il succo"),

    N3 = spawn(?MODULE,start,["N3"]),
    io:format("~p -> ~p~n",["N3",N3]),
    
    sleep(20),
    io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n",["N2",N2]),
    io:format("~p -> ~p~n",["N3",N3]),
    N1 ! {printC},
    N2 ! {printC},
    N3 ! {printC},
    io:format("FIN QUI TUTTO OK ~n"),

    sleep(10),
    N4 = spawn(?MODULE,start,["N4"]),
    io:format("~p -> ~p~n",["N4",N4]),
    N5 = spawn(?MODULE,start,["N5"]),
    io:format("~p -> ~p~n",["N5",N5]),

    sendT(N4,"Ho comprato il vino"),
    sendT(N4,"Ho comprato il salame"),
    sendT(N4,"Ho comprato il pomodoro"),
    sendT(N5,"Ho comprato il prosciutto"),
    sleep(25),
    N1 ! {printC},
    N2 ! {printC},
    N3 ! {printC},
    N4 ! {printC},
    N5 ! {printC},
    io:format("FINITO PARTE2~n"),

    sleep(15),
    exit(N1,kill),
    sleep(10),
    N6 = spawn(?MODULE,start,["N6"]),
    io:format("~p -> ~p~n",["N6",N6]),
    sleep(15),
    N6 ! {printC},
    io:format("CONTROLLARE RICOSTRUZIONE N6~n"),
    sleep(10),
    N7 = spawn(?MODULE,start,["N7"]),
    io:format("~p -> ~p~n",["N7",N7]),
    sleep(10),
    sendT(N6,"Ho comprato il miele"),
    sendT(N6,"Ho comprato il caviale"),
    sleep(25),
    N2 ! {printC},
    N3 ! {printC},
    N4 ! {printC},
    N5 ! {printC},
    N6 ! {printC},
    N7 ! {printC},
    io:format("FINITO PARTE EASY~n"),

    sleep(10),
    exit(N5,kill),
    exit(N6,kill),
    sleep(20),
    sendT(N4,"Ho comprato il sushi"),
    sleep(30),
    N2 ! {printC},
    N3 ! {printC},
    N4 ! {printC},
    N7 ! {printC},
    io:format("FINITO~n").

hard() ->  

    io:format("Hard Version~n"),
    TIME = 2,
    TIME_TO_TRANS = 3,
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

    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    sleep(5),
    io:format("Testing Transaction...~n"),
    sendT(N1,"Ho comprato il pane"),
    sleep(TIME_TO_TRANS),
    sendT(N1,"Ho comprato il pesce"),
    sleep(TIME_TO_TRANS),
    sendT(N2,"Ho comprato il latte"),
    sleep(TIME_TO_TRANS),
    sendT(N3,"Ho comprato il formaggio"),
    sleep(TIME_TO_TRANS),
    sendT(N2,"Ho comprato il pesto"),
    sleep(TIME_TO_TRANS),
    sendT(N2,"Ho comprato il succo"),


    io:format("End Transaction Send ~n"),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    sleep(20),
    io:format("New Actors ~n"),
    N5 = spawn(?MODULE,start,["N5"]),
    io:format("~p -> ~p~n",["N5",N5]),
    io:format("~n~nASPETTO CHE N5 COSTRUISCA LA SUA CATENA~n~n"),
    sleep(50),
    sleep(TIME_TO_TRANS),
    sendT(N1,"Ho comprato il vino"),
 
    io:format("~n!!!! Controllare che n5 non ha il vino~n"),

    io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n",["N2",N2]),
    io:format("~p -> ~p~n",["N3",N3]),
    io:format("~p -> ~p~n",["N4",N4]),
    io:format("~p -> ~p~n",["N5",N5]),
    N1 ! {printC},
    N2 ! {printC},
    N3 ! {printC},    
    N4 ! {printC},    
    N5 ! {printC},
    sleep(50),

    sendT(N5,"Ho comprato la pizza"),
    io:format("Controllare che tutti hanno la pizza e il vino ma n5 ha solo pizza~n"),
    io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n",["N2",N2]),
    io:format("~p -> ~p~n",["N3",N3]),
    io:format("~p -> ~p~n",["N4",N4]),
    io:format("~p -> ~p~n",["N5",N5]),
    N1 ! {printC},
    N2 ! {printC},
    N3 ! {printC},    
    N4 ! {printC},    
    N5 ! {printC},
    sleep(40),

    exit(N1, kill),
    sleep(20),

    sendT(N5,"Ho comprato la coppa"),
    io:format("Controllare che tutti hanno tutto~n"),
    
    % io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n",["N2",N2]),
    io:format("~p -> ~p~n",["N3",N3]),
    io:format("~p -> ~p~n",["N4",N4]),
    io:format("~p -> ~p~n",["N5",N5]),
    % N1 ! {printC},
    N2 ! {printC},
    N3 ! {printC},    
    N4 ! {printC},    
    N5 ! {printC},


    sleep(50),
    io:format("Ultima stampa"),

    % io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n",["N2",N2]),
    io:format("~p -> ~p~n",["N3",N3]),
    io:format("~p -> ~p~n",["N4",N4]),
    io:format("~p -> ~p~n",["N5",N5]),
    % N1 ! {printC},
    N2 ! {printC},
    N3 ! {printC},    
    N4 ! {printC},    
    N5 ! {printC},

    io:format("Finito").

