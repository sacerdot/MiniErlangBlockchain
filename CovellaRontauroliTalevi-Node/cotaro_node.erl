-module(cotaro_node).
-export([initializeNode/0, test_nodes/0]).

%definisce il numero di amici che ogni nodo deve cercare di mantenere
-define(NumberOfFriendsRequired, 3).

%definisce la struttura dello stato di un nodo
% - numberOfNotEnoughFriendRequest: 
%       il nodo si memorizza il numero di richieste di nuovi amici fatte che non gli hanno permesso di tornare al
%       numero di amici richiesto. Quando questo valore raggiunge il numero di amici che abbiamo passiamo a chiedere
%       al nodo professore perchè nessuno dei nostri amici è riuscito ad aiutarci.
% - chain: 
%       è una tupla composta da {chain, IdHead, Dictionary}. Il primo campo è un atomo, il secondo è l'ID del blocco che si trova in testa alla catena
%       (ovvero l'ultimo che è stato inserito) ed infine l'ultimo campo è il dizionario che utilizziamo per memorizzare tutti i blocchi che compongono 
%       la nostra catena. Il dizionario usa come chiavi gli ID dei blocchi e come valori gli oggetti "blocco" corrispondenti.
% - transactionPool: 
%       è la lista delle nuove transazioni che riceviamo e che possiamo inserire in un nuovo blocco dopo averlo minato
% - currentChainLength: 
%       lunghezza corrente della catena del nodo
% - activeMiner: 
%       flag che indica se è attivo correntemente un attore che sta minando un blocco

-record(state, {numberOfNotEnoughFriendRequest, chain, transactionPool, currentChainLength, activeMiner, updateInAnalysis}).

spawnNode() ->
    spawn(fun() -> initializeNode() end).

% utilizzata per monitorare il nodo in maniera tale da ripristinarlo in casa per un qualche errore vada "down"
nodeMonitor(NodePID) ->
    MonitorRef = erlang:monitor(process, NodePID),
    receive
        {'DOWN', MonitorRef, process, NodePID, _} ->
            erlang:demonitor(MonitorRef),
            NewNodePID = spawnNode(),
            io:format("Dead node ~p; restarted as ~p \n",[NodePID, NewNodePID]),
            nodeMonitor(NewNodePID)
    end.

% utilizzata per lanciare un nuovo nodo e il monitor per esso
launchNode() -> 
    NodePID = spawnNode(),
    spawn(fun() -> nodeMonitor(NodePID) end),
    NodePID.

%utilizzata per inizializzare un nuovo nodo, in particolare:
% - initializza lo stato del nuovo nodo
% - richiede degli amici al nodo professore
% - avvia il behaviour del nodo
initializeNode() ->
    State = #state{
        numberOfNotEnoughFriendRequest = 0,
        chain =  {chain, none, dict:new()},
        transactionPool = [],
        currentChainLength = 0,
        activeMiner = false,
        updateInAnalysis = []
    },
    process_flag(trap_exit, true),
    askFriendsToTeacher(),
    loop([], State).

loop(MyFriends, State) ->
    receive
        {ping, Mittente, Nonce} ->
            %io:format("~p has received ping request, sending pong~n", [self()]),
            Mittente ! {pong, Nonce},
            loop(MyFriends, State);

        {dead, Node} ->
            %un amico è morto quindi il nodo aggiorna la sua lista di amici e ne cerca uno nuovo
            MyFriendsUpdated = MyFriends -- [Node],
            io:format("For ~p the node ~p is dead, friend list updated = ~w~n",[self(), Node, MyFriendsUpdated]),
            askFriends(MyFriendsUpdated),
            loop(MyFriendsUpdated, State);

        {timer_askFriendsToTeacher} ->
            %controlliamo se nel frattempo è successo qualcosa che ci ha fatto raggiungere il numero necessario di amicizie,
            %se non è così chiediamo al professore
            case length(MyFriends) < ?NumberOfFriendsRequired of
                true -> askFriendsToTeacher();
                false -> nothingToDo
            end,
            loop(MyFriends, State);

        {friends, Nonce, Friends} ->
            %gestirà solo il messaggio di risposta del teacher alla get_friends (quello con esattamente il Nonce memorizzato nel dizionario di processo)
            TeacherNonce = get(friendsTeacherNonce),
            if
                TeacherNonce =:= Nonce ->
                    %per gestire l'arrivo di una lista di possibili amici mi invio internamente il messaggio privato apposito
                    self() ! {friendsInternalMessage, Friends},
                    erase(friendsTeacherNonce);
                true ->
                    %else
                    io:format("~p receive a friends list with a unknown nonce, so it erases the message~n", [self()])
            end,
            loop(MyFriends, State);

        {friendsInternalMessage, OtherFriends} ->
            %una lista di nuovi possibili amici è stata ricevuta
            %estriamo dallo stato il numero di richieste di amici che non sono state sufficienti
            PreviousNumberOfNotEnoughFriendRequest = State#state.numberOfNotEnoughFriendRequest,
            %selezioniamo solo i nuovi possibili amici rimuovendo noi stessi e nodi che già conosciamo
            NewFriends = OtherFriends -- (MyFriends ++ [self()]),
            case NewFriends of
                [] ->
                    %non riusciamo ad aggiungere amici
                    ActualNumberOfNotEnoughFriendRequest = PreviousNumberOfNotEnoughFriendRequest + 1,
                    NewState = State#state{numberOfNotEnoughFriendRequest = ActualNumberOfNotEnoughFriendRequest},
                    if
                        ActualNumberOfNotEnoughFriendRequest < length(MyFriends) ->
                            %non riusciamo ad aggiungere amici ma abbiamo ancora amici a cui chiedere
                            askFriends(MyFriends);
                        ActualNumberOfNotEnoughFriendRequest == length(MyFriends) ->
                            %non riusciamo ad aggiungere amici e abbiamo già chiesto a tutti i nostri amici quindi chiediamo al nodo professore
                            askFriendsToTeacher();
                        ActualNumberOfNotEnoughFriendRequest > length(MyFriends) ->
                            %non riusciamo ad aggiungere amici e abbiamo già chiesto anche al nodo professore quindi aspettiamo un pò e poi richiediamo a lui
                            launchTimerToAskFriendToTeacher()
                    end;
                _ ->
                    %abbiamo dei nuovi potenziali amici da aggiungere
                    NewState = State,
                    launchFriendsAdder(MyFriends, NewFriends)
            end,
            loop(MyFriends, NewState);

        {friendsAdded, MyNewListOfFriends} ->
            io:format("~p updated friend list = ~w ~n", [self(), MyNewListOfFriends]),
            %estriamo dallo stato il numero di richieste di amici che non sono state sufficienti
            PreviousNumberOfNotEnoughFriendRequest = State#state.numberOfNotEnoughFriendRequest,
            %se anche con i nuovi amici non riusciamo a raggiungere il numero necessario chiediamo nuovi amici,
            %altrimenti azzeriamo il numero di chiamate che non sono state sufficienti perchè abbiamo raggiunto il numero di amici richiesto
            case length(MyNewListOfFriends) < ?NumberOfFriendsRequired of
                true ->
                    ActualNumberOfNotEnoughFriendRequest = PreviousNumberOfNotEnoughFriendRequest + 1,
                    NewState = State#state{numberOfNotEnoughFriendRequest = ActualNumberOfNotEnoughFriendRequest},
                    if
                        ActualNumberOfNotEnoughFriendRequest < length(MyNewListOfFriends) ->
                            askFriends(MyNewListOfFriends);
                        ActualNumberOfNotEnoughFriendRequest == length(MyNewListOfFriends) ->
                            askFriendsToTeacher();
                        ActualNumberOfNotEnoughFriendRequest > length(MyNewListOfFriends) ->
                            launchTimerToAskFriendToTeacher()
                    end;
                false ->
                    NewState = State#state{numberOfNotEnoughFriendRequest = 0 }
            end,
            loop(MyNewListOfFriends, NewState);

        {get_friends, Sender, Nonce} ->
            %ci arriva una richiesta di trasmissione dei nostri amici, quindi inviamo la nostra lista al mittente
            Sender ! {friends, Nonce, MyFriends},
            loop(MyFriends, State);

        {push, Transaction} ->
            launchTransactionIsInTheChain(Transaction, State#state.chain),
            loop(MyFriends, State);

        {transactionNotInTheChain, Transaction} ->
            %Transaction = {IDtransazione, Payload}
            {IdTransaction, _} = Transaction,
            %controlliamo se la transazione in questione è già presente nella nostra lista delle nuove transazioni che abbiamo già ricevuto
            TransactionFoundInTheList = searchTransactionInTheList(IdTransaction, State#state.transactionPool),
            case TransactionFoundInTheList of
                true ->
                    %se la transazione è già nella catena, non facciamo nulla
                    nothingToDo,
                    loop(MyFriends, State);
                false ->
                    %se non conoscevamo la transazione, la inseriamo nella lista della transazione da provare ad inserire nei prossimi blocchi
                    %e poi la ritrasmettiamo ai nostri amici
                    %io:format("~p receive a new transaction with ID ~w~n", [self(), IdTransaction]),
                    NewTransactionPool = State#state.transactionPool ++ [Transaction],
                    launchSenderToAllFriend(MyFriends, {push, Transaction}),
                    NewState = State#state{
                        transactionPool = NewTransactionPool,
                        activeMiner = checkMining(State#state.activeMiner, NewTransactionPool, State#state.chain)
                    },       
                    loop(MyFriends, NewState)
            end;

        {get_head, Sender, Nonce} ->
            launchGetHeadActor(Sender, Nonce, State#state.chain),
			loop(MyFriends, State);

		{get_previous, Sender, Nonce, IDPreviousBlock} ->
            launchPreviousActor(Sender, Nonce, State#state.chain, IDPreviousBlock),
			loop(MyFriends, State);

		{mine_successful, NewBlock} ->
			launchAcceptBlockActor(self(), State#state.chain, State#state.transactionPool, State#state.currentChainLength, NewBlock),
			loop(MyFriends, State);

		{mine_update, NewBlock, NewChain, _IDHead, NewTransactionPool, NewChainLength} ->
            {chain, CurrentHead, _} = State#state.chain,
            {_, NewPreviousID, _, _} = NewBlock,
            % se testa della catena corrente è uguale all'id del blocco precedente di quello minato
            % la catena non è cambiata e può essere aggiornata con il nuovo blocco;
            % altrimenti è cambiata e il blocco deve essere stato scartato
            case CurrentHead =:= NewPreviousID of
                false->
                    %printChainAndList(NewChain, [self()]++["mindis"], self()),
                    NewState = State#state{
                        activeMiner = checkMining(false, State#state.transactionPool, State#state.chain)
                    };
                true ->
                    %printChainAndList(NewChain, [self()]++["minres"], self()),
                    NewState = State#state{
                        chain = NewChain, 
                        transactionPool = NewTransactionPool, 
                        currentChainLength = NewChainLength,
                        activeMiner = checkMining(false, NewTransactionPool, NewChain)
                    },
                    [F ! {update, self(), NewBlock} || F <- MyFriends]
            end,
            loop(MyFriends, NewState);

        {update, Sender, Block} ->
            {BlockID, _, _, _} = Block,
            UpdateInAnalysis = State#state.updateInAnalysis,
            case lists:member(BlockID, UpdateInAnalysis) of 
                true -> 
                    % l'update per il blocco ricevuto è già in corso e non deve esserne instanziata 
                    % una ulteriore gestione per esso
                    nothingToDo,
                    loop(MyFriends, State);
                false -> 
                    % non vi è nessuna update in corso per il blocco ricevuto, viene quindi instanziato 
                    % un sotto-attore per gestirla
                    NewState = State#state{updateInAnalysis = UpdateInAnalysis ++ [BlockID]},
                    launchUpdateActor(self(), Sender, MyFriends, Block, State#state.chain),
                    loop(MyFriends, NewState)
            end;

        {update_response, UpdateResponse} ->
            CurrentChainLength = State#state.currentChainLength,
            CurrentTransactionPool = State#state.transactionPool,
            UpdateInAnalysis = State#state.updateInAnalysis,
            case UpdateResponse of
                {new_chain, NewChain, NewChainLength, TransactionToRemove, UpdateBlockID} ->
                    % ricevuta la catena risultante dall'operazione di update in seguito alla ricezione
                    % di un blocco; nel caso in cui questa sia più lunga di quella corrente, quest'ultima
                    % viene sostituita con la prima
                    NewState = State#state{updateInAnalysis = UpdateInAnalysis -- [UpdateBlockID]},
                    case NewChainLength > CurrentChainLength of
                        true ->
                            StateWithNewChain = NewState#state{
                                chain = NewChain,
                                transactionPool = CurrentTransactionPool -- TransactionToRemove,
                                currentChainLength = NewChainLength
                            },
                            %printChainAndList(NewChain, [self()]++["upres"], self()),
                            loop(MyFriends, StateWithNewChain);
                        false ->
                            loop(MyFriends, NewState)
                    end;
                {discarded_chain, UpdateBlockID} ->
                    % catena risultante in seguito alla ricezione di un blocco da non considerare:
                    % il blocco è scorretto o è già contenuto nella catena corrente, oppure 
                    % partendo dal nuovo blocco non si è riusciti a ricostruire la catena risultante
                    % completa
                    NewState = State#state{updateInAnalysis = UpdateInAnalysis -- [UpdateBlockID]}, 
                    loop(MyFriends, NewState)
            end;

        {print_chain} ->
            printChain(State#state.chain, self()),
            loop(MyFriends, State);

        {'EXIT', ActorDeadPID, Reason} ->
            %abbiamo linkato tutti gli attori che abbiamo spawniamo, se questi terminano normalmente non facciamo nulla,
            %altrimenti li ri-lanciamo con le informazioni memorizzate nel dizionario di processo
            case Reason of
                normal -> nothingToDo;
				killed -> nothingToDo; %abbiamo appositamente killato l'attore
                _ ->
                    ProcessData = get(ActorDeadPID),
                    case ProcessData of
                        launchTimerToAskFriendToTeacher ->
                            launchTimerToAskFriendToTeacher();
                        {watcher, PID} ->
                            launchWatcher(PID, self());
                        {friendsAsker} ->
                            askFriends(MyFriends);
                        {friendsAdder, NewFriends} ->
                            launchFriendsAdder(MyFriends, NewFriends--MyFriends);
                        {messageSender, FriendList, Message} ->
                            launchSenderToAllFriend(FriendList, Message);
                        {transcationIsInTheChainController, Transaction} ->
                            launchTransactionIsInTheChain(Transaction, State#state.chain);
						% Uso A, B, C, D per evitare di usare variabili già bindate
						{send_previous_actor, A, B, C, D} ->
                            launchPreviousActor(A, B, C, D);
						{send_head_actor, E, F, G} ->
                            launchGetHeadActor(E, F, G);
						{miner_actor} ->
                            NewState = State#state{activeMiner = false},
                            erase(ActorDeadPID),
                            loop(MyFriends, NewState);
						{launch_acceptBlock, U_PID, U_NewBlock} ->
							launchAcceptBlockActor(U_PID, State#state.chain, State#state.transactionPool, State#state.currentChainLength, U_NewBlock);
                        {launch_update, Father, Sender, NewBlock} ->
                            launchUpdateActor(Father, Sender, MyFriends, NewBlock, State#state.chain);
                        _ ->
                            %se non so gestire la exit mi suicido
                            exit(Reason)
                    end
            end,
            %dove aver fatto ripartire l'attore crashato eliminiamo la entry collegata al vecchio attore ormai morto dal dizionario di processo
            erase(ActorDeadPID),
            loop(MyFriends, State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%GENERAL FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(N) -> receive after N*1000 -> ok end.

sendMessageWithDisturbance(DestinationPID, Message) ->
    %ogni volta che inviate un messaggio, ci deve essere una probabilità su 10 di non inviarlo e una su 10 di inviarne due copie
    case rand:uniform(10) of
        0 ->
            %non invio il messaggio
            %io:format("Not send message (~w) to ~p for disturbance ~n", [Message, DestinationPID]),
            nothingToDo;
        1 ->
            %invio il messaggio 2 volte
            %io:format("Send message (~w) to ~p 2 times for disturbance ~n", [Message, DestinationPID]),
            DestinationPID ! Message, DestinationPID ! Message;
        _ ->
            %altrimenti invio il messaggio correttamente una sola volta
            DestinationPID ! Message
    end.
sendToAllFriend([], _) -> nothingToDo;
sendToAllFriend(FriendList, Message) ->
    lists:foreach(fun(FriendPID) -> sendMessageWithDisturbance(FriendPID, Message) end, FriendList).
launchSenderToAllFriend(FriendList, Message) ->
    MessageSenderPID = spawn_link(fun () -> sendToAllFriend(FriendList, Message) end),
    %inseriamo le informazioni sull'attore message sender lanciato nel dizionario di processo così che
    %se questo muore per qualche ragione, venga rilanciato
    put(MessageSenderPID, {messageSender, FriendList, Message}).

getHead([]) -> none;
getHead(CurrentChain) ->
	{chain, IdHead, CurrentDictChain} = CurrentChain,
	case dict:find(IdHead, CurrentDictChain) of
		{ok, Head} -> Head;
		error -> nothingToDo %abbiamo già IdHead come testa, l'errore non si verificherà mai
	end.

getBlockFromDictChain(CurrentDictChain, BlockID) ->
    case dict:find(BlockID, CurrentDictChain) of
		{ok, Block} -> Block;
		error -> none
	end.

getBlockFromChain(CurrentChain, BlockID) ->
    {chain, _, CurrentDictChain} = CurrentChain,
    getBlockFromDictChain(CurrentDictChain, BlockID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TOPOLOGY FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

watch(Node, Main) ->
    sleep(10),
    Ref = make_ref(),
    Node ! {ping, self(), Ref},
    receive
        {pong, Ref} -> watch(Node, Main)
    after 2000 -> Main ! {dead, Node}
    end.
launchWatcher(PID, LoopPID) ->
    WatcherPID = spawn_link(fun () -> watch(PID, LoopPID) end),
    %inseriamo le informazioni sull'attore watcher lanciato nel dizionario di processo così che
    %se questo muore per qualche ragione, venga rilanciato
    put(WatcherPID, {watcher, PID}).

launchTimerToAskFriendToTeacher() ->
    Creator = self(),
    TimerPID =  spawn_link(
        fun () ->
            %io:format("~p launch timer to ask friends to teacher~n", [Creator]),
            sleep(2),
            Creator ! {timer_askFriendsToTeacher}
        end),
    %inseriamo le informazioni sull'attore timer lanciato nel dizionario di processo così che
    %se questo muore per qualche ragione, venga rilanciato
    put(TimerPID, launchTimerToAskFriendToTeacher).

friendsAsker(MyFriends, LoopPID) ->
    %selezioniamo casualmente uno dei nostri amici e gli inviamo una richiesta per ottenere la sua lista di amici
    SelectedFriend = lists:nth(rand:uniform(length(MyFriends)), MyFriends),
    %io:format("~p require friends to ~p~n", [LoopPID, SelectedFriend]),
    Nonce = make_ref(),
    SelectedFriend ! {get_friends, self(), Nonce},
    receive
        {friends, Nonce, Friends} -> LoopPID ! {friendsInternalMessage, Friends}
    after 10000 -> LoopPID ! {friendsInternalMessage, []}
    end.
askFriendsToTeacher() ->
    askFriends([]).
askFriends([]) ->
    %io:format("~p require friends to teacher node~n", [self()]),
    Nonce = make_ref(),
    %salvo il nonce nel dizionario di processo per dare la possibilità all'attore principale quando riceve un messaggio friends
    %di controllare che sia esattamente quello atteso dal teacher
    put(friendsTeacherNonce, Nonce),
    global:send(teacher_node, {get_friends, self(), Nonce});
askFriends(MyFriends) ->
    Self = self(),
    FriendsAskerPID = spawn_link(fun () -> friendsAsker(MyFriends, Self) end),
    %inseriamo le informazioni sull'attore friends asker lanciato nel dizionario di processo così che
    %se questo muore per qualche ragione, venga rilanciato
    put(FriendsAskerPID, {friendsAsker}).

addFriends(MyFriends, [], LoopPID) ->
    LoopPID ! {friendsAdded, MyFriends};
addFriends(MyFriends, OtherFriends, LoopPID) ->
    %aggiungiamo amici finchè non raggiungiamo il numero necessario o finiscono i potenziali nuovi amici
    %la scelta di un nuovo amico, tra i potenziali a disposizione è casuale e su questo viene lanciato un watcher
    %per controllare che rimanga in vita
    case length(MyFriends) < ?NumberOfFriendsRequired of
        true ->
            NewFriend = lists:nth(rand:uniform(length(OtherFriends)), OtherFriends),
            launchWatcher(NewFriend, LoopPID),
            addFriends( MyFriends ++ [NewFriend], OtherFriends -- [NewFriend], LoopPID);
        false ->
            LoopPID ! {friendsAdded, MyFriends}
    end.
launchFriendsAdder(MyFriends, OtherFriends) ->
    Self = self(),
    FriendsAdderPID = spawn_link(fun () -> addFriends(MyFriends, OtherFriends, Self) end),
    %inseriamo le informazioni sull'attore friends adder lanciato nel dizionario di processo così che
    %se questo muore per qualche ragione, venga rilanciato
    put(FriendsAdderPID, {friendsAdder, OtherFriends}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PUSH FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%la searchTransactionInTheChainAux solleva un'eccezione 'found' nel momento in cui trova in un blocco della catena la transazione cercata
searchTransactionInTheChainAux(_, none, _) -> false ;
searchTransactionInTheChainAux(IdTransaction, IdBlock, Chain) ->
    Block = getBlockFromChain(Chain,IdBlock),
    {_, IDPreviousBlock, CurrentTransactionList, _} = Block,
    %predicato che ritorna true se la transazione in input è quella cercata (stesso ID), false altrimenti
    case searchTransactionInTheList(IdTransaction, CurrentTransactionList) of
        true -> throw(found);
        false -> searchTransactionInTheChainAux(IdTransaction, IDPreviousBlock, Chain)
    end.
searchTransactionInTheChain(IdTransaction, Chain) ->
    try
        {chain, IdHeadBlock, _} = Chain,
        searchTransactionInTheChainAux(IdTransaction, IdHeadBlock, Chain)
    catch
        found -> true
    end.
searchTransactionInTheList(IdTransaction, TransactionList) ->
    %predicato che ritorna true se la transazione in input è quella cercata (stesso ID), false altrimenti
    lists:member(
        IdTransaction,
        lists:map(fun (Y) -> {Y_ID, _} = Y, Y_ID end, TransactionList)
    ).

transactionIsInTheChain(Transaction, Chain, LoopPID) ->
    %Transaction = {IDtransazione, Payload}
    {IdTransaction, _} = Transaction,
    %controlliamo se la transazione in questione è già presente nella nostra catena
    TransactionFoundInTheChain = searchTransactionInTheChain(IdTransaction, Chain),
    case TransactionFoundInTheChain of
        true ->
            %se la transazione è già nella catena, non facciamo nulla
            nothingToDo;
        false ->
            %se non la conoscevamo
            LoopPID ! {transactionNotInTheChain, Transaction}
    end.
launchTransactionIsInTheChain(Transaction, Chain) ->
    Self = self(),
    TransactionControllerPID = spawn_link(fun () -> transactionIsInTheChain(Transaction, Chain, Self) end),
    %inseriamo le informazioni sull'attore transaction controller lanciato nel dizionario di processo così che
    %se questo muore per qualche ragione, venga rilanciato
    put(TransactionControllerPID, {transcationIsInTheChainController, Transaction}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%HEAD/PREVIOUS FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendHeadActor(Sender, Nonce, CurrentChain) ->
	Sender ! {head, Nonce, getHead(CurrentChain)}.

launchGetHeadActor(Sender, Nonce, CurrentChain) ->
    SendHeadPID = spawn_link(fun() -> sendHeadActor(Sender, Nonce, CurrentChain) end),
	put(SendHeadPID, {send_head_actor, Sender, Nonce, CurrentChain}).

%% se non abbiamo il blocco allora non inviamo il messaggio.
%% in caso contrario, inviamo le informazioni del blocco richiesto
sendPreviousActor(Sender, Nonce, CurrentChain, IdBlock) ->
	SearchedBlock = getBlockFromChain(CurrentChain, IdBlock),
	case SearchedBlock of
		none -> nothingToDo;
		_ -> Sender ! {previous, Nonce, SearchedBlock}
	end.

launchPreviousActor(Sender, Nonce, CurrentChain, IdBlock) ->
    PreviousActorPID = spawn_link(fun () -> sendPreviousActor(Sender, Nonce, CurrentChain, IdBlock) end),
	put(PreviousActorPID, {send_previous_actor, Sender, Nonce, CurrentChain, IdBlock}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%MINE FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mine(ID_previousBlock, TransactionList) ->
    BlockID = make_ref(),
    Solution = proof_of_work:solve({ID_previousBlock, TransactionList}),
    {BlockID, ID_previousBlock, TransactionList, Solution}.

%% registra se stesso con l'atomo miner_process, in modo da essere killato in caso di updateBlock sulle stesse transazioni su cui stiamo minando.
%% Nel caso in cui il mining sia avvenuto con successo, manda a PID un messaggio contenente il nuovo stato (quindi la nuova catena).
miner(IdPreviousBlock, PID, Transactions) ->
    TransactionsToMine = lists:sublist(Transactions, 10),
    NewBlock = mine(IdPreviousBlock, TransactionsToMine),
    PID ! {mine_successful, NewBlock}.

launchMinerActor(IdPreviousBlock, PID, Transactions) ->
	MinerActorPID = spawn_link(fun () -> miner(IdPreviousBlock, PID, Transactions) end),
    put(MinerActorPID, {miner_actor}).

% nel caso in cui non sia attivo il mining e la pool di transazioni contenga almeno una transazione viene avviato il mining
checkMining(ActiveMiner, Transactions, Chain) ->
    case (not ActiveMiner) and (length(Transactions) > 0) of
        true ->
            {chain, IDHead, _} = Chain,
            launchMinerActor(IDHead, self(), Transactions),
            true;
        false ->
            ActiveMiner
    end.

%% modifico lo stato aggiornando la catena, la sua lunghezza e le transazioni ancora da minare; restituisco il nuovo stato
acceptBlockActor(Father, Chain, TransactionPool, CurrentChainLength, NewBlock) ->
	{IDHead, _IDPreviousBlock, Transactions, _Solution} = NewBlock,
    {chain, _, DictChain} = Chain,
    NewDictChain = dict:store(IDHead, NewBlock, DictChain),
	NewChain = {chain, IDHead, NewDictChain},
	NewTransactionPool = TransactionPool -- Transactions,
	NewChainLength = CurrentChainLength + 1,
	Father ! {mine_update, NewBlock, NewChain, IDHead, NewTransactionPool, NewChainLength}.
launchAcceptBlockActor(PID, Chain, TransactionPool, CurrentChainLength, NewBlock) ->
    AcceptBlockActorPID = spawn_link(fun() -> acceptBlockActor(PID, Chain, TransactionPool, CurrentChainLength, NewBlock) end),
	put(AcceptBlockActorPID, {launch_acceptBlock, PID, NewBlock}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%UPDATE FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% restituisce il dizionario (catena) che va da BlockID a EndingBlockID (non incluso)
getPartialDictChainFromBlockToBlock(OriginalDictChain, BlockID, EndingBlockID, PartialDictChain) ->
    case BlockID =:= EndingBlockID of
        true -> PartialDictChain;
        false ->
            case getBlockFromDictChain(OriginalDictChain, BlockID) of
                none -> none;
                Block ->
                    NewPartialDictChain = dict:store(BlockID, Block, PartialDictChain),
                    {_, PreviousBlockID, _, _} = Block,
                    getPartialDictChainFromBlockToBlock(OriginalDictChain, PreviousBlockID, EndingBlockID, NewPartialDictChain)
            end
    end.

% scandisce la catena (dizionario) per ottenere la lista di transazioni
scanChainForTransactionList(DictChain, BlockID, List) ->
    case dict:find(BlockID, DictChain) of
        error -> List;
        {ok, Block} ->
            {_, PreviousBlockID, TransactionList, _} = Block,
            NewTransactionList = List ++ TransactionList,
            scanChainForTransactionList(DictChain, PreviousBlockID, NewTransactionList)
    end.

% ritorna la lista complessiva di transazioni per la catena considerata
getChainTransactions(Chain) ->
    {chain, HeadID, DictChain} = Chain,
    scanChainForTransactionList(DictChain, HeadID, []).

% ritorna la lunghezza della catena (dizionario) considerato
getDictChainLength(DictChain) ->
    length(dict:fetch_keys(DictChain)).

% restituisce la catena risultante dal blocco ricevuto in fase di update, la lunghezza per questa e il pool di 
% transazioni da rimuovere nel caso in cui essa diventi la nuova catena;
% 'NewBlockID' e 'NewDictChain' mantengono durante le chiamate ricorsive l'ID del blocco originale ricevuto in
% seguito all'update e il dizionario (che viene aggiornato durante le chiamate) con il quale si costruisce la
% catena derivata da quest'ultimo
getResultingChainFromUpdate(SenderPID, Friends, CurrentChain, Block, NewBlockID, NewDictChain) ->
    {chain, _, CurrentDictChain} = CurrentChain,
    {BlockID, PreviousBlockID, _, _} = Block,
    NewUpdatedDictChain = dict:store(BlockID, Block, NewDictChain),
    case PreviousBlockID =:= none of
        true ->
            % non vi siano nodi comuni tra la catena ricostruita dal blocco ricevuto e quella corrente;
            % viene restituita la catena per il blocco ricevuto in seguito all'update
            NewChain = {chain, NewBlockID, NewUpdatedDictChain},
            {update_response, {new_chain, NewChain, getDictChainLength(NewUpdatedDictChain), getChainTransactions(NewChain), NewBlockID}};
        false ->
            case dict:find(PreviousBlockID, CurrentDictChain) of
                {ok, _} ->
                    % vi è un nodo comune tra la catena derivata dal blocco ricevuto e quella corrente;
                    % queste dovrebbero avere una sotto-catena comune che può essere sfruttata per completare
                    % la ricostruzione della prima senza richiedere i blocchi precedenti ad altri nodi;
                    % viene restituita la catena per il blocco ricevuto in seguito all'update
                    PartialNewDictDelta = NewUpdatedDictChain,
                    ChainsCommonSubChain = getPartialDictChainFromBlockToBlock(CurrentDictChain, PreviousBlockID, none, dict:new()),
                    NewDictChainMerged = dict:merge(
                            fun(_K, V1, _V2) -> V1 end,
                            PartialNewDictDelta,
                            ChainsCommonSubChain
                        ),
                    NewChain = {
                        chain,
                        NewBlockID,
                        NewDictChainMerged
                    },
                    {update_response, {
                        new_chain,
                        NewChain,
                        getDictChainLength(NewDictChainMerged),
                        getChainTransactions(NewChain),
                        NewBlockID
                    }};
                error ->
                    % non si è arrivati alla conclusione della ricostruzione della catena o ad un blocco comune con 
                    % quella corrente; è quindi necessario richiedere il blocco precedente sconosciuto ad altri nodi
                    % (mittente dell'update e amici) al fine di ricostruire la catena
                    Nonce = make_ref(),
                    [A ! {get_previous, self(), Nonce, PreviousBlockID} || A <- [SenderPID] ++ Friends],
                    receive
                        {previous, Nonce, PreviousBlock} ->
                            % gestione della ricezione del messaggio contenente il blocco precedente richiesto
                            getResultingChainFromUpdate(
                                SenderPID,
                                Friends,
                                CurrentChain,
                                PreviousBlock,
                                NewBlockID,
                                NewUpdatedDictChain
                            )
                    after 2000 ->
                        % se messaggio previous non arriva entro timeout (nodi a cui si è chiesto potrebbero non essere 
                        % riusciti a rispondere o proprio non avere il blocco 'previous' richiesto);
                        % viene ritornato l'atomo 'discarded_chain' che il tentativo di ricostruzione non è andato a buon
                        % fine
                        {update_response, {discarded_chain, NewBlockID}}
                    end
        end
    end.

% metodo di gestione dell'update:
% nel caso in cui il blocco ricevuto sia corretto e non sia contenuto nella catena corrente, lo si diffonde ai propri
% amici e si cerca di ricostruire la catena per questo attraverso la 'getResultingChainFromUpdate'
updateHandling(NewBlockSender, Friends, NewBlock, CurrentChain) ->
    {NewBlockID, NewPreviousBlockID, TransactionList, Solution} = NewBlock,
    {chain, _, CurrentDictChain} = CurrentChain,
    case
        proof_of_work:check({NewPreviousBlockID, TransactionList}, Solution) and (dict:find(NewBlockID, CurrentDictChain) =:= error)
    of
        false ->
            % blocco ricevuto da update scorretto o già presente all'interno della catena corrente
            {update_response, {discarded_chain, NewBlockID}};
        true ->
            % diffusione del blocco ricevuto agli amici
            [F ! {update, NewBlockSender, NewBlock} || F <- Friends],
            % tentativo di ricostruzione della catena per il blocco ricevuto
            getResultingChainFromUpdate(NewBlockSender, Friends, CurrentChain, NewBlock, NewBlockID, dict:new())
    end.

% gestione in seguito ad arrivo dell'update e ritorno del risultato al padre
handleUpdate(FatherPID, NewBlockSenderPID, Friends, NewBlock, CurrentChain) ->
    FatherPID ! updateHandling(NewBlockSenderPID, Friends, NewBlock, CurrentChain).

% lancia un sotto-attore per gestire un'update ricevuta
launchUpdateActor(FatherPID, Sender, Friends, NewBlock, CurrentChain) ->
    UpdateActorPID = spawn_link(fun() -> handleUpdate(FatherPID, Sender, Friends, NewBlock, CurrentChain) end),
    put(UpdateActorPID, {launch_update, FatherPID, Sender, NewBlock}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PRINT FUNCTIONS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xToString(X) ->
    lists:flatten(io_lib:format("~p", [X])).

% stampa il blocco
newChainString(OldChainString, Block) ->
    BlockString = xToString(Block),
    OldChainString ++ "\n        -> " ++ BlockString.

% scandisce la catena per la stampa, nella chiamata della funzione BlockID contiene l'ID del blocco di partenza
printDictChain(BlockID, DictChain, ActorPID, StringToPrint) ->
    case (BlockID =:= none) or (getDictChainLength(DictChain) =:= 0) of
        true ->
            io:format("\n\nChain of ~p: ~s\n", [ActorPID, StringToPrint]);
        false ->
            case dict:find(BlockID, DictChain) of
                {ok, Block} -> 
                    {_, PreviousBlockID, _, _} = Block,
                    printDictChain(PreviousBlockID, DictChain, ActorPID, newChainString(StringToPrint, Block));
                error ->
                    io:format("\n\nSomething wrong in chain of actor ~p: ~s\n\n", [ActorPID, StringToPrint])
	        end
    end.

% stampa la catena
printChain(Chain, ActorPID) ->
    {chain, Head, DictChain} = Chain,
    spawn(fun()-> printDictChain(Head, DictChain, ActorPID, "") end).

% stampa la catena e il pid del nodo
printChainAndList(Chain, StartStringsList, ActorPID) ->
    {chain, Head, DictChain} = Chain,
    spawn(fun()-> printDictChain(Head, DictChain, ActorPID, xToString(StartStringsList)) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TEST%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_nodes() ->
    _T = spawn(teacher_node, main, []),
	sleep(1),
    NodeList0 = launchNNode(8, []),
    sleep(2),
    spawn(fun () -> sendTransactions(NodeList0, 0) end),

    sleep(rand:uniform(5)),
    PIDNodeToKill1 = lists:nth(rand:uniform(length(NodeList0)), NodeList0),
    exit(PIDNodeToKill1, manually_kill),
    NodeList1 = NodeList0 -- [PIDNodeToKill1],

    sleep(rand:uniform(10)),    
    PIDNodeToKill2 = lists:nth(rand:uniform(length(NodeList1)), NodeList1),
    exit(PIDNodeToKill2, manually_kill),
    NodeList2 = NodeList1 -- [PIDNodeToKill2],
    
    sleep(rand:uniform(20)),
    PIDNodeToKill3 = lists:nth(rand:uniform(length(NodeList2)), NodeList2),
    exit(PIDNodeToKill3, manually_kill),
    NodeList = NodeList2 -- [PIDNodeToKill3],

    %sleep(rand:uniform(10)),
    %PIDNodeToKill4 = lists:nth(rand:uniform(length(NodeList)), NodeList),
    %WrongList = {1, 2},
    %io:format("Trying to kill node ~p with block ~p \n",[PIDNodeToKill4, WrongList]),
    %PIDNodeToKill4 ! {friendsAdded, WrongList},

    %sleep(rand:uniform(10)),
    %PIDNodeToKill5 = lists:nth(rand:uniform(length(NodeList)), NodeList),
    %WrongTransaction = [1,2],
    %io:format("Trying to kill node ~p with block ~p \n",[PIDNodeToKill5, WrongTransaction]),
    %PIDNodeToKill5 ! {transactionNotInTheChain, WrongTransaction},

    %sleep(rand:uniform(10)),
    %Nonce2 = make_ref(),
    %global:send(teacher_node, {get_friends, self(), Nonce2}),
    %receive
    %    {friends, Nonce2, SecondNodeList} ->
    %        io:format("Teacher list: ~p \n",[SecondNodeList])
    %end,

    sleep(60),
    [N ! {print_chain} || N <- NodeList],
    sleep(15),
    test_launched.

launchNNode(0, NodeList) ->
    NodeList;
launchNNode(N, NodeList) ->
    launchNNode(N-1, NodeList ++ [launchNode()]).

sendTransactions(_, 20) ->
    nothingToDo;
sendTransactions(NodeList, I) ->
    lists:nth(rand:uniform(length(NodeList)), NodeList) ! {push, {make_ref(), list_to_atom("Transazione" ++ integer_to_list(I))}},
    receive after rand:uniform(50)*10 -> ok end,
    sendTransactions(NodeList, I+1).
