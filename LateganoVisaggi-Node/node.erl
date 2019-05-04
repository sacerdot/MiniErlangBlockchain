% author (@Antonio Lategano)
% author (@Salvatore Visaggi)

-module(node).
-export([main/0, loop/5, start/0]).
-define(TIMEOUT, 10000).

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
    sleep(10),
    Nonce = make_ref(),
    Node ! {ping, self(), Nonce},
    receive
        {pong, Nonce} ->
          watch(Main,Node)
    after 2000 ->
        Main ! {dead, Node}
    end.

loop(Friends, TToMine, TMined, Chain, Mining) ->
    case length(Friends) of
        % se non ho amici, chiedo al teacher node
        0 ->
            global:send(teacher_node, {get_friends, self(), make_ref()});

        % se ne ho un numero minore compreso tra 1 e 2 chiedo ad uno dei miei amici
        N when N<3 ->
            % sceglo a caso uno tra gli amici e mando una richiesta di amicizia
            %io:format("~p, asking friends for new friends~n", [self()]),
            AskToFriend = lists:nth(rand:uniform(length(Friends)), Friends),
            sendMessage(AskToFriend, {get_friends, self(), make_ref()});
            %AskToFriend ! {get_friends, self(), make_ref()};
        _ ->
            ok
    end,

    receive
        % se ricevo un ping rispondo con un pong al mittente
        {ping, Sender, Nonce} ->
            Sender ! {pong, Nonce},
            loop(Friends, TToMine, TMined, Chain, Mining);

        % se gli altri nodi mi chiedono amici mando semplicemente la lista di amici che ho
        {get_friends, Sender, Nonce} ->
            sendMessage(Sender, {friends, Nonce, Friends}),
            %Sender ! {friends, Nonce, Friends},
            loop(Friends, TToMine, TMined, Chain, Mining);

        % il teacher node o gli altri amici mi rispondono con una lista di amici
        {friends, _, Sent_Friends} ->
            % elimino il PID del mio nodo e degli amici (se presenti)
            New_Friends = Sent_Friends -- [self() | Friends],
            % qui viene salvata la nuova lista di amici
            MyNewFriends = addFriends(Friends, New_Friends),
            %io:format("~p My new friends are: ~p~n", [self(), MyNewFriends]),
            loop(MyNewFriends, TToMine, TMined, Chain, Mining);

        {dead, Node} ->
            %uno degli amici e' morto, ne devo acquisire un altro
            io:format("[~p] - Dead node ~p~n",[self(), Node]),
            loop(Friends -- [Node], TToMine, TMined, Chain, Mining);


        %%%%%%%%%%%%%%%%%%%%%%%   TRANSACTION   %%%%%%%%%%%%%%%%%%%%%%%%%%

        % ricevo una transazione {ID, Payload}, se l'ID non e' già presente la ritrasmetto agli amici
        % l'inserimento nei blocchi da minare è compito del miner avvaito all'inizio con {miner_ready}
        {push, T} ->
            NewTransactions =
                case lists:member(T, TToMine++TMined) of
                    true ->
                        %io:format("[~p] - PUSH_T: Transazione gia' presente... non faccio nulla!~n", [self()]),
                        TToMine;
                    false ->
                        % invio la transazione agli amici e la aggiungo alle transazioni da minare
                        %io:format("[~p] - PUSH_T: Invio la transazione agli amici!~n", [self()]),
                        spawn(fun() -> sendTransToFriends(T, Friends) end),
                        [T | TToMine]
                end,
            loop(Friends, NewTransactions, TMined, Chain, Mining);


        %%%%%%%%%%%%%%%%%%%%%%%   BLOCK   %%%%%%%%%%%%%%%%%%%%%%%%%%

        {update, Sender, Block} ->
            % verifico se il blocco non è già nella mia catena
            case lists:member(Block, Chain) of
                true ->
                    io:format("[~p] - UPDATE_B: Blocco gia' presente... non faccio nulla!~n", [self()]),
                    loop(Friends, TToMine, TMined, Chain, Mining);
                false->
                    %io:format("[~p] - UPDATE_B: Controllo se il blocco e' corretto!~n", [self()]),
                    % verifico se il blocco è corretto
                    case checkBlock(Block) of
                        true ->
                            %blocco corretto, lo ritrasmetto agli amici
                            io:format("[~p] - UPDATE_B: Invio il blocco agli amici!~n", [self()]),
                            spawn(fun() -> sendBlockToFriends(Block, Friends) end),

                            try
                                % chiamo l'algoritmo di ricostruzione della catena
                                io:format("[~p] - UPDATE_B: Chiamo l'algoritmo di ricostruzione della catena!~n", [self()]),
                                chain_reconstruction(Block, Chain, Sender, Friends)
                            catch
                                % blocco scartato per qualche motivo, non faccio nulla
                                discard ->
                                    loop(Friends, TToMine, TMined, Chain, Mining);

                                % ricevuta nuova catena e nuove TMined
                                {NewChain, NewTransactions} ->
                                    io:format("[~p] - CHAIN_RECONSTRUCTION: Ho ricevuto una nuova catena e nuove TMined... aggiorno la mia visione!~n", [self()]),
                                    % se il miner stava lavorando lo killo
                                    case Mining of
                                        none ->
                                            continue;
                                        _ ->
                                            io:format("[~p] - KILL_MINER: Il miner stava lavorando... lo killo!~n", [self()]),
                                            exit(Mining, kill),
                                            self() ! {miner_ready}
                                    end,
                                    TMinedNew = NewTransactions ++ TMined,
                                    TToMineNew = TToMine -- TMinedNew,
                                    loop(Friends, TToMineNew, TMinedNew, NewChain, none)
                            end;

                        false ->
                            io:format("[~p] - UPDATE_B: Blocco non corretto... non faccio nulla!~n", [self()]),
                            loop(Friends, TToMine, TMined, Chain, Mining)
                    end
            end;

        %%%%%%%%%%%%%%%%%%%%%%%   MINER   %%%%%%%%%%%%%%%%%%%%%%%%%%

        % c'è almeno una transazione da minare e il miner è pronto a prendere transazioni per costruire blocchi
        {miner_ready} when length(TToMine) > 0 ->
            io:format("[~p] - MINER: Miner pronto!~n", [self()]),
            NewTToMine = TToMine -- TMined,
            TInMining = lists:sublist(NewTToMine, 10),
            Self = self(),
            case length(Chain) of
                % la catena è vuota, questo blocco sarebbe il primo
                0 ->
                    io:format("[~p] - MINER: Catena vuota, creo primo blocco!~n", [self()]),
                    New_Mining = spawn(fun() -> createBlock(TInMining, none, Self) end);
                % la catena non è vuota
                _ ->
                    io:format("[~p] - MINER: Catena non vuota, creo nuovo blocco!~n", [self()]),
                    [{IdBlock,_,_,_}|_] = Chain,
                    New_Mining = spawn(fun() -> createBlock(TInMining, IdBlock, Self) end)
            end,
            loop(Friends, NewTToMine, TMined, Chain, New_Mining);

        % ho terminato il mining di un nuovo blocco, lo invio a tutti gli amici, ripristino il miner
        % e aggiorno la mia visione della catena
        {block_mined, Block} ->
            % ha finito di minare un blocco, vuol dire che è di nuovo pronto
            self() ! {miner_ready},
            {_,IDPreviousBlock,TransactionsMined,_} = Block,

            case length(Chain) of
                0 ->
                    % il blocco che minato deve avere come IDPreviousBlock none, se non lo ha lo scartiamo
                    case IDPreviousBlock =:= none of
                        true ->
                            io:format("[~p] - BLOCK_MINED: Aggiunto primo blocco minato... invio ad amici!~n", [self()]),
                            spawn(fun() -> sendBlockToFriends(Block, Friends) end), %lo giro agli amici
                            % aggiorno la mia visione della catena e delle transazioni minate e non
                            loop(Friends, TToMine--TransactionsMined, TMined++TransactionsMined, [Block]++Chain, Mining);
                        false ->
                            io:format("[~p] - BLOCK_DISCARDED: Catena vuota ma il blocco minato non ha come previous none!~n", [self()]),
                            loop(Friends, TToMine, TMined, Chain, Mining)
                    end;
                _ ->
                    % il blocco minato deve avere come IDPreviousBlock l'ID della testa della catena
                    % se così non fosse vuol dire che c'è un buco
                    [{IdBlock,_,_,_} | _] = Chain,
                    case IdBlock =:= IDPreviousBlock of
                        true ->
                            io:format("[~p] - BLOCK_MINED: Aggiunto nuovo blocco minato... invio ad amici!~n", [self()]),
                            spawn(fun() -> sendBlockToFriends(Block, Friends) end), %lo giro agli amici
                            % aggiorno la mia visione della catena e delle transazioni minate e non
                            loop(Friends, TToMine--TransactionsMined, TMined++TransactionsMined, [Block]++Chain, Mining);
                        false ->
                            io:format("[~p] - BLOCK_DISCARDED: Blocco minato ma non punta alla testa della chain!~n", [self()]),
                            loop(Friends, TToMine, TMined, Chain, Mining)
                    end
            end;


        %%%%%%%%%%%%%%%%%%%%%%%   CHAIN   %%%%%%%%%%%%%%%%%%%%%%%%%%

        {getChain} ->
            %io:format("[~p] - Chiedo la catena ad uno degli amici amici!~n", [self()]),
            case length(Friends) of
                0 ->
                    io:format("[~p] - GETCHAIN: Non ho amici a cui chiedere la catena! :(~n", [self()]),
                    loop(Friends, TToMine, TMined, Chain, Mining);
                _ ->
                    io:format("[~p] - GETCHAIN: Chiedo la catena ad un amico!~n", [self()]),
                    Friend = lists:nth(rand:uniform(length(Friends)), Friends),
                    {NewTMined, NewChain} = getChain(Friend, Friends -- [Friend]),
                    loop(Friends, TToMine, NewTMined, NewChain, Mining)
            end;

        {getHead, Sender, Nonce} ->
            case length(Chain) of
                % catena vuota mando il messaggio
                0 ->
                    io:format("[~p] - GETCHAIN: La mia catena e' vuota... lo notifico all'amico!~n", [self()]),
                    sendMessage(Sender, {head, Nonce, empty});
                    %Sender ! {head, Nonce, empty};
                % catena con almeno un elemento, mando la testa all'amico
                _ ->
                    io:format("[~p] - GETCHAIN: La mia catena non e' vuota... mando all'amico la testa!~n", [self()]),
                    [H|_] = Chain,
                    sendMessage(Sender, {head, Nonce, H})
                    %Sender ! {head, Nonce, H}
            end,
            loop(Friends, TToMine, TMined, Chain, Mining);

        {get_previous, Sender, Nonce, IDPreviousBlock} ->
            Block = searchBlock(IDPreviousBlock, Chain),
            case Block of
                none -> stop;
                _ ->
                    sendMessage(Sender, {previous, Nonce, Block})
                    %Sender ! {previous, Nonce, Block}
            end,
            loop(Friends, TToMine, TMined, Chain, Mining);

        %%%%%%%%%%%%%%%%%%%%%%%   UTILS   %%%%%%%%%%%%%%%%%%%%%%%%%%

        {getC} ->
            io:format("[~p] - CATENA : ~p~n~n~n", [self(), Chain])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% se la lista dei nuovi amici e' vuota restituisco la lista di amici vecchia
addFriends(Friends, []) ->
    Friends;

%se la lista di miei amici e' vuota devo aggingerne qualcuno
addFriends([], New_Friends) ->
    % scelgo uno tra gli amici a caso
    ToAddFriend = lists:nth(rand:uniform(length(New_Friends)), New_Friends),
    Self = self(),
    spawn(fun() -> watch(Self, ToAddFriend) end),
    addFriends([ToAddFriend], New_Friends -- [ToAddFriend]);

addFriends(Friends, New_Friends) ->
    case length(Friends) < 3 of
        true ->
            ToAddFriend = lists:nth(rand:uniform(length(New_Friends)), New_Friends),
            Self = self(),
            spawn(fun() -> watch(Self, ToAddFriend) end),
            addFriends([ToAddFriend | Friends], New_Friends -- [ToAddFriend]);
        false ->
            Friends
    end.


%%%%%%%%%%%%%%%%%%%%%%%   TRANSACTION   %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%caso base (chain vuota o blocco non trovato)
sendTransToFriends(_, []) ->
    %io:format("SEND_TRANSACTION: Finito invio transazione~n"),
    ok;

sendTransToFriends(T, [F|Friends]) ->
    %io:format("SEND_TRANSACTION: Invio all'amico [~p]~n", [F]),
    sendMessage(F,{push, T}),
    %F ! {push, T},
    sendTransToFriends(T, Friends -- [F]).


%%%%%%%%%%%%%%%%%%%%%%%%%%   BLOCK   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ho finito di scorrere la catena oppure la catena e' vuota e non ho trovato nulla
searchBlock(_, []) -> none;

% se trova nella catena un blocco con quell'ID (IDToSearch) lo restituisce, altrimenti restituisce none
searchBlock(IDToSearch, Chain) ->
    [H|T] = Chain,
    {IDBlock,_,_,_} = H,

    case IDToSearch =:= IDBlock of
        true -> H;
        false -> searchBlock(IDToSearch, T)
    end.

sendBlockToFriends(_,[]) ->
    %io:format("SEND_BLOCK: Finito invio!~n"),
    ok;

sendBlockToFriends(Block, [F| Friends]) ->
    %io:format("SEND_BLOCK: Invio all'amico [~p]~n", [F]),
    sendMessage(F, {update, self(), Block}),
    %F ! {update, self(), Block},
    sendBlockToFriends(Block, Friends).

checkBlock(Block) ->
    {_,IDPreviousBlock,Transactions,Solution} = Block,
    proof_of_work:check({IDPreviousBlock, Transactions}, Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%   MINER   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% funzione per il mining di un blocco, al termine invia al sender un messaggio con il nuovo blocco minato
createBlock(Transactions, IdPrevBlock, Sender) ->
    Solution = proof_of_work:solve({IdPrevBlock, Transactions}),
    NewBlock = {make_ref(), IdPrevBlock, Transactions, Solution},
    Sender ! {block_mined, NewBlock}.


%%%%%%%%%%%%%%%%%%%%%%%%%%   CHAIN   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% caso base: ho finito gli amici a cui chiedere, resituisco catena e TMined vuote
getChain(_, []) ->
    io:format("[~p] - GETCHAIN: Non ho piu' amici... restituisco la catena vuota!~n", [self()]),
    {[],[]};

% chiedo la visione della catena ad uno dei miei amici (lo faccio solo all'inizio!)
getChain(Friend, FriendsToAsk) ->
    % chiedo la cima della chain ad un amico
    sendMessage(Friend, {getHead, self(), make_ref()}),
    %Friend ! {getHead, self(), make_ref()},
    receive
        % la chain dell'amico e' vuota, allora chiedo ad un altro amico
        {head, _, empty} ->
            io:format("[~p] - GETCHAIN: La catena dell'amico e' vuota... chiedo ad un'altro amico!~n", [self()]),
            AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
            getChain(AnotherFriend, FriendsToAsk -- [AnotherFriend]);

        % ho ricevuto la testa da uno degli amici, vado a ricostruire il resto della catena
        {head, _, Block} ->
            io:format("[~p] - GETCHAIN: Ricevuta testa blockchain... continuo a costruire la catena!~n", [self()]),
            {_, IDPreviousBlock, Transactions, _} = Block,
            getRemainingChain(Friend, FriendsToAsk, IDPreviousBlock, [Transactions], [Block])

    % se non ricevo alcuna risposta dall'amico per un tot di tempo chiedo ad un altro amico
    after ?TIMEOUT ->
        case length(FriendsToAsk) of
            0 -> {[],[]};   % restituisco catena vuota
            _ ->
                io:format("[~p] - GETCHAIN: L'amico non risponde... chiedo ad un altro amico!~n", [self()]),
                AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
                getChain(AnotherFriend, FriendsToAsk -- [AnotherFriend])
        end
    end.

% ho finito la lista di amici, allora restituisco la catena vuota
getRemainingChain(_,[],_,_,_) ->
    io:format("[~p] - GETREMCHAIN: Non ho piu' amici... restituisco la catena vuota!~n", [self()]),
    {[],[]};

% caso in cui sono arrivato all'ultimo blocco della catena e quindi la resituisco insieme
% alle transazioni minate (presenti nei blocchi della catena)
getRemainingChain(_, _, none, TMined, Chain) ->
    io:format("[~p] - GETREMCHAIN: Ricostruita intera catena!~n", [self()]),
    {TMined, Chain};

getRemainingChain(Friend, FriendsToAsk, IDPreviousBlock, TMined, Chain) ->
    io:format("[~p] - GETREMCHAIN: Chiedo all'amico il precedente blocco!~n", [self()]),
    sendMessage(Friend, {get_previous, self(), make_ref(), IDPreviousBlock}),
    %Friend ! {get_previous, self(), make_ref(), IDPreviousBlock},
    receive
        {previous, _, Block} ->
            io:format("[~p] - GETREMCHAIN: Ricevuto blocco successivo!~n", [self()]),
            {_, IDPreviousBlockTemp, Transactions, _} = Block,
            sendMessage(Friend, {get_previous, self(), make_ref(), IDPreviousBlockTemp}),
            %Friend ! {get_previous, self(), make_ref(), IDPreviousBlockTemp},
            getRemainingChain(Friend, FriendsToAsk, IDPreviousBlockTemp, TMined++Transactions, [Block]++Chain)

    % se non ricevo alcuna risposta dall'amico per un tot di tempo chiedo ad un altro amico
    after ?TIMEOUT ->
        case length(FriendsToAsk) of
            0 -> {[],[]};   % restituisco la catena vuota
            _ ->
                io:format("[~p] - GETREMCHAIN: L'amico non risponde... chiedo ad un altro amico!~n", [self()]),
                AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
                getRemainingChain(AnotherFriend, FriendsToAsk -- [AnotherFriend], IDPreviousBlock, TMined, Chain)
        end
    end.

chain_reconstruction(Block, Chain, Sender, Friends) ->
    {_,IDPreviousBlock,TInBlock,_} = Block,

    % la mia catena è vuota, allora questo blocco ricevuto costituirà la nuova catena
    case length(Chain) =:= 0 of
        true ->
            io:format("[~p] - CHAIN_RECONSTRUCTION: La mia catena e' vuota, questo blocco ricevuto diventa la mia nuova catena!~n", [self()]),
            throw( {[Block],TInBlock} );
        false ->
            continue
    end,

    % la mia catena non è vuota e il blocco ricevuto punta alla testa della mia catena,
    % la nuova catena avrà come nuova testa questo blocco ricevuto
    [{IDBlock,_,_,_}|_] = Chain,
    case IDPreviousBlock=:=IDBlock of
        true ->
            io:format("[~p] - CHAIN_RECONSTRUCTION: Blocco ricevuto punta alla testa della mia catena... lo aggiungo!~n", [self()]),
            throw( {[Block]++Chain, TInBlock} );
        false -> continue
    end,

    % cerco se il previousId del blocco ricevuto è presente o no nella mia catena
    case searchBlock(IDPreviousBlock, Chain) of
        none ->
            % il blocco precedente non è stato trovato nella mia catena,
            % vado a cercarlo tra quelle degli amici o del sender
            io:format("[~p] - CHAIN_RECONSTRUCTION: Previous del blocco ricevuto non trovato nella mia catena... cerco in quelle di Sender/Friends!~n", [self()]),
            searchBlockOthers([], IDPreviousBlock, Chain, Sender, Friends--[Sender]);
        _ ->
            % il padre del blocco ricevuto è già presente nella catena (e ha altri blocchi collegati), viene quindi scartato
            io:format("[~p] - CHAIN_RECONSTRUCTION: Previous di blocco ricevuto trovato nella mia catena ma non e' la testa... blocco ricevuto scartato!~n", [self()]),
            throw(discard)
    end.

searchBlockOthers(OtherChain, Block, MyChain, Sender, Friends) ->
    {IDBlock,IDPreviousBlock,_,_} = Block,

    case IDPreviousBlock of
        % ho scorso tutta la catena dell'amico, quindi devo decidere quale delle due accettare
        none ->
            io:format("[~p] - CR_SEARCH_PREV_OTHERS: Scorso tutta la catena dell'amico... decidere se accettare catena mia o di friend!~n", [self()]),
            case length(MyChain) >= length(OtherChain++[Block]) of
                % la mia catena è almeno uguale all'altra, quindi scarto il blocco
                true ->
                    io:format("[~p] - CR_SEARCH_PREV_OTHERS: My Chain piu' lunga dell'altra... blocco ricevuto scartato!~n", [self()]),
                    throw(discard);
                % la catena dell'amico è più grande, allora diventa la mia nuova catena
                false ->
                    io:format("[~p] - CR_SEARCH_PREV_OTHERS: My chain piu' piccola dell'altra... accetto Other Chain!~n", [self()]),
                    NewTransactions = lists:flatmap(fun(A) -> {_,_,X,_}=A, X end, OtherChain++[Block]),
                    throw({OtherChain++[Block], NewTransactions})
            end;
        _ -> continue
    end,

    % controllo se l'ID del blocco ricevuto è presente nella mia catena:
    %   * se non c'è vuol dire che non c'è una biforcazione
    %   * se c'è vuol dire che abbiamo trovato una biforcazione
    case searchBlock(IDBlock, MyChain) of
        % l'ID del blocco ricevuto non è nella mia catena quindi chiedo al Sender o agli amici
        none ->
            % searchPrevious cerca tra le catene degli amici il blocco a cui punta il blocco ricevuto
            % se non lo trova allora lo scarta
            io:format("[~p] - CR_SEARCH_PREV_OTHERS(ciclo): Block non trovato in My chain... cerco previous dall'amico!~n", [self()]),
            Prev = searchPrevious(IDPreviousBlock, [Sender]++Friends),
            searchBlockOthers(OtherChain++[Prev], Prev, MyChain, Sender, Friends);

        % nell'iterazione sono arrivato ad un blocco in comune quindi ho una biforcazione
        CommonBlock ->
            io:format("[~p] - CR_SEARCH_PREV_OTHERS(ciclo): Trovata biforcazione!~n", [self()]),
            % indice nella mia catena del blocco in comune
            IndexCommonBlockInMyChain = blockIndex(CommonBlock, MyChain, 1),
            CommonChain = lists:nthtail(IndexCommonBlockInMyChain-1, MyChain),
            MyChainToCompareLength = IndexCommonBlockInMyChain-1,
            OtherChainToCompareLength = length(OtherChain),

            case OtherChainToCompareLength > MyChainToCompareLength of
                true ->
                    % verifico se in OtherChain ci sono transazioni già presenti in MyChain
                    case commonTransactionsVerify(CommonChain, OtherChain) of
                        % scarto il blocco perchè ci sono transazioni in OtherChain già presenti nella mia catena
                        true ->
                            throw(discard); % scarto il blocco e rimane la catena mia
                        % non ci sono transazioni duplicate, accetto quindi la nuova catena e la unisco alla mia
                        false ->
                            NewTrans = lists:flatmap(fun(A)->{_,_,X,_}=A, X end, OtherChain),
                            NewChain = OtherChain ++ CommonChain,
                            throw({NewChain, NewTrans})
                    end;
                % nella biforcazione la mia catena è più lunga dell'altra
                false -> throw(discard)
            end
    end.

% caso base, ho finito gli amici a cui chiedere e non ho trovato il precedente,
% il blocco ricevuto viene scartato
searchPrevious(_,[]) ->
    io:format("[~p] - CR_SEARCH_PREVIOUS(ciclo): Finito la lista di amici... scarto il blocco ricevuto!~n", [self()]),
    throw(discard);

% vedo se il Sender o gli amici hanno il blocco a cui punta il blocco ricevuto
searchPrevious(IDPreviousBlock, Friends) ->
    [Friend|_] = Friends,
    Nonce = make_ref(),
    sendMessage(Friend, {get_previous, self(), Nonce, IDPreviousBlock}),
    %Friend ! {get_previous, self(), Nonce, IDPreviousBlock},
    receive
        {previous, Nonce, Block} ->
            io:format("[~p] - CR_SEARCH_PREVIOUS(ciclo): Ricevuto blocco precedente... verifico correttezza e restituisco!~n", [self()]),
            case checkBlock(Block) of
                % se il blocco ricevuto è valido lo restituisco
                true -> Block;
                 % il blocco non è valido quindi chiedo ad un altro amico
                false -> searchPrevious(IDPreviousBlock,Friends--[Friend])
            end
    after
        % se non ricevo risposta da un tot di tempo allora chiedo ad un altro amico
        ?TIMEOUT ->
            io:format("[~p] - CR_SEARCH_PREVIOUS(ciclo): Friend non risponde... chiedo a un altro amico!~n", [self()]),
            searchPrevious(IDPreviousBlock,Friends--[Friend])
    end.

% restituisce la posizione di Block in Chain
blockIndex(Block, Chain, Index) ->
    [H|T] = Chain,
    case Block =:= H of
        true -> Index;
        false -> blockIndex(Block, T, Index+1)
    end.

% verifica se nelle due catene ci sono transazioni duplicate
commonTransactionsVerify(CommonChain, OtherChain) ->
    TransactionsInCommonChain = lists:flatmap(fun(A)->{_,_,X,_}=A, X end, CommonChain),
    TransactionsInOtherChain = lists:flatmap(fun(A)->{_,_,X,_}=A, X end, OtherChain),
    SetTInCommonChain = sets:from_list(TransactionsInCommonChain),
    SetTInOtherChain = sets:from_list(TransactionsInOtherChain),
    Intersection = sets:intersection(SetTInCommonChain,SetTInOtherChain),
    case sets:size(Intersection) of
        0 -> false;
        _ -> true
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%   UTILS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendMessage(Receiver, Msg) ->
    case rand:uniform(10) of
        1 ->
            %io:format("SEND_MSG: Inviati 2 messaggi!~n"),
            Receiver ! Msg,
            Receiver ! Msg;
        2 ->
            %io:format("SEND_MSG: Messaggio perso!~n"),
            nothing;
        _ ->
            %io:format("SEND_MSG: Messaggio inviato normalmente!~n"),
            Receiver ! Msg
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start() ->
    io:format("~n[~p] - ASKING FOR FRIENDS TO TEACHER~n", [self()]),
    global:send(teacher_node, {get_friends, self(), make_ref()}),
    sleep(5),

    io:format("~n[~p] - ASK TO FRIENDS FOR INITIAL CHAIN~n", [self()]),
    self() ! {getChain},

    io:format("~n[~p] - INVOKE MINER~n", [self()]),
    self() ! {miner_ready},

    loop([],[],[],[],none).


main() ->
    spawn(teacher_node, main, []),
    sleep(5),
    N1 = spawn(node, start, []),
    N2 = spawn(node, start, []),
    N3 = spawn(node, start, []),
    N4 = spawn(node, start, []),

    sleep(10),
    io:format("~n~n1. PUSHO 4 TRANSAZIONI!~n"),
    N1 ! {push, {make_ref(), "ciao1"}},
    N1 ! {push, {make_ref(), "ciao2"}},
    N1 ! {push, {make_ref(), "ciao3"}},
    N1 ! {push, {make_ref(), "ciao4"}},

    sleep(10),
    io:format("~n~n2. PUSHO 4 TRANSAZIONI!~n"),
    N1 ! {push, {make_ref(), "ciao11"}},
    N1 ! {push, {make_ref(), "ciao21"}},
    N1 ! {push, {make_ref(), "ciao31"}},
    N1 ! {push, {make_ref(), "ciao41"}},

    sleep(3),
    io:format("~n~n3. PUSHO 4 TRANSAZIONI!~n"),
    N2 ! {push, {make_ref(), "ciao111"}},
    N2 ! {push, {make_ref(), "ciao112"}},
    N2 ! {push, {make_ref(), "ciao113"}},
    N2 ! {push, {make_ref(), "ciao114"}},

    sleep(10),
    io:format("~n~n4. PUSHO 4 TRANSAZIONI!~n"),
    N4 ! {push, {make_ref(), "ciao1111"}},
    N4 ! {push, {make_ref(), "ciao1112"}},
    N4 ! {push, {make_ref(), "ciao1113"}},

    sleep(10),
    N1 ! {getC},
    N2 ! {getC},
    N3 ! {getC},
    N4 ! {getC}.
