-module(mosa_node).
-author("Andrea Morabito, Paolo Santilli").
-export([main/1,test/0]).
-define(MaxNumberOfFriends, 3).
-define(TimeSleep, 2).
-define(TeacherNodeUrl, 'C:/Users/paolo.santilli2/Desktop/Unibo/Emerging Paradigms Programming/Erlang/MiniErlangBlockchain/teacher_node.erl').
-define(ProofOfWorkUrl, 'C:/Users/paolo.santilli2/Desktop/Unibo/Emerging Paradigms Programming/Erlang/MiniErlangBlockchain/proof_of_work.erl').
-on_load(load_module/0).

sleep(N) -> receive after N*1000 -> ok end.


watch(Main,Node) ->
 	sleep(10),
	Ref = make_ref(),
 	Node ! {ping, self(), Ref},
 	receive
		{pong, Ref} -> watch(Main,Node)
 	after 2000 -> Main ! {dead, Node}
	end.

load_module() -> 	compile:file(?TeacherNodeUrl),
					compile:file(?ProofOfWorkUrl),
					ok.


loop(MyListFriends,TransactionsList,MyChain) ->
	% Controllo che la mia lista di amici
	% Nel caso in cui è vuota chiedo al teacher_node di darmene uno
	% Altrimenti chiedo ad uno la sua lista di amci 
	
	case length(MyListFriends) =:= 0 of
		true 	-> 	
				global:send(teacher_node,{get_friends,self(),make_ref});
		false ->
				askToFriend(MyListFriends)
				
	end,

	% Mi metto in ascolto dei messaggi in arrivo
	receive
		{ping, Mittente, Nonce} -> 
			io:format("~p Received ping request! Send pong response ~n ~n",[self()]),			
			sendMsg(Mittente,{pong, Nonce}),
			loop(MyListFriends,TransactionsList,MyChain);

		{dead, Node} ->
			io:format("For ~p the node ~p is dead ~n ~n",[self(),Node]),

            UpdatedList = MyListFriends -- [Node],
			loop(UpdatedList,TransactionsList,MyChain);

		{friends, Nonce, ListOtherFriends} ->
			io:format("~p Receive friend list ~p ~p ~n ~n",[self(),ListOtherFriends,Nonce]),

			% Trovo una lista di amici dall'elenco degli amici ricevuta (escludendo me e gli amici già presenti)
            NewFriends = ListOtherFriends --  (MyListFriends ++ [self()]),
            
            % Aggiorno la mia lista			
            MyNewListFriends = addFriend(MyListFriends, NewFriends),
			
            loop(MyNewListFriends,TransactionsList,MyChain);

		{get_friends, Sender, Nonce} ->
			io:format("~p Send friend list to ~p ~n ~n",[self(),Sender]),
			Mess = {friends, Nonce, MyListFriends},
			sendMsg(Sender, Mess),
            loop(MyListFriends,TransactionsList,MyChain);
			
			

        % Gossiping di una  Transazione = {IDtransazione, Payload}
        {push, Transazione} ->
        	io:format(" Questa è la transazione: ~p ~n~n",[Transazione]),
        	case lists:member(Transazione, TransactionsList) of
        		true -> loop(MyListFriends,TransactionsList,MyChain); %Se la transazione è gia presente nella mia lista
        		false -> 
					
					TList = TransactionsList ++ [Transazione],

					io:format("This is the transaction list of ~p: ~p ~n~n",[self(),TList]),
        			% Mando la transazione a tutti i miei amici
        			Mess = {push, Transazione},

        			sendMsgToList(MyListFriends, Mess),

        			% Controllo la dimensione della lista delle Transazioni
        			% Nel caso in cui comprenda 10 transazioni creo un nuovo blocco
        			% Altrimenti posso andare avanti
        			case length(TList) =:= 6 of
        				true -> 
        					io:format("Transactions list of ~p is full! Creation new Block ~n ~n",[self()]),
        					
        					case MyChain of
        						[] -> 
									Soluzione = proof_of_work:solve({none, TList}),
        							NewBlock = {make_ref(), none, TList, Soluzione};

								_ ->
        							% Prendo la testa della mia catena
        							{Id, Prev, _, _} = lists:last(MyChain),
									Soluzione = proof_of_work:solve({Prev, TList}),
        							NewBlock = {make_ref(), Id, TList, Soluzione}
        					end,

        					MessBlock = {update, self(), NewBlock},
        					sendMsgToList(MyListFriends, MessBlock),

        					% Aggiorno la mia catena 
        					Chain = MyChain ++ [NewBlock],
							io:format("Chain of ~p is : ~p ~n~n",[self(),Chain]),

        					% Mi rimetto in ascolto dei messaggi che arrivano con una nuova lista di transazioni
        					loop(MyListFriends, [], Chain);

        				false -> 	io:format("-------- ~p Successful insertion Transaction and list is ~p ~n~n",[self(),TList]),
        							loop(MyListFriends, TList, MyChain)


        			end
        	end;
			
        % Gossiping di un  Blocco = {IDnuovo_blocco, IDblocco_precedente, Lista_di_amici, Soluzione}
        {update, Sender, Blocco} ->

			  	{IDnuovo_blocco, IDblocco_precedente, Lista_di_transazioni, Soluzione} = Blocco,

				io:format("Sender: ~p and ID_nuovo_blocco: ~p ~n~n" , [Sender,IDnuovo_blocco]),

        	case lists:member(Blocco, MyChain) of % Controllo che il blocco da minare non sia gia presente nella mia catena
        		true -> io:format("Block already in chain ~n~n");
        		false ->
        			case proof_of_work:check({IDblocco_precedente, Lista_di_transazioni}, Soluzione) of
        				true ->
        					io:format("Block can be added to the chain of ~p ~n~n",[self()]),
        					% ricostruzione catena chiedendo al sender

        					Chain = MyChain ++ [Blocco],

        					% Mi rimetto in ascolto di messaggi con la lista di transazioni aggiornata
        					loop(MyListFriends, [], Chain);

        				false -> 	io:format("Block already in the chain ~n~n"),
        							loop(MyListFriends, TransactionsList, MyChain)
        			end
        	end;

        % Se non si conosce il blocco precedente (es. appena connesso alla blockchain)
        {get_previous, Mittente, Nonce, IDblocco_precedente} ->
        	io:format("Send of previous Block ~n~n"),

        	PreviuousBlock = searchBlock(MyChain, IDblocco_precedente),

        	Mess = {previous, Nonce, PreviuousBlock},
        	sendMsg(Mittente, Mess),

        	loop(MyListFriends, TransactionsList, MyChain);

        {previous, Nonce, Blocco} -> 
        	{Id_block, _, _, _} = Blocco,
        	io:format("Get previous Block of ~p ~p ~n", [Id_block,Nonce]),

        	[ _ | Blocco] = MyChain ,

        	loop(MyListFriends, TransactionsList, MyChain);

        {get_head, Mittente, Nonce} -> 
        	io:format("Send the head of chain of ~p ~n ~n",[self()]),

        	Mess = {head, Nonce, lists:last(MyChain)},
        	sendMsg(Mittente, Mess),


        	loop(MyListFriends, TransactionsList, MyChain);

        {head, Nonce, Blocco} ->
        	io:format("Get head of the Block ~p of process ~p ~n ~n",[Blocco,self()]),

        	[Blocco | _ ] = MyChain,

        	loop(MyListFriends, TransactionsList, MyChain);

		{printT,NameNode} -> io:format("Transaction List of ~p : ~p ~n~n",[NameNode,TransactionsList]),
							loop(MyListFriends,TransactionsList,MyChain);

		{printC,NameNode} -> io:format("~p) Chain of ~p : ~p ~n~n",[NameNode,self(),MyChain]),
							 loop(MyListFriends,TransactionsList,MyChain)

			
	end.


% Chiedo ad un amico casuale la sua lista di amici

askToFriend(MyListFriends) ->
	
	case length(MyListFriends) < ?MaxNumberOfFriends of
		true ->	io:format("~p Require list friends to a random friend of this ~p ~n ~n",[self(),MyListFriends]),

				Mess = {get_friends, self(), make_ref()},
				Friend = lists:nth(rand:uniform(length(MyListFriends)), MyListFriends),
				sendMsg(Friend, Mess);
				
		false -> io:format("~p Have already three friends ~n ~n",[self()]),
				 nothing_to_do
	end.

% Aggiungo un amico alla mia lista
addFriend(MyListFriends, []) -> MyListFriends;

addFriend(MyListFriends, NewFriends) ->
	
	case length(MyListFriends) < ?MaxNumberOfFriends of
		true ->

			Friend = lists:nth(rand:uniform(length(NewFriends)), NewFriends),
			Self = self(),
			spawn(fun () -> watch(Self, Friend) end),
			addFriend(MyListFriends ++ [Friend], NewFriends -- [Friend]);		

		false ->io:format("List of ~p friends is full ~p ~n ~n",[self(),length(MyListFriends)])
end.


sendMsg(Sender, Msg) ->
			io:format("Send message to ~p ~n~n",[Sender]),
			case rand:uniform(10) of 
				1 ->io:format("Messaggio di ~p perso ~n~n",[self()]), 
					lost;

				2 -> 
					io:format("Messaggio di ~p mandato due volte ~n~n",[self()]),
					Sender ! Msg,
					Sender ! Msg;
				
				_ -> Sender ! Msg
			end.
				
	

sendMsgToList(ListFriend,Msg) ->
	io:format(" ~p Send messages to list ~p ~n~n",[self(),ListFriend]),
	lists:foreach(fun (Friend) -> 
		case rand:uniform(10) of 
			1 ->io:format("Messaggio di ~p perso ~n~n",[self()]), 
				lost;

			2 -> io:format("Messaggio di ~p mandato due volte ~n~n",[self()]),
				 Friend ! Msg,
				 Friend ! Msg;

			_ -> Friend ! Msg
		end
	end,ListFriend).

searchBlock([H|T],Id) ->

	{ID, Precedente, _ , _ } = H,

	io:format("Precedente: ~p ~n~n" , [Precedente]),

	case ID =:= Id of
		true -> H;
		false -> searchBlock(T,Id)
	end.


% Mi registro nella lista globale ed inizio
main(NameNode) -> 
	
  	register(NameNode ,self()),
 	global:register_name(NameNode,self()),
	loop([],[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTING CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendTransaction(Dest,Payload) ->
    Dest ! {push, {make_ref(), Payload}},
    io:format("> Send ~p to ~p ~n~n",[Payload,Dest]).

test() ->
	
	
	spawn(teacher_node, main, []), % teacher node
  	sleep(1),
	
	TIME_TO_TRANS = 3,

	N1 = spawn(fun() -> main(n1) end),
	io:format("N1) ->  ~p ~n ~n",[N1]),
	sleep(2),
	N2 = spawn(fun() -> main(n2) end),
	io:format("N2) ->  ~p ~n ~n",[N2]),
	sleep(2),
	N3 = spawn(fun() -> main(n3) end),
	io:format("N3) ->  ~p ~n ~n",[N3]),
	sleep(2),
	io:format("Finish to spawn nodes and teacher-node ~n ~n"),
	sleep(2),

	sleep(5),
    io:format("Testing Transaction...~n~n"),
    sendTransaction(N1,"Transaction 1 "),
    sleep(TIME_TO_TRANS),
    sendTransaction(N1,"Transaction 2 "),
    sleep(TIME_TO_TRANS),
    sendTransaction(N2,"Transaction 3 "),
    sleep(TIME_TO_TRANS),
    sendTransaction(N2,"Transaction 4 "),
    sleep(TIME_TO_TRANS*5),
    sendTransaction(N3,"Transaction 5 "),
    sendTransaction(N3,"Transaction 6 "),

	
	N4 = spawn(fun() -> main(n4) end),
	io:format("N4) ->  ~p ~n ~n",[N4]),
	sleep(?TimeSleep),
	N5 = spawn(fun() -> main(n5) end),
	io:format("N5) ->  ~p ~n ~n",[N5]),
	sleep(?TimeSleep),
	sendTransaction(N4,"Transaction 7 "),
    sendTransaction(N5,"Transaction 8 "),
	sleep(20),

	N1 ! {printC,n1},
	N2 ! {printC,n2},
	N3 ! {printC,n3},
	N4 ! {printC,n4},
	N5 ! {printC,n5},
	sleep(10).


