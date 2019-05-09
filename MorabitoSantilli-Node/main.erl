-module(main).
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
	
	case MyListFriends of
		[] ->  global:send(teacher_node,{get_friends,self(),make_ref()});
			   
		_ ->   askToFriend(MyListFriends)
			   
	end,

	% Mi metto in ascolto dei messaggi in arrivo
	receive
		{ping, Mittente, Nonce} -> 
			sendMsg(Mittente,{pong, Nonce}),
			loop(MyListFriends,TransactionsList,MyChain);

		{dead, Node} ->
			io:format("For ~p the node ~p is dead ~n ~n",[self(),Node]),

            UpdatedList = MyListFriends -- [Node],
			loop(UpdatedList,TransactionsList,MyChain);

		{friends, Nonce, ListOtherFriends} ->

			% Trovo una lista di amici dall'elenco degli amici ricevuta (escludendo me e gli amici già presenti)
            % Aggiorno la mia lista	
			NewFriends = ListOtherFriends -- (MyListFriends ++ [self()]),

			case NewFriends of

				[] -> global:send(teacher_node,{get_friends,self(),make_ref()}),
					  loop(MyListFriends,TransactionsList,MyChain);

				_ -> MyNewListFriends = addFriend(MyListFriends, NewFriends),
					 loop(MyNewListFriends,TransactionsList,MyChain)
			end;		

		{get_friends, Sender, Nonce} ->
			Mess = {friends, Nonce, MyListFriends},
			sendMsg(Sender, Mess),
            loop(MyListFriends,TransactionsList,MyChain);
			
			

        % Gossiping di una  Transazione = {IDtransazione, Payload}
        {push, Transazione} ->
        	case lists:member(Transazione, TransactionsList) of
        		true -> TransactionsList,  %Se la transazione è gia presente nella mia lista
                        loop(MyListFriends, TransactionsList, MyChain);
				false -> 

					%Controllo che la transazione non sia presente nella catena 
					SearchT = searchTransactionInTheChain(Transazione,MyChain),
					
					case SearchT of
						true -> TList = TransactionsList -- [Transazione];
					
						false ->TList = TransactionsList ++ [Transazione]
					end,

					% Mando la transazione a tutti i miei amici
        			Mess = {push, Transazione},
        			sendMsgToList(MyListFriends, Mess),

        			%Scelgo random se minare o no la lista di transizioni 
        			case rand:uniform(2) of
        				1 when length(TList) > 0 -> 
							% Se decido di minare controllo che la lista sia minore di 10 
        					case length(TList) < 10  of 
								true -> Result = mineTransactions(TList,MyChain),
        								MessBlock = {update, self(), Result},
    									sendMsgToList(MyListFriends, MessBlock),
										Chain = MyChain ++ [Result],
										loop(MyListFriends, [], Chain);

								% Se è maggiore di 10 creo una sottolista
								false -> Trans_List = lists:sublist(TList,10),
										 Result = mineTransactions(Trans_List,MyChain),
										 MessBlock = {update, self(), Result},
    									 sendMsgToList(MyListFriends, MessBlock),
										 Chain = MyChain ++ [Result],
										 loop(MyListFriends, TList -- [Trans_List], Chain)
										 
   							end;

						1 when length(TList) =< 0 -> loop(MyListFriends,[],MyChain);

        				2 ->% Se decido di non minare la lista aggiungo la transazione alla lista   
							 loop(MyListFriends, TransactionsList ++ [Transazione], MyChain)


        			end
        	end;
			
        % Gossiping di un  Blocco = {IDnuovo_blocco, IDblocco_precedente, Lista_di_amici, Soluzione}
        {update, Sender, Blocco} ->
        	{IDnuovo_blocco, IDblocco_precedente, Lista_di_transazioni, Soluzione} = Blocco,

        	case lists:member(Blocco, MyChain) of % Controllo che il blocco da minare non sia gia presente nella mia catena
        		true -> loop(MyListFriends, TransactionsList, MyChain);
        		false ->
        			case proof_of_work:check({IDblocco_precedente, Lista_di_transazioni}, Soluzione) of % Controllo che il blocco possa essere inserito nella catena
        				true ->
        					%io:format("Block ~p from ~p to the chain ~p of ~p ~n",[Blocco,Sender,MyChain,self()]),

							% Controllo che nessuna transazione del blocco arrivato sia già presente nella mia catena e
							% nel caso ci sia non accetto il blocco
							TNotMined =  searchOtherTransactionInTheChain(Lista_di_transazioni,MyChain),

							case TNotMined of 

								true ->% io:format("~p TNotMined ~p ~n~n",[self(),TNotMined]),
								 	  loop(MyListFriends,TransactionsList,MyChain);
								
							    false -> %io:format("~p Vuota ~n~n",[self()]),
									  % Aggiorno la mia catena
									  Chain = MyChain ++ [Blocco],
									  
									  % Ricostruzione catena
									  % Se la mia catena ha un solo blocco
									  case length(Chain) =:= 1 of
											true -> 
											% Se ho solo un blocco ed l'ID del blocco precedente è uguale a none la catena è composta da un solo blocco
											% Altrimenti bisogna ricostruire la catena chiedendo al sender la sua
												case IDblocco_precedente =:= none of
													true -> %io:format(" First Block ~p of the chain of ~p added ---- ~p ~p ~n",[Blocco,self(),IDnuovo_blocco,Sender]);
															nothing_to_do;
													false ->
														Nonce = make_ref(),
														Mess = {get_previous, self(), Nonce, IDblocco_precedente},
														% Chiedo ad uno dei miei amici il blocco precedente
														sendMsg(lists:nth(rand:uniform(length(MyListFriends)), MyListFriends), Mess)
												end;

											false -> nothing_to_do										 												
									  end,

									  % Mi rimetto in ascolto di messaggi con la lista di transazioni aggiornata
									  loop(MyListFriends,TransactionsList -- [Lista_di_transazioni], Chain)
								 
							end;	

        				false -> 	loop(MyListFriends, TransactionsList, MyChain)
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
        	{Id_block, Id_prev, _, _} = Blocco,
        	io:format("Receive previous Block ~p ~n", Id_block),

            [_ | Blocco] = MyChain,
            io:format("Previous Block added ~n"),

            case Id_prev =:= none of
                true -> io:format("Chain of ~p complete ~n",[self()]);
                false ->
                    Mess = {get_previous, self(), Nonce, Id_prev},
                    sendMsg(lists:nth(rand:uniform(length(MyListFriends)), MyListFriends), Mess)
            end,
        	loop(MyListFriends, TransactionsList, MyChain);

        {get_head, Mittente, Nonce} -> 
        	%io:format("Send the head of chain of ~p ~n ~n",[self()]),

        	Mess = {head, Nonce, lists:last(MyChain)},
        	sendMsg(Mittente, Mess),


        	loop(MyListFriends, TransactionsList, MyChain);

        {head, Nonce, Blocco} ->
        	%io:format("Get head of the Block ~p of process ~p -> Nonce : ~p ~n ~n",[Blocco,self(),Nonce]),

        	[Blocco | _ ] = MyChain,

        	loop(MyListFriends, TransactionsList, MyChain);

		{printT,NameNode} -> io:format("Transaction List of ~p : ~p ~n~n",[NameNode,TransactionsList]),
							loop(MyListFriends,TransactionsList,MyChain);

		{printC,NameNode} -> io:format("~p) Chain of ~p :~n ~p ~n~n",[NameNode,self(),MyChain]),
							 loop(MyListFriends,TransactionsList,MyChain);

		{printF,NameNode} -> io:format("Friends List of ~p : ~p ~n ~n",[NameNode,MyListFriends]),
							 loop(MyListFriends,TransactionsList,MyChain)				

			
	end.


% Chiedo ad un amico casuale la sua lista di amici

askToFriend(MyListFriends) ->
		
	Length = length(MyListFriends),

	case Length < ?MaxNumberOfFriends of
		true ->	
				Mess = {get_friends, self(), make_ref()},
				Friend = lists:nth(rand:uniform(length(MyListFriends)), MyListFriends),
				sendMsg(Friend, Mess);
				
		false -> MyListFriends
	end.


% Aggiungo un amico alla mia lista
addFriend(MyListFriends, NewFriends) ->
	
	case length(MyListFriends) < ?MaxNumberOfFriends of
		
        true -> case NewFriends of
                				
					[] ->   MyListFriends;
                
           			_ ->    Friend = lists:nth(rand:uniform(length(NewFriends)), NewFriends),

        					spawn(fun () -> watch(self(), Friend) end),
        					addFriend(MyListFriends ++ [Friend], NewFriends -- [Friend])
       			end;
        false -> MyListFriends
						
	end.


sendMsg(Sender, Msg) ->
			case rand:uniform(10) of 
				1 ->%Perdo il messaggio 
					lost;

				2 -> 
					%Mando due volte il messaggio
					Sender ! Msg,
					Sender ! Msg;
				
				_ -> Sender ! Msg
			end.
				
	

sendMsgToList(ListFriend,Msg) ->
	lists:foreach(fun (Friend) -> 
		case rand:uniform(10) of 
			1 ->%Perdo il messaggio 
				lost;

			2 -> %Mando due volte il messaggio
				 Friend ! Msg,
				 Friend ! Msg;

			_ -> Friend ! Msg
		end
	end,ListFriend).

searchBlock(Chain,Id) ->

    
    [H|T] = Chain,
	{ID, Precedente, _ , _ } = H,

	io:format("Precedente: ~p ~n~n" , [Precedente]),

	case ID =:= Id of
		true -> H;
		false -> searchBlock(T,Id)
	end.


searchTransactionInTheChain(Transazione,Chain)->
	case Chain of

		[] -> false;
		
		_ -> [H|T] = Chain,

			 {_, _, Lista_di_transazioni, _} = H,

			 case lists:member(Transazione,Lista_di_transazioni) of

				 true -> true;

				 false -> searchTransactionInTheChain(Transazione,T)

			 end
			 
	end.

searchOtherTransactionInTheChain(ListaTransazioni,Chain)->

	case length(ListaTransazioni) of 
		
		0 -> false; 

		_ -> [Head|Tail] = ListaTransazioni,

			 SearchT = searchTransactionInTheChain(Head,Chain),
			 
			 case SearchT of

			 true -> true;
			 		 
			
			 false->  searchOtherTransactionInTheChain(Tail,Chain)
				
			 end
	end.	

mineTransactions(TList,MyChain) ->
	case MyChain of

        [] -> 
			Soluzione = proof_of_work:solve({none, TList}),
        	Block = {make_ref(), none, TList, Soluzione},
			Block;
									
		_ ->
        	% Prendo la testa della mia catena
        	{Id, Prev, _, _} = lists:last(MyChain),
			Soluzione = proof_of_work:solve({Prev, TList}),
        	Block = {make_ref(), Id, TList, Soluzione},
			Block
    end.
 	
	

% Mi registro nella lista globale ed inizio
main(NameNode) -> 
	
  	register(NameNode ,self()),
 	global:register_name(NameNode,self()),
	loop([],[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTING CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendT(Dest,Payload) ->
    Dest ! {push, {make_ref(), Payload}},
    io:format("> Send ~p to ~p ~n~n",[Payload,Dest]).

test() ->
	
	TIME = 2,
    TIME_TO_TRANS = 3,
    spawn(teacher_node,main,[]), % teacher_node
    sleep(1),
    
    N1 = spawn(?MODULE,main,[n1]),
    io:format("~p -> ~p~n",["N1",N1]),
    sleep(TIME),
    
    N2 = spawn(?MODULE,main,[n2]),
    io:format("~p -> ~p~n",["N2",N2]),
    sleep(TIME),

    N3 = spawn(?MODULE,main,[n3]),
    io:format("~p -> ~p~n",["N3",N3]),
    sleep(TIME),
    
    N4 = spawn(?MODULE,main,[n4]),
    io:format("~p -> ~p~n~n",["N4",N4]),
	sleep(15),

	%N1 ! {printF,n1},
	%N2 ! {printF,n2},
	%N3 ! {printF,n3},
	%N4 ! {printF,n4},
	%sleep(10),
	% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %sleep(5),
    io:format("Testing Transaction...~n ~n"),
    sendT(N1,"  Transazione 1  "),
    sleep(TIME_TO_TRANS),
    sendT(N1,"  Transazione 2  "),
    sleep(TIME_TO_TRANS),
    sendT(N2,"  Transazione 3  "),
    sleep(TIME_TO_TRANS),
    sendT(N3,"  Transazione 4  "),
    sleep(TIME_TO_TRANS),
    sendT(N2,"  Transazione 5  "),
    sleep(TIME_TO_TRANS),
    sendT(N2,"  Transazione 6  "),

    io:format("End Transactions Send ~n ~n"),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %sleep(20),
    io:format("New Actors ~n~n"),
    N5 = spawn(?MODULE,main,[n5]),
    io:format("~p -> ~p~n",["N5",N5]),
    %sleep(20),
    sleep(TIME_TO_TRANS),
    sendT(N1,"  Transazione 7  "),
 
    io:format("~p -> ~p~n~n",["N1",N1]),
    io:format("~p -> ~p~n~n",["N2",N2]),
    io:format("~p -> ~p~n~n",["N3",N3]),
    io:format("~p -> ~p~n~n",["N4",N4]),
    io:format("~p -> ~p~n~n",["N5",N5]),
    N1 ! {printC,n1},
    N2 ! {printC,n2},
    N3 ! {printC,n3},    
    N4 ! {printC,n4},    
    N5 ! {printC,n5},
    sleep(50),

    sendT(N5,"  Transazione 8"),
    io:format("~p -> ~p~n~n",["N1",N1]),
    io:format("~p -> ~p~n~n",["N2",N2]),
    io:format("~p -> ~p~n~n",["N3",N3]),
    io:format("~p -> ~p~n~n",["N4",N4]),
    io:format("~p -> ~p~n~n",["N5",N5]),
    N1 ! {printC,n1},
    N2 ! {printC,n2},
    N3 ! {printC,n3},    
    N4 ! {printC,n4},    
    N5 ! {printC,n5},
    sleep(40),

    exit(N1, kill),
    sleep(20),

    sendT(N5,"  Transazione 9  "),
    io:format("Controllare che tutti hanno tutto~n"),
    
    % io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n",["N2",N2]),
    io:format("~p -> ~p~n",["N3",N3]),
    io:format("~p -> ~p~n",["N4",N4]),
    io:format("~p -> ~p~n",["N5",N5]),
    % N1 ! {printC},
    N2 ! {printC,n2},
    N3 ! {printC,n3},    
    N4 ! {printC,n4},    
    N5 ! {printC,n5},


    sleep(50),
    io:format("Ultima stampa~n~n"),

    % io:format("~p -> ~p~n",["N1",N1]),
    io:format("~p -> ~p~n~n",["N2",N2]),
    io:format("~p -> ~p~n~n",["N3",N3]),
    io:format("~p -> ~p~n~n",["N4",N4]),
    io:format("~p -> ~p~n~n",["N5",N5]),
    % N1 ! {printC},
    N2 ! {printC,n2},
    N3 ! {printC,n3},    
    N4 ! {printC,n4},    
    N5 ! {printC,n5},

    io:format("Finito~n~n").



