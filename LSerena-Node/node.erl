-module(node).
-author("Luca Serena").
-export([main/0]).
-export([makeT/1]).
-record(transaction, {idTransaction, payload}).
-record(block, {idBlock, idPreviousBlock, transactions, solution}).
-record(head_of_blocks, {head_blockID, leng}).

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 -> Main ! {dead, Node}
  end.

loop(Nodes, TransactionsList, Blocks, Heads, Nonces) ->
  receive
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref},
       io:format("Pong sent to  ~p~n",[Sender]),
       loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {friends, Nonce, New_Nodes} ->
        New_Nonces = Nonces,
        case lists:member(Nonce, Nonces) of
       	    true -> Friends = addNode (Nodes, New_Nodes),
       	              loop(Friends, TransactionsList, Blocks, Heads, New_Nonces);
            false -> io:format("Nonce not found ~n"),
                     loop(Nodes, TransactionsList, Blocks, Heads, New_Nonces)
        end;

    {dead, Node} ->
       io:format("Dead node ~p~n",[Node]),
       loop(Nodes -- [Node], TransactionsList, Blocks, Heads, Nonces);

    {get_friends, Sender, Nonce} ->
        Sender ! {friends, Nonce, Nodes},
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {push, NewTransaction} ->
    	NewTransactionsList = case lists:member(NewTransaction, TransactionsList) of
    		true ->
    	        TransactionsList;
    	    false -> add_transaction(NewTransaction, Nodes),
    	             io:format ("NewTransaction~n"),
    	             [NewTransaction | TransactionsList]
    	    end,
    	loop(Nodes, NewTransactionsList, Blocks, Heads, Nonces);

    {update, NewBlock} ->    %still need to delete transactions in the block
        case lists:member(NewBlock, Blocks) of
        	false->      
            case  proof_of_work:check({NewBlock#block.idPreviousBlock, NewBlock#block.transactions}, NewBlock#block.solution) of
        	    true ->  io:format("New Block added ~p~n",[NewBlock]),
                         comunicate_block(NewBlock, Nodes),
        	             NewTransactions = lists:subtract(TransactionsList, NewBlock#block.transactions),
        	             case findBlockGivenId (Blocks, NewBlock#block.idPreviousBlock) of  %if the previous is not none and is missing I ask for previous
            	              none -> NewHead = #head_of_blocks{head_blockID = NewBlock#block.idBlock, leng = 1},                         	                             
            	                      io:format("Head added ~n"),
            	                      loop(Nodes, NewTransactions, [NewBlock|Blocks], [NewHead|Heads], Nonces);
            	              notfound->  Ref = make_ref(),  
                	                  io:format("asking for previous block~n"),    	                       
            	                      randomFriend(Nodes) ! {get_head, self(), Ref},
            	                      loop(Nodes, NewTransactions, [NewBlock|Blocks], Heads, [Ref|Nonces]);        
                              Y ->  %if the previous block is found
                                    case findHeadGivenId (Heads, Y#block.idBlock) of
                         	                 notfound ->% NewHead = #head_of_blocks{head_blockID = NewBlock#block.idBlock, leng = 1},
                         	                             io:format("Head not found ~n"),
                         	                             loop(Nodes, NewTransactions, [NewBlock|Blocks], Heads, Nonces);
                         	                 X ->        NewHead = #head_of_blocks{head_blockID = NewBlock#block.idBlock, leng = X#head_of_blocks.leng + 1},
                         	                             UpdatedHeads = Heads -- [X],
                         	                             io:format("Head substituted ~n"),
                         	                             loop(Nodes, NewTransactions, [NewBlock|Blocks], [NewHead | UpdatedHeads], Nonces)
                                    end
                        end;
               	false -> io:format("Block not valid ~n"),
                    	loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
            end;
            true -> loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
        end;

    {previous, Nonce, NewBlock} ->
        case lists:member(Nonce, Nonces) of
        	false -> loop(Nodes, TransactionsList, Blocks, Heads, Nonces);
            true ->     
	              RdmElem = randomFriend(Nodes),
                  case NewBlock#block.idPreviousBlock /= none of 
        	           true ->  
        	                 case findBlockGivenId(Blocks, NewBlock#block.idPreviousBlock) == notfound of
        	      	             true -> RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock};
        	      	             false -> ok %gli dovrei dire: aggiorna la lunghezza di della head
        	                 end;
        	            false -> is_first
                  end,
                  loop(Nodes, TransactionsList, [Blocks|NewBlock], Heads, Nonces)
        end;
        
    {get_previous, Sender, Nonce, PreviousID} ->
        Elem = findBlockGivenId(Blocks, PreviousID),
         case Elem == notfound of
    	        false -> Sender ! {previous, Nonce, Elem}; %sendMessage(Sender, {previous, Nonce, Elem});
       	        true -> notfound
        end,
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {head, Nonce, NewBlock} ->
        case lists:member(Nonce, Nonces) of
        	false -> nonce_not_found;
        	true ->
	         RdmElem =  randomFriend(Nodes),
             case NewBlock == none of
                 true -> loop (Nodes, TransactionsList, Blocks, Heads, Nonces);
                 false -> case NewBlock#block.idPreviousBlock == none of
                 	          false  -> RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock};
                 	          true ->is_first
            	          end,
            	          Blocks = [NewBlock | Blocks]
              end
        end,
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {get_head, Sender, Nonce} -> 
        ID = maximumLengthHead (Heads, none),
        Head = findBlockGivenId(Blocks, ID),
        sendMessage (Sender, {head, Nonce, Head}),  % Sender ! {head, Nonce, Head}, 
        Sender! {add_head, Nonce, findHeadGivenId(Heads, ID)},
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {add_nonce, Nonce} ->
         loop(Nodes, TransactionsList, Blocks, Heads, [Nonce | Nonces]);

    {add_head, Nonce, Head} ->
        case lists:member(Nonce, Nonces) of
        	 true ->  loop(Nodes, TransactionsList, Blocks, [Head| Heads], Nonces);
             false -> loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
        end;
    
    % l'attore research comunica che il processo è terminato e ne viene chiamato uno nuovo, con la topologia aggiornata
    {ask_friend} ->
        Sel = self(),
        spawn(fun () -> research(Nodes, Sel) end),
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    %l'attore mining comunica che il processo è terminato e ne viene chiamato uno nuovo, con le transazioni da minare aggiornate
    {ask_transactions, Mined} ->
        NewTransactions = lists:subtract (TransactionsList, Mined),
        Self = self(),
        Prev = maximumLengthHead(Heads, none),
        PrevId = case Prev of 
          	none -> none;
           	Y -> Y#head_of_blocks.head_blockID
        end,
        spawn (fun() -> mining(Self, NewTransactions, PrevId) end),
        loop (Nodes, NewTransactions, Blocks, Heads, Nonces)
  end.        

maximumLengthHead ([], Res) -> Res;

maximumLengthHead ([H|T], Res) ->
    case Res == none of
    	true -> maximumLengthHead (T, H);
    	false -> case H#head_of_blocks.leng > Res#head_of_blocks.leng of
    		          true -> maximumLengthHead (T, H#head_of_blocks.head_blockID);
    		          false -> maximumLengthHead (T, Res)
                 end
    end.

findBlockGivenId ([], Id) ->
    case Id == none of
    	true -> none;
    	false ->  notfound
    end;

findBlockGivenId ([H|T], Id) ->
    case Id == none of
    	true -> none;
    	false -> case H#block.idBlock == Id of
     	             true -> H;
     	             false -> findBlockGivenId(T, Id)
     	end
    end.

add_transaction (_, []) -> ok;

add_transaction (Trans, [H|T]) ->
    H ! {push, Trans},
    add_transaction (Trans, T).

comunicate_block (_, []) -> ok;

comunicate_block (Block, [H|T]) ->
    H ! {update, Block},
    comunicate_block (Block, T).

addNode(Nodes, []) ->  Nodes;

addNode(Nodes, [H | T]) ->
 Self = self(),
  case self() /= H of
  	  true ->
            case lists:member(H, Nodes) of
                 true -> addNode(Nodes, T);
                 false ->
                        io:format("New Friend ~p~n", [H]),
                        spawn (fun() -> watch(Self, H) end),
                        addNode([H | Nodes], T)
            end;
      false -> addNode(Nodes, T)
  end.

findHeadGivenId ([], _) ->
    notfound;

findHeadGivenId ([H|T], Id) ->
    case H#head_of_blocks.head_blockID == Id of
     	true -> H;
     	false -> findBlockGivenId(T, Id)
    end.

%un messaggio su 10 viene perso e uno su 10 viene spedito 2 volte
sendMessage (Recipient, Content) ->
    Behaviour = rand:uniform(10),
    case Behaviour of
    	1 -> Recipient ! {Content},
             Recipient ! {Content};
        2 ->  io:format("Message Lost~n");
        X when X > 2 ->    Recipient ! Content
    end.

%attore che si occupa di minare transazioni. Aspetta di avere in input almeno due transazioni per minare e non ne mina più di 10 alla volta
mining (MyNode, Transactions, IdPrev) ->
    case length (Transactions) of
        X when X < 2 ->  
            sleep(5), %non sovraccaricare di richieste al processo main quando non ci sono transazioni
            MyNode ! {ask_transactions, []};
        Y  -> 
            io:format("start mining...~n"),
            ToMine = case Y > 10 of 
               	 true -> lists:sublist (Transactions, 10);
               	 false -> Transactions
            end,
            Solution = proof_of_work:solve({IdPrev, ToMine}),
            NewBlock = #block{idBlock = make_ref(), idPreviousBlock = IdPrev, transactions = ToMine, solution = Solution},
            io:format("block mined...~n"), 
            MyNode ! {update, NewBlock},
            MyNode ! {ask_transactions, ToMine}
    end.

randomFriend (Nodes) -> 
        IndexNode = rand:uniform(length(Nodes)),
	    lists:nth(IndexNode, Nodes).
 
%lo scopo di questo attore è, ogni 5 secondi, controllare che la topologia sia a posto. Se ci sono meno di 3 amici viene chiesto ad un altro
%amico oppure al professore di fornire una lista di amici.
research (Nodes, Self)  ->
    sleep (5),
    NUM = length(Nodes) + 1,
    RDM = rand:uniform(NUM),
    case length (Nodes) >= 3 of
      	 true -> ok;
       	 false -> 
       	        Ref = make_ref(),
       	        nodeLS ! {add_nonce, Ref},
       	        case RDM == NUM of 
       	           	 true -> {teacher_node, teacher@localhost} ! {get_friends, Self, Ref};
       	           	 false -> lists:nth (RDM, Nodes) ! {get_friends, Self, Ref}     
       	        end
    end,
    nodeLS ! {ask_friend}.

%funzione che consente a un nodo di fare una transazione. Una transazione è composta da ID e Payload (l'input fornito dall'utente-nodo)
makeT (Payload) -> 
       NewT = #transaction{idTransaction = make_ref(), payload = Payload},
       nodeLS ! {push, NewT}.       

main() ->
    Self = self(),
    register(nodeLS, self()),
    spawn(fun () -> research ([], Self) end),
    spawn(fun () -> mining (Self, [], none) end),
    Ref = make_ref(),
    {teacher_node, teacher@localhost} ! {get_friends, self(), Ref},
    loop([],[],[],[], [Ref]).
    %spawn (fun() -> node:main() end).
    %TODO: garbage collection:
    %                         - togliere le heads che non hanno percorso completo da testa a primo blocco
    %                         - togliere heads che hanno una lunghezza < 2 rispetto alla head principale
    %                         - togliere i blocchi che non fanno parte di catena attive

