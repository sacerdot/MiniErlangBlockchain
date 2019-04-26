-module(node).
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
      %sendMessage(Sender, {pong, Ref}),
       io:format("Pong sent to  ~p~n",[Sender]),
       loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {friends, Nonce, New_Nodes} ->
      %  io:format("nodes-received  ~p~p~n",[length(Nodes), length(New_Nodes)]),
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
    	    false ->  io:format ("leng ~p~p~n", [length(Nodes), Nodes]),
    	              add_transaction(NewTransaction, Nodes),
    	             io:format ("NewTransaction~n"),
    	             [NewTransaction | TransactionsList]
    	    end,
    	loop(Nodes, NewTransactionsList, Blocks, Heads, Nonces);

    {update,  NewBlock} ->    %still need to delete transactions in the block
        UpdatedBlocks = Blocks,
        UpdatedHeads = Heads,
        case lists:member(NewBlock, Blocks) of
        	 false->      
            case  proof_of_work:check({NewBlock#block.idPreviousBlock, NewBlock#block.transactions}, NewBlock#block.solution) of
        	    true ->  io:format("New Block added ~p~n",[NewBlock]),
        	             case findBlockGivenId (Blocks, NewBlock#block.idPreviousBlock) == notfound of  %if the previous is not none and is missing I ask for previous
            	              true->  Ref = make_ref(),
                                      Nonces = [Nonces | Ref],
            	                      randomFriend(Nodes) ! {get_previous, self(), Ref, NewBlock#block.idPreviousBlock};
                              %if the block is added
                              false -> UpdatedBlocks = [NewBlock | Blocks],
                                        case findHeadGivenId (Heads, NewBlock#block.idPreviousBlock) of
                         	                 notfound -> NewHead = #head_of_blocks{head_blockID = NewBlock#block.idBlock, leng = 1},
                         	                             UpdatedHeads = [NewHead | Heads];
                         	                 X ->        NewHead = #head_of_blocks{head_blockID = NewBlock#block.idBlock, leng = X#head_of_blocks.leng + 1},
                         	                             UpdatedHeads = [NewHead| Heads],
                         	                             UpdatedHeads = UpdatedHeads -- [X]
                                        end
                        end;
               	false -> io:format("Block not valid ~n")
            end;
            true -> present_yet
        end,
        loop(Nodes, TransactionsList, UpdatedBlocks, UpdatedHeads, Nonces);
        
    {previous, Nonce, NewBlock} ->
        case lists:member(Nonce, Nonces) of
        	false -> nonce_not_found;
            true ->     
	              RdmElem = randomFriend(Nodes),
                  case NewBlock#block.idPreviousBlock /= none of 
        	           true ->  
        	                 case findBlockGivenId(Blocks, NewBlock#block.idPreviousBlock) == notfound of
        	      	             true -> RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock};
        	      	             false -> ok
        	                 end;
        	      false -> is_first
                  end
        end,
        loop(Nodes, TransactionsList, [Blocks|NewBlock], Heads, Nonces);

    {get_previous, Sender, Nonce, PreviousID} ->
        case lists:member(Nonce, Nonces) of
             false -> nonce_not_found;
             true ->
                     Elem = findBlockGivenId(Blocks, PreviousID),
                     case Elem == notfound of
           	              false -> Sender ! {previous, Nonce, Elem};
           	              true -> notfound
                     end
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
        case lists:member(Nonce, Nonces) of
        	false -> nonce_not_found;
        	true ->
                    ID = maximumLengthHead (Heads),
                    Head = findBlockGivenId(Blocks, ID),
                    Sender ! {head, Nonce, Head}
        end,
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {add_nonce, Nonce} ->
         loop(Nodes, TransactionsList, Blocks, Heads, [Nonce | Nonces]);

    {ask_friend} ->
        Sel = self(),
        spawn(fun () -> research(Nodes, Sel) end),
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {add_transaction, Payload} ->
          NewTransaction = #transaction{idTransaction = make_ref(), payload = Payload},
          lists:foreach(fun(N) -> 
          	N ! {push, NewTransaction}
          	end, Nodes),
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces)
  end.        

maximumLengthHead (Heads) ->
       MaxLeng = 0,
       Elem = none,
       lists:foreach(fun(N) ->
       	                case N#head_of_blocks.leng > MaxLeng of
       		                 true -> Elem = N#head_of_blocks.head_blockID,
       		                         MaxLeng = N#head_of_blocks.leng;
       		                 false -> ok
                        end                
                    end, Heads),
       Elem.


findBlockGivenId ([], _) ->
    notfound;

findBlockGivenId ([Block|Tail], Id) ->
    case Id of
    	none -> none;
    	X -> case Block#block.idBlock == Id of
     	          true -> Block;
     	          false -> findBlockGivenId(Tail, Id)
     	end
    end.

add_transaction (Trans, []) -> ok;

add_transaction (Trans, [H|T]) ->
    H ! {push, Trans},
    add_transaction (Trans, T).

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

findHeadGivenId ([H|Tail], Id) ->
    case H#head_of_blocks.head_blockID == Id of
     	true -> H;
     	false -> findBlockGivenId(Tail, Id)
    end.

sendMessage (Recipient, Content) ->
    Behaviour = rand:uniform(10),
    case Behaviour of
    	1 -> Recipient ! {Content},
             Recipient ! {Content};
        2 ->  io:format("Message Lost~n");
        X when X > 2 ->    Recipient ! Content
    end.

mining (MyNode, Transactions, IdPrev) ->
    Solution = proof_of_work:solve({IdPrev, Transactions}),
    NewBlock = #block{idBlock = make_ref(), idPreviousBlock = IdPrev, transactions = Transactions, solution = Solution},
    MyNode ! {update, NewBlock}.
      

randomFriend (Nodes) -> 
        IndexNode = rand:uniform(length(Nodes)),
	    lists:nth(IndexNode, Nodes).

research (Nodes, Self)  ->
    sleep (3),
  %  io:format("numero nodi ~p~n",[length(Nodes)]),
    NUM = length(Nodes) + 1,
    RDM = rand:uniform(NUM),
    case length (Nodes) of
      	 3 -> ok;
       	 Y -> 
       	        Ref = make_ref(),
       	        nodeLS ! {add_nonce, Ref},
       	        case RDM of 
       	           	 NUM -> 
       	           	      {teacher_node, teacher@localhost} ! {get_friends, Self, Ref};
       	           	 X   -> 
       	                 lists:nth (RDM, Nodes) ! {get_friends, Self, Ref}                   
       	        end
    end,
    nodeLS ! {ask_friend}.

makeT (Payload) -> 
       NewT = #transaction{idTransaction = make_ref(), payload = Payload},
       nodeLS ! {push, NewT}.

       

main() ->
    Self = self(),
    register(nodeLS, self()),
    spawn(fun () -> research([], Self) end),
    Ref = make_ref(),
    {teacher_node, teacher@localhost} ! {get_friends, self(), Ref},
    loop([],[],[],[], [Ref]).
    %spawn (fun() -> node:main() end).
