-module(node).
-export([main/0]).
-record(transaction, {idTransaction, payload}).
-record(block, {idBlock, idPreviousBlock, transactions, solutions}).
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

loop(Nodes, TransactionsList, Blocks, Heads) ->
  receive
    {ping, Sender, Ref} ->
       Sender ! {pong, Ref} ,
       io:format("Pong sent to  ~p~n",[Sender]),
        case lists:member(Sender,Nodes) of %or Sender == {teacher_node, teacher@localhost} of
        	true-> loop (Nodes, TransactionsList, Blocks, Heads);
        	false -> loop ([Sender|Nodes], TransactionsList, Blocks, Heads)
        end;

    {friends, Nonce, New_Nodes} ->
          IndexNewNode = rand:uniform(length(New_Nodes)),
	      NewElem = lists:nth(IndexNewNode,New_Nodes),
          case lists:member(NewElem,Nodes) of
             true -> Nodes;
             false ->
                io:format("New node ~p~n",[NewElem]),
                Self = self(),
                spawn(fun () -> watch(Self,NewElem) end),
                [NewElem|Nodes]
          end,
       loop(New_Nodes, TransactionsList, Blocks, Heads);

    {dead, Node} ->
       io:format("Dead node ~p~n",[Node]),
       case length(Nodes) of
            0 ->  
                Refer = make_ref(),
                teacher_node ! {get_friends, self(), Refer};
            X when X < 3 ->
                   Refer = make_ref(),
	               Elem =  randomFriend(Nodes),
                   Elem ! {get_friends, self(), Refer}
               end,
       loop(Nodes -- [Node], TransactionsList, Blocks, Heads);

    {get_friends, Sender, Nonce} ->
        Sender ! {friends, Nonce, Nodes},
        loop (Nodes, TransactionsList, Blocks, Heads);

    {push, NewTransaction} ->
    	NewTransactionsList = case lists:member(NewTransaction, TransactionsList) of
    		true ->
    	        lists:foreach(fun(N) ->
                N ! {push, NewTransaction}
                    end, Nodes),
    			TransactionsList;
    	    false ->
    	         [NewTransaction | TransactionsList]
    	         end,
    	loop(Nodes, TransactionsList, Blocks, Heads);

    {update,  NewBlock} ->    %still need to delete transactions in the block
        UpdatedBlocks = Blocks,
        UpdatedHeads = Heads,
        case lists:member(NewBlock, Blocks) of
        	 false->      
            case  proof_of_work:check({NewBlock#block.idPreviousBlock, NewBlock#block.transactions}, NewBlock#block.solutions) of
        	    true ->  io:format("New Block added ~p~n",[NewBlock]),
        	             case findBlockGivenId (Blocks, NewBlock#block.idPreviousBlock) == notfound of  %if the previous is not none and is missing I ask for previous
            	              true-> randomFriend(Nodes) ! {get_previous, self(), make_ref(), NewBlock#block.idPreviousBlock};
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
            end
        end,
        loop(Nodes, TransactionsList, UpdatedBlocks, UpdatedHeads);
        

    {previous, Nonce, NewBlock} ->
	    RdmElem = randomFriend(Nodes),
        case NewBlock#block.idPreviousBlock == none of
        	false ->  RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock}
        end,
        loop(Nodes, TransactionsList, [Blocks|NewBlock], Heads);

    {get_previous, Sender, Nonce, PreviousID} ->
           Elem = findBlockGivenId(Blocks, PreviousID),
           case Elem == notfound of
           	    false -> Sender ! {previous, Nonce, Elem}
           end,
           loop (Nodes, TransactionsList, Blocks, Heads);

    {head, Nonce, NewBlock} ->
        IndexNode = rand:uniform(length(Nodes)),
	    RdmElem = lists:nth(IndexNode, Nodes),
        case NewBlock == none of
            true -> loop (Nodes, TransactionsList, Blocks, Heads);
            false -> case NewBlock#block.idPreviousBlock == none of
            	          false  -> RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock}	
            	     end,
                    loop (Nodes, TransactionsList, [NewBlock|Blocks], Heads)
        end;

    {get_head, Sender, Nonce} -> 
        ID = maximumLengthHead (Heads),
        Head = findBlockGivenId(Blocks, Nodes),
        Sender ! {head, Nonce, Head},
        loop (Nodes, TransactionsList, Blocks, Heads)
  end.         

maximumLengthHead (Heads) ->
       MaxLeng = 0,
       Elem = none,
       lists:foreach(fun(N) ->
       	                case N#head_of_blocks.leng > MaxLeng of
       		                 true -> Elem = N#head_of_blocks.head_blockID,
       		                         MaxLeng = N#head_of_blocks.leng 
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
        X when X > 2 -> Recipient ! {Content}
    end.

mining (MyNode, Transactions, IdPreviousBlock) ->
    Solution = proof_of_work:solve({IdPreviousBlock, Transactions}),
    MyNode ! {make_ref(), IdPreviousBlock, Transactions, Solution}.
      

randomFriend (Nodes) -> 
        IndexNode = rand:uniform(length(Nodes)),
	    lists:nth(IndexNode, Nodes).


main() ->
    register(node1, self()),
    global:register_name(node1,self()),
    register(stateLoop,
           spawn(fun () -> loop([], [], [], []) end)),
        {teacher_node, teacher@localhost} ! {get_friends, self(), make_ref()},
    loop([],[],[],[]).

