-module(node).
-export([main/0]).
-record(transaction, {idTransaction, payload}).
-record(block, {idBlock, idPreviousBlock, transactions, solutions}).

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 -> Main ! {dead, Node}
  end.

loop(Nodes, TransactionsList, Blocks) ->

  receive
    {ping, Sender, Ref} ->
       Sender ! {pong, Ref} ,
        case lists:member(Sender,Nodes) of
        	true-> loop (Nodes, TransactionsList, Blocks);
        	false -> loop ([Sender|Nodes], TransactionsList, Blocks)
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
       loop(New_Nodes, TransactionsList, Blocks);

    {dead, Node} ->
       io:format("Dead node ~p~n",[Node]),
       case length(Nodes) of
            0 ->  
                Refer = make_ref(),
                teacher_node ! {get_friends, self(), Refer};
            X when X < 3 ->
                   Refer = make_ref(),
                   Index = rand:uniform(length(Nodes)),
	               Elem = lists:nth(Index,Nodes),
                   Elem ! {get_friends, self(), Refer}
               end,
       loop(Nodes -- [Node], TransactionsList, Blocks);

    {get_friends, Sender, Nonce} ->
        Sender ! {friends, Nonce, Nodes},
        loop (Nodes, TransactionsList, Blocks);

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
    	loop(Nodes, TransactionsList, Blocks);

    {update,  NewBlock} ->
        case lists:member(NewBlock, Blocks) of
        	 false->      
            case  proof_of_work:check({NewBlock#block.idPreviousBlock, NewBlock#block.transactions}, NewBlock#block.solutions) of
        	    true ->
            	         io:format("New Block added ~p~n",[NewBlock]),
        	             loop (Nodes, TransactionsList, [NewBlock|Blocks]);
            	false -> io:format("Block not valid ~n"),
        	             loop(Nodes, TransactionsList, Blocks)
            end      
        end; 

    {previous,  Nonce, NewBlock} ->
        IndexNode = rand:uniform(length(Nodes)),
	    RdmElem = lists:nth(IndexNode, Nodes),
        case NewBlock#block.idPreviousBlock == none of
        	false ->  RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock}
        end,
        loop(Nodes, TransactionsList, [Blocks|NewBlock]);

    {get_previous, Sender, Nonce, PreviousID} ->
           Elem = findBlockGivenId(Blocks, PreviousID),
           case Elem == none of
           	    false -> Sender ! {previous, Nonce, Elem}
           end,
           loop (Nodes, TransactionsList, Blocks);

    {head, Nonce, NewBlock} ->
        IndexNode = rand:uniform(length(Nodes)),
	    RdmElem = lists:nth(IndexNode, Nodes),
        case NewBlock == none of
            true -> loop (Nodes, TransactionsList, Blocks);
            false -> case NewBlock#block.idPreviousBlock == none of
            	          false  -> RdmElem ! {get_previous, self(), Nonce, NewBlock#block.idPreviousBlock}	
            	     end,
                    loop (Nodes, TransactionsList, [NewBlock|Blocks])
        end;

    {get_head, Sender, Nonce} ->
        Head = lists:nth(1, Blocks),
        Sender ! {head, Nonce, Head},
        loop (Nodes, TransactionsList, Blocks)

  end.         


findBlockGivenId ([], _) ->
    none;

findBlockGivenId ([Block|Tail], Id) ->
    case Block#block.idBlock == Id of
     	true -> Block;
     	false -> findBlockGivenId(Tail, Id)
    end.

sendMessage (Recipient, Content) ->
    Behaviour = rand:uniform(10),
    case Behaviour of
    	1 -> Recipient ! {Content},
             Recipient ! {Content};
        X when X > 2 -> Recipient ! {Content}
    end.

mining (Transactions, IdPreviousBlock) ->
    Solution = proof_of_work:solve({IdPreviousBlock, Transactions}),
    My_Pid ! {make_ref(), IdPreviousBlock, Transactions, Solution}.
      


main() ->
    My_Pid = spawn (node, loop, []),
    {teacher_node, nonode@nohost} ! {get_friends, self(), make_ref()}.
