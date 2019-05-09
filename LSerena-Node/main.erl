-module(main).
-author("Luca Serena").
-export([main/0]).
-export([makeT/1]).
%-record(transaction, {idTransaction, payload}).
%-record(block, {idBlock, idPreviousBlock, transactions, solution}).
%-record(head_of_blocks, {head_blockID, leng}).  
% In questa versione non uso i record ma le tuple, che comunque fanno  riferimento alle strutture descritte sopra
% una Head è una coppia ID del blocco testa - lunghezza della catena

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
    %   io:format("Pong sent to  ~p~n",[Sender]),
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
        Sender !  {friends, Nonce, Nodes},
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

    {update, NewBlock} ->   
        case isBlockYet(Blocks, NewBlock) of % se NewBlock è già nella lista di blocchi
            false->      
            case  proof_of_work:check({element(2, NewBlock), element(3, NewBlock)}, element (4, NewBlock)) of
                true ->  io:format("New Block added ~p~n",[NewBlock]),               
                        comunicate_block(NewBlock, Nodes),
                        NewTransactions = lists:subtract(TransactionsList, element(3, NewBlock)),
                        Friend  = randomFriend(Nodes),  
                        Ref = make_ref(), 
                        case findBlockGivenId (Blocks, element(2, NewBlock)) of  %se il precedente è none aggiungo oltre a blocco una Head di lunghezza 1
                            none -> NewHead = {element(1, NewBlock), 1},                                                        
                                    io:format("Head added ~n"),
                                    loop(Nodes, NewTransactions, [NewBlock|Blocks], [NewHead|Heads], Nonces);
                            notfound->    %se non trovo il blocco precedente lo chiedo
                                    io:format("asking for previous block~n"),                       
                                    sendMessage (Friend, {get_previous, self(), Ref, element(2, NewBlock)}), 
                                    sendMessage (Friend, {get_head, self(), Ref}),
                                    loop(Nodes, NewTransactions, [NewBlock|Blocks], Heads, [Ref|Nonces]);        
                            Y ->   %se trovo il blocco presedente:
                                    case findHeadGivenId (Heads, element(1, Y)) of
                                            notfound ->%se non è una testa allora chiedo una testa, così sono sicuro di avere la catena più lunga
                                                       sendMessage (Friend, {get_head, self(), Ref}),
                                                       loop(Nodes, NewTransactions, [NewBlock|Blocks], Heads, [Ref|Nonces]);
                                            X ->       %se il blocco precedente è una testa allora NewBlock diventa la nuova testa
                                                       NewHead = {element(1, NewBlock), element(2, X) + 1},
                                                       UpdatedHeads = Heads -- [X],
                                                       io:format("Head substituted, now length is  ~p~n", [element(2, NewHead)]),
                                                       loop(Nodes, NewTransactions, [NewBlock|Blocks], [NewHead | UpdatedHeads], Nonces)
                                    end
                        end;
                false -> io:format("Block not valid ~n"),
                      loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
            end;
            true -> loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
        end;

    {previous, Nonce, NewBlock} -> 
        case isBlockYet(Blocks, NewBlock) of% %se il blocco è già conosciuto non lo si aggiunge
            true->  loop(Nodes, TransactionsList, Blocks, Heads, Nonces);
            false -> case lists:member(Nonce, Nonces) of 
                        true ->    
                            RdmElem = randomFriend(Nodes),
                            case element(2, NewBlock) /= none of 
                                true ->  
                                    case findBlockGivenId(Blocks, element (2, NewBlock)) == notfound of
                                        true -> sendMessage(RdmElem, {get_previous, self(), Nonce, element(2, NewBlock)});
                                        false -> ok 
                                    end;
                                false -> is_first
                            end,
                            io:format("Block received: ~p~n",[NewBlock]),
                            loop(Nodes, TransactionsList, [NewBlock | Blocks], Heads, Nonces);
                        false-> loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
                    end
        end;
        
    {get_previous, Sender, Nonce, PreviousID} ->
        Elem = findBlockGivenId(Blocks, PreviousID),
        case Elem  of
        	    notfound -> Ref = make_ref(),
        	               self() ! {add_nonce, Ref}, %se il nodo non ha il blocco che viene richiesto sarà lui stesso a richiederlo agli amici
        	               sendMessage (randomFriend(Nodes), {get_previous, self(), Ref, PreviousID});
              none     -> ok;                       %non ci sono altri blocchi da mandare
              X        -> sendMessage(Sender, {previous, Nonce, X})   %se trova il blocco lo manda
        end,
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {head, Nonce, NewBlock} ->
        case lists:member(Nonce, Nonces) of
            false -> loop (Nodes, TransactionsList, Blocks, Heads, Nonces);
            true ->  
                case isBlockYet(Blocks, NewBlock) of%case lists:member (NewBlock, Blocks) of
                        false -> RdmElem =  randomFriend(Nodes),
                                case element(2, NewBlock) of
                                        none -> is_first;
                                        X -> sendMessage(RdmElem, {get_previous, self(), Nonce, X})
                                end,
                                io:format("Head block received: ~p~n",[NewBlock]),
                                loop (Nodes, TransactionsList, [NewBlock | Blocks], Heads, Nonces);
                        true -> loop (Nodes, TransactionsList, Blocks, Heads, Nonces)
                end
        end;

    {get_head, Sender, Nonce} -> 
        ID = maximumLengthHead (Heads, none),
        Head = case ID of    %head conterrà il blocco testa della catena più lunga
          none -> findBlockGivenId(Blocks, none);
          Y    -> findBlockGivenId(Blocks, element(1, Y))
        end,
        case Head of
          none -> none;
          X    -> Sender! {add_head, Nonce, findHeadGivenId(Heads, element(1, ID))},
                  sendMessage (Sender, {head, Nonce, X}) %mando oltre al blocco, anche il riferimento alla testa
        end,
        loop (Nodes, TransactionsList, Blocks, Heads, Nonces);

    {add_nonce, Nonce} ->
        loop(Nodes, TransactionsList, Blocks, Heads, [Nonce | Nonces]);

    {add_head, Nonce, Head} ->
        case lists:member(Nonce, Nonces) of
           true ->  io:format("Head received and added: ~p~n",[Head]),
                    loop(Nodes, TransactionsList, Blocks, [Head| Heads], Nonces);
           false -> loop(Nodes, TransactionsList, Blocks, Heads, Nonces)
        end;
    
    % l'attore research comunica che il processo è terminato e ne viene chiamato uno nuovo, con la topologia aggiornata
    % se il node non conosce blocchi allora chiede agli amici (nel caso ne abbia) la testa di un blocco 
    {ask_friend} ->
        case length(Blocks) of
          0 -> case length (Nodes) of
                    0 -> ok;
                    _    -> Ref = make_ref (),
                            self() ! {add_nonce, Ref}, 
                            sendMessage (randomFriend(Nodes), {get_head, self(), Ref})
               end;
          _ -> ok
        end,
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
            Y -> element(1, Y)
        end,
        spawn (fun() -> mining(Self, NewTransactions, PrevId) end),
        loop (Nodes, NewTransactions, Blocks, Heads, Nonces)
  end.        

%funzione per restituire la testa della catena più lunga. Restituisce una Head e non un blocco
maximumLengthHead ([], Res) -> Res;

maximumLengthHead ([H|T], Res) ->
    case Res == none of
      true -> maximumLengthHead (T, H);
      false -> case element(2, H) > element(2, Res) of
                  true -> maximumLengthHead (T, H);
                  false -> maximumLengthHead (T, Res)
                 end
    end.

%trova un blocco in una lista di blocchi dato un certo Id.
% Se l'Id è none restituisce none, se non trova il blocco restituisce notfound, altrimenti restituisce il blocco
findBlockGivenId ([], Id) ->
    case Id == none of
      true -> none;
      false ->  notfound
    end;

findBlockGivenId ([H|T], Id) ->
    case Id == none of
      true -> none;
      false -> case element(1, H) == Id of
                   true -> H;
                   false -> findBlockGivenId(T, Id)
      end
    end.

%data una lista di blocchi restituisce true se il blocco viene trovato (ovvero se viene trovato un blocco con la stessa soluzione)
%È stato necessario fare questo e non confrontare semplicementi i blocchi perché blochi con stesse transazioni, ID precedente e quindi soluzione 
%possono avere ID diverso, in quanto frutto di diversi mining. 
isBlockYet ([], _ ) -> false;

isBlockYet ([H|T], Sol) ->
   case Sol of
   	none-> true;
   	Y  ->
         case element(4, H) == element (4, Y) of
            true  -> true;
            false -> isBlockYet (T, Sol)
         end
   end.

%Per comunicare le transazioni agli amici
add_transaction (_, []) -> ok;

add_transaction (Trans, [H|T]) ->
    H ! {push, Trans},
    add_transaction (Trans, T).

%Per comunicare i blocchi agli amici
comunicate_block (_, []) -> ok;

comunicate_block (Block, [H|T]) ->
    H ! {update, Block},
    comunicate_block (Block, T).

%Vengono aggiunti amici a meno che non se ne abbiano già 3
addNode(Nodes, []) ->  Nodes;

addNode(Nodes, [H | T]) ->
 case length (Nodes) >= 3 of 
      true -> Nodes;
      false ->
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
          end
  end.

%restituisce una Head (oppure notfound) dato un certo ID
findHeadGivenId ([], _) ->
    notfound;

findHeadGivenId ([H|T], Id) ->
    case element(1, H) == Id of
      true -> H;
      false -> findHeadGivenId(T, Id)
    end.

%un messaggio su 10 viene perso e uno su 10 viene spedito 2 volte
sendMessage (Recipient, Content) ->
    Behaviour = rand:uniform(10),
    case Behaviour of
      1 -> Recipient ! {Content},
             Recipient ! {Content};
        2 ->  io:format("Message Lost ~p~n", [Content]);
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
            NewBlock = {make_ref(),  IdPrev, ToMine, Solution},
            io:format("block mined...~n"), 
            MyNode ! {update, NewBlock},
            MyNode ! {ask_transactions, ToMine}
    end.

%restituisce un nodo a caso tra gli amici o il nodo professore nel caso in cui non si abbiano amici
randomFriend (Nodes) -> 
  case length (Nodes) of 
  	0 -> {teacher_node, docente@localhost} ;
  	_ -> IndexNode = rand:uniform(length(Nodes)),
         lists:nth(IndexNode, Nodes)
   end.

%lo scopo di questo attore è, ogni 5 secondi, controllare che la topologia sia a posto. Se ci sono meno di 3 amici viene chiesto ad un altro
%amico oppure al professore di fornire una lista di amici.
research (Nodes, Self)  ->
    sleep (5),
    NUM = length(Nodes) + 1,
    RDM = rand:uniform(NUM),
    case length (Nodes) >= 3 of
         true -> ok;
         false -> Ref = make_ref(),
                  nodeLS ! {add_nonce, Ref},
                  case RDM == NUM of 
                     true -> global:send(teacher_node, {get_friends, Self, Ref});
                              %{teacher_node, teacher@localhost} ! {get_friends, Self, Ref};
                     false -> lists:nth (RDM, Nodes) ! {get_friends, Self, Ref}     
                end
    end,
    nodeLS ! {ask_friend}.

%funzione che consente a un nodo di fare una transazione. Una transazione è composta da ID e Payload (l'input fornito dall'utente-nodo)
makeT (Payload) -> 
       NewT = {make_ref(), Payload},
       nodeLS ! {push, NewT}.       

main() ->
    Self = self(),
    register(nodeLS, self()),
    spawn(fun () -> research ([], Self) end),
    spawn(fun () -> mining (Self, [], none) end),
    Ref = make_ref(),
    global:send(teacher_node, {get_friends, Self, Ref}),
    %{teacher_node, teacher@localhost} ! {get_friends, self(), Ref},
    loop([],[],[],[], [Ref]).
    %net_adm:ping('docente@localhost'). spawn (fun() -> main:main() end).