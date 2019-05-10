-module(teacher_client).
-export([main/0,test1/0,test2/0]).

get_friends() ->
 Nonce = make_ref(),
 global:send(teacher_node,{get_nodes, self(), Nonce}),
 receive {nodes,Nonce,L} -> L end
.

push_trans(Node,Trans) ->
 io:format("invio ~p a ~p~n", [Trans,Node]),
 Node ! {push, Trans}
 .

print_chain(Node,Block) ->
 case Block of
   {ID, Prev, Trans, _} ->
    io:format("   blocco ~p~n", [ID]),
    [ io:format("   transazione ~p~n", [T]) || T <- Trans],
    case Prev =:= none of
      true -> ok
    ; false ->
       Nonce = make_ref(),
       Node ! {get_previous, self(), Nonce, Prev},
       receive
          {previous, Nonce, N} -> print_chain(Node,N)
       after 5000 -> io:format("nessuna risposta da ~p~n", [Node])
       end
    end
  ; _ -> io:format("FORMATO DEL BLOCCO NON RICONOSCIUTO: ~p~n", [Block])
 end
 .

print_chain(Node) ->
 io:format("chain di ~p~n", [Node]),
 Nonce = make_ref(),
 Node ! {get_head, self(), Nonce},
 receive
   {head, Nonce, none} ->
     io:format("~n",[])
 ; {head, Nonce, Head} ->
     print_chain(Node,Head),
     io:format("~n",[])
 after 5000 -> io:format("nessuna risposta da ~p~n", [Node])
 end
.

test1() ->
 Friends = get_friends(),
 Trans = [ {make_ref(),"transaction"++integer_to_list(N)}|| N <- lists:seq(1,15) ],
 PT = fun(F) ->
   [ push_trans(F,T) || T <- Trans ],
   [ push_trans(F,T) || T <- Trans ]
  end,
 [ PT(F) || F <- Friends ],
 receive after 5000 -> true end,
 [ print_chain(F) || F <- Friends ]
 .

fake_node(B1,R1,B2) ->
  io:format("fake_node ready~n"),
  receive
    {get_head,Sender,Nonce} ->
      io:format("fake_node get_head da ~p~n",[Sender]),
      Sender ! {head, Nonce, B2},
      fake_node(B1,R1,B2)
  ; {get_previous,Sender,Nonce,R1} ->
      io:format("fake_node get_previous da ~p~n",[Sender]),
      Sender ! {previous, Nonce, B1},
      fake_node(B1,R1,B2)
  ; {get_previous,Sender,_,_} ->
      io:format("fake_node BAD get_previous da ~p~n",[Sender]),
      fake_node(B1,R1,B2)
  ; Msg ->
      io:format("fake_node unexpected message ~p~n",Msg),
      fake_node(B1,R1,B2)
  end
.

test2() ->
 R1 = make_ref(),
 TL1 = [{make_ref(),"T1"}],
 PB1 = {none, TL1},
 Solve1 = proof_of_work:solve(PB1),
 B1 = {R1, none, TL1, Solve1},

 R2 = make_ref(),
 TL2 = [{make_ref(),"T2"}],
 PB2 = {R1, TL2},
 Solve2 = proof_of_work:solve(PB2),
 B2 = {R2, R1, TL2, Solve2},

 Friends = get_friends() ,
 case get_friends() of
    [F|_] ->
      io:format("seleziono amico ~p~n",[F]),
      G = spawn(fun () -> fake_node(B1,R1,B2) end),
      io:format("update della chain a ~p~n",[F]),
      F ! {update, G, B2 },
      receive after 5000 -> true end,
      [ print_chain(H) || H <- Friends ]
  ; _ -> io:format("Non ci sono amici per il test~n")
 end
 .

main() ->
 io:format("--- Test2~n"),
 test2(),
 io:format("--- Test1~n"),
 test1()
.
