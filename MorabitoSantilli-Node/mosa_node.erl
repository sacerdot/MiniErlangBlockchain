-module(mosa_node).
-author("Andrea Morabito, Paolo Santilli").
-export([main/1,test/0]).
-define(MaxNumberOfFriends, 3).
-define(TimeSleep, 2).
-define(TeacherNodeUrl, 'C:/Users/paolo.santilli2/Desktop/Unibo/Emerging Paradigms Programming/Erlang/MiniErlangBlockchain/teacher_node.erl').


sleep(N) -> receive after N*1000 -> ok end.


watch(Main,Node) ->
 	sleep(10),
 	Ref = make_ref(),
 	Node ! {ping, self(), Ref},
 	receive
		{pong, Ref} -> watch(Main,Node)
 	after 2000 -> Main ! {dead, Node}
	end.


loop(MyListFriends) ->
	% Controllo che la mia lista di amici
	% Nel caso in cui è vuota chiedo al teacher_node di darmene uno
	% Altrimenti chiedo ad uno la sua lista di amci 
	
	case MyListFriends of
		[] 	-> 	global:send(teacher_node, {get_friends, self(), make_ref()}),
				io:format("~p Require a friend to teacher_node ~n ~n",[self()]),
				sleep(2);
		_ ->	askFriend(MyListFriends),
				io:format("~p AskFriend ~n ~n",[self()]),
				sleep(2)
	end,

	% Mi metto in ascolto dei messaggi in arrivo
	receive
		{ping, Mittente, Nonce} -> 
			io:format("~p Received ping request! Send pong response ~n ~n",[self()]),

			Mittente ! {pong, Nonce},
			sleep(?TimeSleep),
			loop(MyListFriends);

		{dead, Node} ->
			io:format("For ~p the node ~p is dead ~n ~n",[self(),Node]),

            UpdatedList = MyListFriends -- [Node],
			%askFriend(UpdatedList),
			loop(UpdatedList);

		{friends, Nonce, ListOtherFriends} ->
			io:format("~p Receive friend list ~p ~p ~n ~n",[self(),ListOtherFriends,Nonce]),

			% Trovo una lista di amici dall'elenco degli amici ricevuta (escludendo me e gli amici già presenti)
            NewFriends = ListOtherFriends -- (MyListFriends ++ [self()]),
            
            % Aggiorno la mia lista
			
            MyNewListFriends = addFriend(MyListFriends, NewFriends),
			sleep(?TimeSleep),
			io:format("~p list of friends is: ~p with lenght ~p  ~n ~n",[self(),MyNewListFriends,length(MyNewListFriends)]),
            loop(MyNewListFriends);

		{get_friends, Sender, Nonce} ->
			io:format("~p Send friend list to ~p ~n ~n",[self(),Sender]),

            Sender ! {friends, Nonce, MyListFriends},
			sleep(?TimeSleep),
            loop(MyListFriends)

        %{push, Transazione} -> % Transazione = {IDtransazione, Payload};
        	

        %{update, Sender, Blocco} -> % Blocco = {IDnuovo_blocco, IDblocco_precedente, Lista_di_amici, Soluzione}

	end.


% Chiedo ad un amico casuale la sua lista di amici
askFriend(MyListFriends) ->
	
	case length(MyListFriends) < ?MaxNumberOfFriends of
		true ->	io:format("~p Require list friends to a random friend of this ~p ~n ~n",[self(),MyListFriends]), 
				lists:nth(rand:uniform(length(MyListFriends)), MyListFriends) ! {get_friends, self(), make_ref()};

		false -> io:format("~p Have already three friends ~n ~n",[self()])
	end.

% Aggiungo un amico alla mia lista
addFriend(MyListFriends, []) -> MyListFriends;

addFriend(MyListFriends, NewFriends) ->
	
	case length(MyListFriends) < ?MaxNumberOfFriends of
		true ->
			Friend = lists:nth(rand:uniform(length(NewFriends)), NewFriends),

			io:format("~p Add a new friend ~p  ~n ~n",[self(),Friend]),
			Self = self(),
			spawn(fun () -> watch(Self, Friend) end),
			addFriend(MyListFriends ++ [Friend], NewFriends -- [Friend]);
			

		false -> io:format("List of ~p friends is full ~n ~n",[self()])
end.


% Mi registro nella lista globale ed inizio
main(NameNode) -> 
	
	register(NameNode ,self()),
 	global:register_name(NameNode,self()),
	loop([]).


test() ->
	
	compile:file(?TeacherNodeUrl),
	spawn(teacher_node, main, []), % teacher node
  	sleep(?TimeSleep+3),

	N1 = spawn(fun() -> main(n1) end),
	io:format("~p ~n ~n",[N1]),
	sleep(?TimeSleep),
	N2 = spawn(fun() -> main(n2) end),
	io:format("~p ~n ~n",[N2]),
	sleep(?TimeSleep),
	N3 = spawn(fun() -> main(n3) end),
	io:format("~p ~n ~n",[N3]),
	sleep(?TimeSleep),
	N4 = spawn(fun() -> main(n4) end),
	io:format("~p ~n ~n",[N4]),
	sleep(?TimeSleep),
	io:format("Finish to spawn nodes and teacher-node ~n ~n"),
	sleep(?TimeSleep).
	
	
