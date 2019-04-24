-module(test).
-author("Andrea Morabito, Paolo Santilli").
-export([main/0]).

-define(MaxNumberOfFriends, 3).


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
				io:format("Require a friend to teacher_node ~n");
		_ ->	askFriend(MyListFriends)
	end,

	% Mi metto in ascolto dei messaggi in arrivo
	receive
		{ping, Mittente, Nonce} -> 
			io:format("Received ping request! Send pong response"),

			Mittente ! {pong, Nonce},
			loop(MyListFriends);

		{dead, Node} ->
			io:format("The node ~p is dead ~n", Node),

            UpdatedList = MyListFriends -- [Node],
            askFriend(UpdatedList),
            loop(UpdatedList);

		{friends, Nonce, ListOtherFriends} ->
			io:format("Receive friend list ~n"),

			% Trovo una lista di amici dall'elenco degli amici ricevuta (escludendo me e gli amici già presenti)
            NewFriends = ListOtherFriends -- (MyListFriends ++ [self()]),
            
            % Aggiorno la mia lista
            MyNewListFriends = addFriend(MyListFriends, NewFriends),
            loop(MyNewListFriends);

		{get_friends, Sender, Nonce} ->
			io:format("Send friend list to ~p ~n", Sender),

            Sender ! {friends, Nonce, MyListFriends},
            loop(MyListFriends)

        %{push, Transazione} -> % Transazione = {IDtransazione, Payload};
        	

        %{update, Sender, Blocco} -> % Blocco = {IDnuovo_blocco, IDblocco_precedente, Lista_di_amici, Soluzione}

	end.


% Chiedo ad un amico casuale la sua lista di amici
askFriend(MyListFriends) ->
	io:format("Require list friends to a friend ~n"),
	case length(MyListFriends) < ?MaxNumberOfFriends of
		true -> lists:nth(rand:uniform(length(MyListFriends)), MyListFriends) ! {get_friends, self(), make_ref()};
		false -> io:format("There are already three friends ~n")
	end.

% Aggiungo un amico alla mia lista
addFriend(MyListFriends, []) -> MyListFriends;

addFriend(MyListFriends, NewFriends) ->
	case length(MyListFriends) < ?MaxNumberOfFriends of
		true ->
			Friend = lists:nth(rand:uniform(length(NewFriends)), NewFriends),

			io:format("Add a new friend ~p ~n", Friend),
			spawn(fun () -> watch(self(), Friend) end),
			addFriend(MyListFriends ++ [Friend], NewFriends -- [Friend]); 

		false -> io:format("List of friends is full ~n")
end.


% Mi registro nella lista globale ed inizio
main() -> 
	register(newNode ,self()),
 	global:register_name(newNode,self()),

	io:format("new Node registered ~n"),

	loop([]).
	