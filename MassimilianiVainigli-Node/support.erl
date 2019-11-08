-module(support).
-export([sleep/1, flatten/1, all_elements_are_different/2, get_first_elements/2, index_of_block/2, filter_a/2,send_msg/2, trasform_to_list/1, addRef/2, add_friends/3, watch/2]).

%% attesa di N secondi
sleep(N) -> receive after N*1000 -> ok end.


%% Ritorna una lista appiattita
flatten(X) -> lists:reverse(flatten(X,[])).
flatten([],Acc) -> Acc;
flatten([H|T],Acc) when is_list(H) -> flatten(T, flatten(H,Acc));
flatten([H|T],Acc) -> flatten(T,[H|Acc]).


%% Ritorna true se l'intersezione delle due liste è vuota
all_elements_are_different(L1, L2) ->
  L1F = flatten(L1),
  L2F = flatten(L2),
  Commons = [X || X <- L1F, Y <- L2F, X == Y],
  if(length(Commons) > 0) -> false; true-> true end.


%% Prende i primi n elementi di una lista
get_first_elements(_, 0) -> [];
get_first_elements([], _) ->  [];
get_first_elements(List, Size) -> [lists:nth(1, List)] ++ get_first_elements(List -- [lists:nth(1, List)],Size-1 ).


%% Ritorna l'indice in cui ID compare nella BlockList
index_of_block([],_) -> 0;
index_of_block(BlockList, ID) ->
  Block = lists:nth(1, BlockList),
  Ref = element(1, Block),
  RefID = element(1, ID),
  if(RefID == Ref) -> 0; true -> 1 + index_of_block(BlockList -- [Block], ID) end.


%% Differenza tra due liste
filter_a(L1,L2) ->
  [ X || X <- L1, lists:member(X,L2) =/= true ].



%% il 10% dei messaggi viene perso e un altro 10% viene inviato in doppia copia
send_msg(Receiver, Message) ->
  Rand = rand:uniform(10),
  case Rand of
    1 ->
      Receiver ! Message,
      Receiver ! Message;
    2 ->
      ok;
    _Else ->   Receiver ! Message
  end.


%% Se L è un lista ritorna L, altrimenti [L]
trasform_to_list([L]) -> [L];
trasform_to_list(L) -> [L].


%% funzione ausiliaria
addRef(List, List2) ->  if(List2 == [[]] ) -> List; true -> List ++ List2 end.



%% gestione dell'aggiunta di elementi da una lista all'altra
add_friends(List1, [], _) -> List1;
add_friends(List1, List2, Parent) ->
  MyPid = self(),
  if
    length(List1) >= 3 -> List1;
    true ->
      RandomElement = lists:nth(rand:uniform(length(List2)), List2),
      spawn(fun() ->watch(MyPid, RandomElement) end),
      add_friends(List1++[RandomElement], List2--[RandomElement], Parent)
  end.



%% riferisce a Main della vita di Node
watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 ->  Main ! {dead, Node}
  end.

