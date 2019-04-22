-module(support).
-export([sleep/1, flatten/1, all_elements_are_different/2, get_first_elements/2, index_of_block/2, filter_a/2,send_msg/2]).

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


%% Prende i primi Size elementi di una lista
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