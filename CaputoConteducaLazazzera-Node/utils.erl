-module(utils).
-export([sendMessage/2,sleep/1,watch/2,watchFriends/2]).
-define(RANDOM, 10).

sleep(N) -> receive after N*1000 -> ok end.

sendMessage(Dest,Message) ->
    X = rand:uniform(?RANDOM),
    case X of
        2 ->
            Dest ! Message,
            Dest ! Message;
        3 ->
            none;
        _ -> Dest ! Message
    end.

% Node è il nodo da monitorare
% Main è il nodo che vuole essere avvisato della morte di Node
% il watcher ogni 5s fa ping a Node 
% e se dopo 2s non risponde allora non è piu vivo
watch(Main,Node) ->
    sleep(5),
    Ref = make_ref(),
    Node ! {ping, self(), Ref},
    receive
        {pong, Ref} -> watch(Main,Node)
    after 2000 -> 
        Main ! {dead, Node}
    end.

watchFriends(NewItems,Main) -> [spawn(fun()-> watch(Main,X) end) || X<-NewItems].

