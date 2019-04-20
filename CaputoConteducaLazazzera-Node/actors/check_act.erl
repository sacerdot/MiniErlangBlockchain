-module(check_act).
-export([start_C_act/1, askProfAfterTimeout/1]).
-import (utils , [sendMessage/2,sleep/1]).


%! -----  behavior dell'attore che controlla gli amici di PID ------
start_C_act(PidRoot) -> 
    sleep(6),
    PidRoot ! {checkFriendsList,self()},
    receive
        {myFriendsList, List} -> 
            case length(List) < 3 of
                true -> 
                    requireFriends(List,PidRoot),
                    start_C_act(PidRoot);
                false ->
                    start_C_act(PidRoot)
            end
    end.

requireFriends(FList,PidRoot) -> 
    case length(FList) =:= 0  of
        true -> 
            %io:format("~p chiede al prof ~n",[PidRoot]),
            askProf(PidRoot);
        false ->
            askToFriends(FList,PidRoot),
            sleep(10),
            askProfAfterTimeout(PidRoot)         
    end.
    
askProf(PidRoot) ->
    Nonce = make_ref(),
    PidRoot ! {addNewNonce, Nonce},
    global:send(teacher_node,{get_friends,PidRoot,Nonce}).    


askToFriends(FList, PidRoot) ->
    case length(FList) of 
        0 ->
            nothing_to_do;
        _ -> 
            Nonce = make_ref(),
            PidRoot ! {addNewNonce, Nonce},
            [H|T]= FList,
            sendMessage(H , {get_friends, PidRoot, Nonce}),
            askToFriends(T, PidRoot)
    end.

askProfAfterTimeout(PidRoot) ->
    PidRoot ! {checkFriendsList, self()},
    receive 
        {myFriendsList, FriendsList} ->  
            case length(FriendsList) < 3 of 
                true ->
                    % io:format("Chiamo il prof~n"), 
                    askProf(PidRoot);
                false -> nothing_to_do
            end
    end.