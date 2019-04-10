-module(check_act).
-export([checkList/1, askProfAfterTimeout/1]).

sleep(N) -> receive after N*1000 -> ok end.

%%%%%%%%  behavior dell'attore che controlla gli amici di PID %%%%%%%%
checkList(Pid) -> 
    sleep(6),
    Pid ! {checkList,self()},
    receive
        {myList, List} -> 
            requireFriends(List,Pid),
            checkList(Pid)
    end.

requireFriends(FList,Mittente) -> 
        case length(FList) < 3 of
            true ->
                case length(FList) =:= 0  of
                    true -> 
                        %io:format("~p chiede al prof ~n",[Mittente]),
                        askProf(Mittente);
                    false ->
                        spawn(?MODULE,askProfAfterTimeout,[Mittente]),
                        %io:format("~p chiede agli amici ~n",[Mittente]),
                        askToFriends(FList,Mittente)
                end;
            false -> ok
        end.
askProf(Mittente) ->
    global:send(teacher_node,{get_friends,Mittente,make_ref()}).    
askProf(Mittente, FList) ->
    case length(FList) < 3 of 
        true -> global:send(teacher_node,{get_friends,Mittente,make_ref()});
        false -> ok
    end.
askToFriends(FList, Mittente) ->  
    Ref = make_ref(),
    PidFriendReq = lists:nth(rand:uniform(length(FList)),FList), 
    % mando a PidFriendReq la richiesta di avere amici
    PidFriendReq ! {get_friends, Mittente, Ref}. 
askProfAfterTimeout(Mittente) ->
    sleep(40),
    Self = self(),
    Mittente ! {checkList, Self},
    receive 
        {myList, FriendsList} ->
                askProf(Mittente,FriendsList)
    end.