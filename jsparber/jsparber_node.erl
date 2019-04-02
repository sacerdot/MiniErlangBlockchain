-module(jsparber_node).
-export([main/0, test/0]).

% This is blockchain node, based on teacher_node.

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main,Node)
  after 2000 -> Main ! {dead, Node}
  end.

loop(Nodes, Nonces) ->
  New_nonces = if
                 length(Nodes) == 0 ->
                   [ask_teacher() | Nonces];
                 length(Nodes) < 3 ->
                   [ask_friend(Nodes)| Nonces];
                 true ->
                   Nonces
               end,
  receive
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref} ,
      loop(Nodes, New_nonces);
    {get_friends, Sender, Nonce} ->
      New_nodes = add_nodes(Sender, Nodes),
      Sender ! {friends, Nonce, New_nodes},
      loop(New_nodes, New_nonces) ;
    {friends, Nonce, Incomming_nodes} ->
      case lists:member({friends, Nonce}, New_nonces) of
        true ->
          New_nodes = add_nodes(Incomming_nodes, Nodes),
          io:format("~p nodes discovered~n", [length(New_nodes)]),
          % ask teacher for more friends if we didn't get enough from a friend
          New_New_nonces = if
                             length(New_nodes) < 3 ->
                               [ask_teacher() | New_nonces];
                             true ->
                               New_nonces
                           end,
          %We need to remove the Nonce
          loop(New_nodes, lists:delete({friends, Nonce}, New_New_nonces));
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce with the friends list"),
          loop(Nodes, New_nonces)
      end;
    {dead, Node} ->
      io:format("Dead node ~p~n",[Node]),
      loop(Nodes -- [Node], New_nonces);
    {get_previous, Sender, Nonce, Prev_block_id} ->
      storage ! {get_previous, Nonce, Sender, Prev_block_id};
    {previous, Nonce, Block} ->
      %TODO: check Nonce and if block is valid
      storage ! {add_block, Block};
    {get_head, Sender, Nonce} ->
      storage ! {get_head, Nonce, Sender};
    {head, Nonce, Block} ->
      %TODO: check Nonce and if block is valid
      storage ! {update, Block};
    %Alogritm for gossiping blocks
    {update, {Block_id, Prev_block_id, List_of_transection, Solution}}  ->
      case proof_of_work:check({Block_id, List_of_transection}, Solution) of
        true ->
          send_all(Nodes, {Block_id, Prev_block_id, List_of_transection, Solution}),
          storage ! {add_block, {Block_id, Prev_block_id, List_of_transection, Solution}};
        false ->
          io:format("Wrong solution")
      end;
    %Alogritm for gossiping transections
    {push, Transaction} ->
      % TODO: stop gossiping if transection was already known
      send_all(Nodes, {push, Transaction}),
      storage ! {add_transaction, Transaction}
  end.

storage_loop(Blocks, Transactions) ->
  receive
    {add_block, Block}  ->
      % TODO: add Block in correct location
      storage_loop([Block | Blocks], Transactions);
    {add_transaction, Transaction} ->
      storage_loop(Blocks, [Transaction | Transactions]);
    % TODO: create new block if Tracntions more then 10, DO mining
    {get_previous, Nonce, Sender, Prev_block_id} ->
      Sender ! {previous, Nonce, get_block(Blocks, Prev_block_id)};
    {get_head, Nonce, Sender} ->
      Sender ! {head, Nonce, lists:head(Blocks)}
  end.

send_all([], _) -> none;
send_all([H | T], Data) ->
  H ! Data,
  send_all(T, Data).

get_block([], _) -> none;
get_block([{Block_id, Prev_block_id, List_of_transection, Solution} | T], Id) ->
  if
    Id == Prev_block_id ->
      {Block_id, Prev_block_id, List_of_transection, Solution};
    true ->
      get_block(T, id)
  end.

ask_teacher() ->
  Ref = make_ref(),
  io:format("Ask teacher for more friends~n"),
  {teacher_node, 'teacher_node@librem'} ! {get_friends, self(), Ref},
  {friends, Ref}.

ask_friend(Nodes) ->
  Ref = make_ref(),
  io:format("I only have ~p friends. Ask a friend for more friends~n", [length(Nodes)]),
  lists:nth(rand:uniform(length(Nodes)), Nodes) ! {get_friends, self(), Ref},
  {friends, Ref}.

add_nodes([], Nodes) -> Nodes;
add_nodes([H|T], Nodes) ->
  if
    self() /= H ->
      case lists:member(H, Nodes) of
        true -> add_nodes(T, Nodes);
        false ->
          io:format("New node ~p~n",[H]),
          Self = self(),
          spawn(fun () -> watch(Self, H) end),
          add_nodes(T, [H|Nodes])
      end;
    true ->
      add_nodes(T, Nodes)
  end.

main() ->
  %register(jsparber_node, self()),
  %global:register_name(jsparber_node, self()),
  io:format("A new jsparber_node registered~n"),
  register(storage, spawn(fun () -> storage_loop([], []) end)),
  loop([], []).

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

test() ->
  spawn(fun main/0).
