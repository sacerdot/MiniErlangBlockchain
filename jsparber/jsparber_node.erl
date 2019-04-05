-module(jsparber_node).

-export([main/0, test/0]).

% This is blockchain node, based on teacher_node.

sleep(N) -> receive  after N * 1000 -> ok end.

watch(Main, Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node}
  end.

loop(Nodes, Nonces) ->
  New_nonces = if length(Nodes) == 0 ->
                    [ask_teacher() | Nonces];
                  length(Nodes) < 3 -> [ask_friend(Nodes) | Nonces];
                  true -> Nonces
               end,
  receive
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref}, loop(Nodes, New_nonces);
    {get_friends, Sender, Nonce} ->
      New_nodes = add_nodes(Sender, Nodes),
      Sender ! {friends, Nonce, New_nodes},
      loop(New_nodes, New_nonces);
    {friends, Nonce, Incomming_nodes} ->
      case lists:member({friends, Nonce}, New_nonces) of
        true ->
          New_nodes = add_nodes(Incomming_nodes, Nodes),
          io:format("~p nodes discovered~n", [length(New_nodes)]),
          % ask teacher for more friends if we didn't get enough from a friend
          New_New_nonces = if length(New_nodes) < 3 ->
                                [ask_teacher() | New_nonces];
                              true -> New_nonces
                           end,
          % Ask all new Nodes for there head
          This_nonces = request_head_all(Incomming_nodes),
          %We remove the Nonce and we need to add Nonces for each head request
          loop(New_nodes,
               [This_nonces | lists:delete({friends, Nonce}, New_New_nonces)]);
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with the friends list"),
          loop(Nodes, New_nonces)
      end;
    {dead, Node} ->
      io:format("Dead node ~p~n", [Node]),
      loop(Nodes -- [Node], New_nonces);
    {get_previous, Sender, Nonce, Prev_block_id} ->
      storage ! {get_previous, Nonce, Sender, Prev_block_id},
      loop(Nodes, New_nonces);
    {previous, Nonce, Block} ->
      case lists:member({previous, Nonce}, New_nonces) of
        true ->
          storage ! {add_block, Block},
          loop(Nodes,
               lists:delete({previous, Nonce}, New_nonces));
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with the friends list"),
          loop(Nodes, New_nonces)
      end;
    {get_head, Sender, Nonce} ->
      storage ! {get_head, Nonce, Sender},
      loop(Nodes, New_nonces);
    {head, Nonce, Block} ->
      case lists:member({head, Nonce}, New_nonces) of
        true ->
          storage ! {add_block, Block},
          loop(Nodes,
               lists:delete({previous, Nonce}, New_nonces));
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with the friends list"),
          loop(Nodes, New_nonces)
      end;
    %Alogritm for gossiping blocks
    {update, Block} ->
      storage ! {add_block, Block}, loop(Nodes, New_nonces);
    %Alogritm for gossiping transections
    {push, Transaction} ->
      storage ! {add_transaction, Transaction},
      loop(Nodes, New_nonces);
    %Gossip
    {gossip, Data} ->
      send_all(Nodes, Data), loop(Nodes, New_nonces)
  end.

storage_loop(Blocks, Heads, Transactions) ->
  receive
    {add_block, Block} ->
      {Final_blocks, Final_trans} = case check_block(Blocks, Block) of
                    true ->
                      New_blocks = [Block | Blocks],
                      %TODO: update Heads and clean up Blocks
                      jsparber_node ! {gossip, Block},
                      {New_blocks, remove_transactions(Transactions, New_blocks)};
                    false -> {Blocks, Transactions}
                  end,
      storage_loop(Final_blocks, Heads, Final_trans);
    {add_transaction, Transaction} ->
      New_transactions = case check_transection(Blocks,
                                                Transaction)
                         of
                           true ->
                             jsparber_node ! {gossip, Transaction},
                             [Transaction | Transactions];
                           false -> Transactions
                         end,
      Remaing_transactions = case
                               lists:length(New_transactions) > 9
                             of
                               true ->
                                 {Block_transactions, T_transactions} =
                                 lists:split(10, New_transactions),
                                 spawn(fun () ->
                                           mine_block({get_longest_head(Heads),
                                                       Block_transactions})
                                       end),
                                 T_transactions;
                               false -> New_transactions
                             end,
      storage_loop(Blocks, Heads, Remaing_transactions);
    {get_previous, Nonce, Sender, Prev_block_id} ->
      Sender !
      {previous, Nonce, get_block(Blocks, Prev_block_id)},
      storage_loop(Blocks, Heads, Transactions);
    {request_head, Dest, Nonce} ->
      Dest ! {head, Nonce},
      storage_loop(Blocks, Heads, Transactions);
    {get_head, Nonce, Sender} ->
      Sender ! {head, Nonce, get_longest_head(Heads)},
      storage_loop(Blocks, Heads, Transactions)
  end.


% This takes a list of nodes and sends a get_head request to every node
% It returns a list of Nonces
request_head_all(Nodes) -> request_head_all(Nodes, []).

request_head_all([], Nonces) -> Nonces;
request_head_all([H | T], Nonces) ->
  Nonce = make_ref(),
  H ! {get_head, self(), Nonce},
  send_all(T, [Nonce | Nonces]).

send_all([], _) -> none;
send_all([H | T], Data) -> H ! Data, send_all(T, Data).

build_chain([]) -> none.

mine_block({Id, Transactions}) ->
  Solution = proof_of_work:solve(Transactions),
  storage !
  {add_block, {make_ref(), Id, Transactions, Solution}}.

% Checks if block is new and valid
check_block(Blocks,
            {Block_id, _, List_of_transection, Solution}) ->
  case is_new_block(Blocks, Block_id) of
    true ->
      proof_of_work:check({Block_id, List_of_transection},
                          Solution);
    false -> false
  end.

is_new_block([], _) -> true;
is_new_block([{Block_id, _, _, _} | T], Id) ->
  if Block_id == Id -> false;
     true -> is_new_block(T, Id)
  end.

% Checks if a transection is new
check_transection([], _) -> true;
check_transection([{_, _, List_of_transection, _} | T],
                  {Trans_id, Payload}) ->
  case is_new_transection(List_of_transection, Trans_id)
  of
    true -> check_transection(T, {Trans_id, Payload});
    false -> false
  end.

is_new_transection([], _) -> true;
is_new_transection([{Trans_id, _} | T], Id) ->
  (Trans_id /= Id) and is_new_transection(T, Id).

remove_transactions([], _) -> [];
remove_transactions([H | T],
                    {Block_id, Prev_block_id, List_of_transection,
                     Solution}) ->
  case lists:member(H, List_of_transection) of
    true ->
      remove_transactions(T,
                          {Block_id, Prev_block_id, List_of_transection,
                           Solution});
    false ->
      [H | remove_transactions(T,
                               {Block_id, Prev_block_id,
                                List_of_transection, Solution})]
  end.

% Return longest head out of a list of heads of form {Block_id, Length}
get_longest_head(Heads) ->
  get_longest_head(Heads, {none, 0}).

get_longest_head([], Max_length) -> Max_length;
get_longest_head([{Id, Length} | T],
                 {Max_head, Max_length}) ->
  if Length < Max_length ->
       get_longest_head(T, {Max_head, Max_length});
     Length > Max_length ->
       get_longest_head(T, {Id, Length});
     true ->
       % If equals take the last found
       get_longest_head(T, {Id, Length})
  end.

get_block([], _) -> none;
get_block([{Block_id, Prev_block_id,
            List_of_transection, Solution}
           | T],
          Id) ->
  if Id == Prev_block_id ->
       {Block_id, Prev_block_id, List_of_transection,
        Solution};
     true -> get_block(T, id)
  end.

ask_teacher() ->
  Ref = make_ref(),
  io:format("Ask teacher for more friends~n"),
  {teacher_node, teacher_node@librem} !
  {get_friends, self(), Ref},
  {friends, Ref}.

ask_friend(Nodes) ->
  Ref = make_ref(),
  io:format("I only have ~p friends. Ask a friend "
            "for more friends~n",
            [length(Nodes)]),
  lists:nth(rand:uniform(length(Nodes)), Nodes) !  {get_friends, self(), Ref},
  {friends, Ref}.

add_nodes([], Nodes) -> Nodes;
add_nodes([H | T], Nodes) ->
  if self() /= H ->
       case lists:member(H, Nodes) of
         true -> add_nodes(T, Nodes);
         false ->
           io:format("New node ~p~n", [H]),
           Self = self(),
           spawn(fun () -> watch(Self, H) end),
           add_nodes(T, [H | Nodes])
       end;
     true -> add_nodes(T, Nodes)
  end.

main() ->
  register(jsparber_node, self()),
  %global:register_name(jsparber_node, self()),
  io:format("A new jsparber_node registered~n"),
  register(storage,
           spawn(fun () -> storage_loop([], [], []) end)),
  loop([], []).

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

test() -> spawn(fun main/0).
