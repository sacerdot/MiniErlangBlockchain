-module(jsparber_node).

-export([main/0, test/0]).

% This is blockchain node, based on teacher_node.
% TODO: Add function to lose messages
% TODO: Clean up heads and blocks

sleep(N) -> receive  after N * 1000 -> ok end.

watch(Main, Node) ->
  sleep(10),
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> watch(Main, Node)
  after 2000 -> Main ! {dead, Node}
  end.

% This removes Nonces after a timeout of 2 secounds
nonces_cleaner(Main, Nonces) ->
  spawn(fun () ->
            sleep(2),
            Main ! {remove_nonce, Nonces}
        end),
  Nonces.

ask_with_delay(Main, Teacher) ->
  spawn(fun () ->
            sleep(10),
            Main ! {request_friends, Teacher}
        end).

check_friends_list(Nodes, Nonces) ->
  % we use a nonce to block future requests
  case lists:member({int_friends, 1}, Nonces) of
    false ->
      if
        length(Nodes) == 0 ->
          ask_with_delay(self(), true),
          {int_friends, 1};
        length(Nodes) < 3 ->
          ask_with_delay(self(), false),
          {int_friends, 1};
        true -> []
      end;
    true -> []
  end.

loop(Nodes, Old_nonces) ->
  Nonces = [check_friends_list(Nodes, Old_nonces) | Old_nonces],
  receive
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref}, loop(Nodes, Nonces);
    {request_friends, Teacher} ->
      New_nonces = case length(Nodes) < 3 of
                     true ->
                       case Teacher of
                         true -> nonces_cleaner(self(), [ask_teacher(self())]) ++ Nonces;
                         false -> nonces_cleaner(self(), [ask_friend(self(), Nodes)]) ++ Nonces
                       end;
                     false ->
                       Nonces
                   end,
      loop(Nodes, New_nonces -- [{int_friends, 1}]);
    {get_friends, Sender, Nonce} ->
      New_nodes = add_nodes([Sender], Nodes),
      Sender ! {friends, Nonce, New_nodes},
      loop(New_nodes, Nonces);
    {remove_nonce, Rm_nonces} ->
      loop(Nodes, Nonces -- Rm_nonces);
    {friends, Nonce, Incomming_nodes} ->
      case lists:member({friends, Nonce}, Nonces) of
        true ->
          New_nodes = add_nodes(Incomming_nodes, Nodes),
          % If we still don't have enough friends ask the the teacher
          case length(New_nodes) < 3 of
            true -> ask_with_delay(self(), true);
            false -> none
          end,
          io:format("~p nodes discovered~n", [length(Incomming_nodes)]),
          io:format("~p Number of avaible nodes~n", [length(New_nodes)]),
          % Ask all new Nodes for there head
          This_nonces = request_head_all(Incomming_nodes -- [self()]),
          %We remove the Nonce and we need to add Nonces for each head request
          loop(New_nodes, Nonces ++ nonces_cleaner(self(), This_nonces) -- [{friends, Nonces}]);
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with for friends~n"),
          loop(Nodes, Nonces)
      end;
    {dead, Node} ->
      io:format("Dead node ~p~n", [Node]),
      loop(Nodes -- [Node], Nonces);
    {get_previous, Sender, Nonce, Prev_block_id} ->
      storage ! {get_previous, Nonce, Sender, Prev_block_id},
      loop(Nodes, Nonces);
    {previous, Nonce, Block} ->
      case lists:member({previous, Nonce}, Nonces) of
        true ->
          storage ! {add_previous_block, Block},
          loop(Nodes, Nonces -- [{previous, Nonce}]);
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with for previous~n"),
          loop(Nodes, Nonces)
      end;
    {get_head, Sender, Nonce} ->
      storage ! {get_head, Sender, Nonce},
      loop(Nodes, Nonces);
    {head, Nonce, Block} ->
      case lists:member({head, Nonce}, Nonces) of
        true ->
          storage ! {add_block, Block},
          loop(Nodes, Nonces -- [{head, Nonce}]);
        false ->
          io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with head~n"),
          loop(Nodes, Nonces)
      end;
    %Alogritm for gossiping blocks
    {update, Block} ->
      storage ! {add_block, Block}, loop(Nodes, Nonces);
    %Alogritm for gossiping transections
    {push, Transaction} ->
      storage ! {add_transaction, Transaction},
      loop(Nodes, Nonces);
    %Gossip
    {gossip, Data} ->
      send_all(Nodes, Data), loop(Nodes, Nonces);
    % Show is used for debuging
    {show} ->
      io:format("Number of nodes ~p~n", [length(Nodes)]),
      io:format("Number of nonces ~p~n", [length(Nonces)]);
    {request_previous, Prev_block_id} ->
      % Ask a random friend for the block
      Ref = make_ref(),
      lists:nth(rand:uniform(length(Nodes)), Nodes) !  {get_previous, self(), Ref, Prev_block_id},
      loop(Nodes, [Ref | Nonces])
  end.

storage_loop(Blocks, Heads, Transactions, Trans_mining) ->
  io:format("Heads ~p~n", [Heads]),
  receive
    {add_head, New_head, Remove_head} ->
      io:format("New head"),
      storage_loop(Blocks, [New_head | Heads -- [Remove_head]], Transactions, Trans_mining);
    {add_previous_block, Block} ->
      {Final_blocks, Final_trans} = case check_block(Blocks, Block) of
                                      true ->
                                        New_blocks = [Block | Blocks],
                                        % Explore chains with any missing previous block
                                        explore_all_chains(New_blocks, Heads),
                                        {New_blocks, remove_transactions(Transactions, New_blocks)};
                                      false -> {Blocks, Transactions}
                                    end,
      storage_loop(Final_blocks, Heads, Final_trans, Trans_mining);
    {add_block, Block} ->
      {Final_blocks, Final_trans, Final_mining_trans} = case check_block(Blocks, Block) of
                                                          true ->
                                                            io:format("New Block~n"),
                                                            New_blocks = [Block | Blocks],
                                                            % Explore chain till we find a old head, Block is in this case a head
                                                            explore_chain(New_blocks, Heads, Block),
                                                            jsparber_node ! {gossip, {update, Block}},
                                                            {New_blocks, remove_transactions(Transactions, New_blocks), remove_transactions(Trans_mining, New_blocks)};
                                                          false -> {Blocks, Transactions, Trans_mining}
                                                        end,
      storage_loop(Final_blocks, Heads, Final_trans, Final_mining_trans);
    {add_transaction, Transaction} ->
      io:format("Transection: ~p~n", [Transaction]),
      case lists:member(Transaction, Transactions ++ Trans_mining) of
           false ->
          New_transactions = case check_transection(Blocks,
                                                    Transaction)
                             of
                               true ->
                                 io:format("Send transaction~n"),
                                 jsparber_node ! {gossip, {push, Transaction}},
                                 [Transaction | Transactions];
                               false -> Transactions
                             end,
          {Remaing_transactions, Mining_transactions} = case
                                                          length(New_transactions) > 9
                                                        of
                                                          true ->
                                                            {Block_transactions, T_transactions} =
                                                            lists:split(10, New_transactions),
                                                            spawn(fun () ->
                                                                      mine_block({get_longest_head(Heads),
                                                                                  Block_transactions})
                                                                  end),
                                                            {T_transactions, Trans_mining ++ Block_transactions};
                                                          false -> {New_transactions, Trans_mining}
                                                        end,
          storage_loop(Blocks, Heads, Remaing_transactions, Mining_transactions);
        true -> storage_loop(Blocks, Heads, Transactions, Trans_mining)
      end;
    {get_previous, Nonce, Sender, Prev_block_id} ->
      Sender !
      {previous, Nonce, get_block(Blocks, Prev_block_id)},
      storage_loop(Blocks, Heads, Transactions, Trans_mining);
    {get_head, Sender, Nonce} ->
      Sender ! {head, Nonce, get_longest_head(Heads)},
      storage_loop(Blocks, Heads, Transactions, Trans_mining)
  end.

% This takes a list of nodes and sends a get_head request to every node
% It returns a list of Nonces
request_head_all([]) -> [];
request_head_all([H | T]) ->
  Nonce = make_ref(),
  H ! {get_head, self(), Nonce},
  [{head, Nonce} | request_head_all(T)].

send_all([], _) -> none;
send_all([H | T], Data) -> H ! Data, send_all(T, Data).

% Explore every chain to check if we got all blocks now
explore_all_chains(_, []) -> none;
explore_all_chains(Blocks, [{Head_id, Length} | T]) ->
  % Explore onyl heads with missing blocks (where we couldn't get the length)
  case Length == 0 of
    true ->
      % we can ignore the head we are exploring right now
      explore_chain(Blocks, T, get_block_by_id(Blocks, Head_id));
    false -> none
  end,
  explore_all_chains(Blocks, T).

% Takes 3 arguments Blocks, Heads and new Head
explore_chain(Blocks, Heads, {Head_id, Prev_head_id, _, _}) ->
  explore_chain(Blocks, Heads, Head_id, Prev_head_id, 0).

% In case we don't have the explored chain at all (we are hiting none)
explore_chain(_, _, Head_to_insert, none, Depth) ->
  storage ! {add_head, {Head_to_insert, Depth}, []};

explore_chain(Blocks, Heads, Head_to_insert, Prev_head_id, Depth) ->
  case lists:keyfind(Prev_head_id, 1, Heads) of
    {_, Length} ->
      %send new head to storage and the head to replace
      storage ! {add_head, {Head_to_insert, Depth + Length}, Prev_head_id};
    false ->
      case get_block_by_id(Blocks, Prev_head_id) of
        % We could remove this block form the Blocks list
        % I'm not sure if that would imporve performance
        {_, Prev_block_id, _, _} ->
          explore_chain(Blocks, Heads, Head_to_insert, Prev_block_id, Depth + 1);
        false ->
          io:format("Need to request block"),
          %request block
          storage ! {add_head, {Head_to_insert, 0}, []},
          jsparber_node ! {request_previous, Prev_head_id}
      end
  end.

get_block_by_id(Blocks, Id) ->
  lists:keyfind(Id, 2, Blocks).

mine_block({Id, Transactions}) ->
  io:format("Started mining~n"),
  Solution = proof_of_work:solve({Id, Transactions}),
  io:format("Finished mining~n"),
  storage !
  {add_block, {make_ref(), Id, Transactions, Solution}}.

% Checks if block is new and valid
check_block(Blocks,
            {Block_id, Prev_block_id, List_of_transection, Solution}) ->
  case is_new_block(Blocks, Block_id) of
    true ->
      io:format("Check block~p~n", [{Prev_block_id, Solution}]),
      proof_of_work:check({Prev_block_id, List_of_transection},
                          Solution);
    false -> false
  end;
check_block(_, _) -> io:format("No valid block, maybe empty chain?~n"), false.

is_new_block([], _) -> true;
is_new_block([{Block_id, _, _, _} | T], Id) ->
  if Block_id == Id -> false;
     true -> is_new_block(T, Id)
  end.

% Checks if a transection is new
check_transection([], _) -> true;
check_transection([{_, _, List_of_transection, _} | T],
                  {Trans_id, Payload}) ->
  case lists:keyfind(Trans_id, 1, List_of_transection) of
    false -> check_transection(T, {Trans_id, Payload});
    _ -> true
  end.

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

ask_teacher(Self) ->
  Ref = make_ref(),
  io:format("Ask teacher for more friends~n"),
  {teacher_node, teacher_node@librem} !
  {get_friends, Self, Ref},
  {friends, Ref}.

ask_friend(Main, Nodes) ->
  Ref = make_ref(),
  io:format("I only have ~p friends. Ask a friend "
            "for more friends~n",
            [length(Nodes)]),
  lists:nth(rand:uniform(length(Nodes)), Nodes) !  {get_friends, Main, Ref},
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
           spawn(fun () -> storage_loop([], [], [], []) end)),
  loop([], []).

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

test_transaction() ->
  jsparber_node ! {push, {make_ref(), "Some random data"}}.

test() ->
  spawn(fun main/0),
  sleep(20),
  test_transaction(),
  test_transaction(),
  test_transaction(),
  test_transaction(),
  test_transaction(),
  test_transaction(),
  test_transaction(),
  test_transaction(),
  test_transaction().
