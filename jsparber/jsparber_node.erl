-module(jsparber_node).

-export([main/0, test/0]).

% This is blockchain node, based on teacher_node.
% TODO: Clean up heads and blocks
% TODO: Maintain a friends list of exactly 3 nodes
% TODO: Ask all friends for more friends before reasking the teacher

-define(TEACHER_NODE, {teacher_node, teacher_node@librem}).
-define(DEAD_TOLERANZ, 0).
%Timeout for request in seconds
-define(TIMEOUT, 2).

msg_anomaly(Sender, Msg) ->
  X = rand:uniform(10),
  if ( X == 1 ) -> none; %Lost msg
     ( X == 10 ) -> Sender ! Msg, Sender ! Msg; %Duplicate msg
     true -> Sender ! Msg %No anomaly
  end.

sleep(N) ->
  receive
  after N * 1000 -> ok
  end.

watch(Main, Node) -> spawn(fun () -> watch(Main, Node, ?DEAD_TOLERANZ) end).
% We need to sleep after checking so we check impideatly new nodes
watch(Main, Node, Toleranz) ->
  Ref = make_ref(),
  Node ! {ping, self(), Ref},
  receive
    {pong, Ref} -> sleep(10), watch(Main, Node, ?DEAD_TOLERANZ)
  after 2000 ->
          case Toleranz =< 0 of
            true ->  Main ! {dead, Node};
            false -> sleep(10), watch(Main, Node, Toleranz - 1)
          end
  end.

% Request friends (get_friends)
get_friends(Main, Dest) ->
  Nonce = make_ref(),
  Dest ! {get_friends, Main, Nonce},
  io:format("Sent get_friends to ~p with Nonce ~p~n", [Dest, {friends, Nonce}]),
  {friends, Nonce}.

% Request previous Block (get_previous)
% Keep track of the Nonce and remake the request when no reply within ?TIMEOUT
get_previous(Main, Dest, Block_id) ->
  spawn(fun () ->
            Nonce = make_ref(),
            msg_anomaly(Dest, {get_previous, self(), Nonce, Block_id}),
            receive
              {previous, Nonce, Block} -> Main ! {add_previous, Block}
            after ?TIMEOUT * 1000 -> Main ! {failed, get_previous, Block_id}
            end
        end).

% Request chain Head (get_head)
% Keep track of the Nonce and remake the request when no reply within ?TIMEOUT
get_head(Main, Dest) ->
  spawn(fun () ->
            Nonce = make_ref(),
            msg_anomaly(Dest, {get_head, self(), Nonce}),
            receive
              {head, Nonce, Block} -> Main ! {add_head, Block}
            after ?TIMEOUT * 1000 -> Main ! {failed, get_head}
            end
        end).

% Send update (update)
% We don't need to track this message
update(Main, Dest, Block) ->
  Dest ! {update, Main, Block}.

% Send push (update)
% We don't need to track this message
push(Dest, Transaction) ->
  Dest ! {push, Transaction}.

friends_loop() ->
  Main = self(),
  spawn(fun () -> friends_loop(Main, []) end).
friends_loop(Main, Asked_Nodes) ->
  sleep(5),
  io:format("Check friends~n"),
  Main ! {check_friends, self()},
  receive
    {need_friends, Nodes} ->
      io:format("Response need_friends~n"),
      % Ask each friend till we get 3 friends
      Node = get_random_friend_or_teacher(Nodes -- Asked_Nodes),
      % We need to make request
      Main ! {request_friends, Node},
      friends_loop(Main, [Node | Asked_Nodes]);
    {enough_friends} ->
      io:format("Enough friends~n"),
      friends_loop(Main, [])
  end.

% This sends remove_nonces with Nonces after a timeout of TIMEOUT to the main loop
nonces_cleaner(Main, Nonces) ->
	spawn(fun () ->
						sleep(?TIMEOUT),
						Main ! {remove_nonce, Nonces}
				end),
	Nonces.

loop(Nodes, Nonces) ->
	receive
		% Internal communication to keep friends
		{check_friends, Sender} ->
			case length(Nodes) < 3 of
        true -> Sender ! {need_friends, Nodes};
        false -> Sender ! {enough_friends}
      end,
      loop(Nodes, Nonces);
    {request_friends, Node} ->
      loop(Nodes, Nonces ++ nonces_cleaner(self(), [get_friends(self(), Node)]));
    {remove_nonce, Rm_nonces} ->
      loop(Nodes, Nonces -- Rm_nonces);

    % Reply for friendes requests
    {ping, Sender, Ref} ->
      Sender ! {pong, Ref}, loop(Nodes, Nonces);
    {get_friends, Sender, Nonce} ->
      Sender ! {friends, Nonce, Nodes},
      loop(Nodes, Nonces);
   	{get_previous, Sender, Nonce, Prev_block_id} ->
      storage  ! {get_previous, Nonce, Sender, Prev_block_id},
      loop(Nodes, Nonces);
    {get_head, Sender, Nonce} ->
      storage ! {get_head, Sender, Nonce},
      loop(Nodes, Nonces);

    % Update Nodes storage
    {friends, Nonce, Incomming_nodes} ->
      case lists:member({friends, Nonce}, Nonces) of
        true -> loop(add_nodes(Incomming_nodes, Nodes), Nonces -- [Nonce]);
        false -> io:format("INVALID MESSAGE: We got a wrong nonce "
                    "with for friends~n"),
          loop(Nodes, Nonces)
      end;
    {dead, Node} ->
      io:format("Dead node ~p~n", [Node]),
      loop(Nodes -- [Node], Nonces);

    % Update Block and Transaction storage
    {update, Block} ->
      storage ! {add_block, Block},
      loop(Nodes, Nonces);
    {push, Transaction} ->
      storage ! {add_transaction, Transaction},
      loop(Nodes, Nonces);

    {add_previous, Block} ->
       storage ! {add_previous_block, Block},
       loop(Nodes, Nonces);
    {add_head, Nonce, Block} ->
      storage ! {add_block, Block},
      loop(Nodes, Nonces);

    %Gossip
    {gossip, Data} ->
      send_all(Nodes, Data),
      loop(Nodes, Nonces);

    % Show is used for debuging
    {show} ->
      io:format("Number of nodes ~p~n", [length(Nodes)]),
      loop(Nodes, Nonces)
  end.

storage_loop(Blocks, Heads, Transactions, Trans_mining) ->
  io:format("Heads ~p~n", [Heads]),
  receive
    {add_head, New_head, Remove_head} ->
      io:format("New head~n"),
      storage_loop(Blocks, [New_head | Heads -- [Remove_head]], Transactions, Trans_mining);
    {add_previous_block, Block} ->
      {Final_blocks, Final_trans} = case check_block(Blocks, Block) of
                                      true ->
                                        New_blocks = [Block | Blocks],
                                        % Explore chains with any missing previous block
                                        explore_all_chains(New_blocks, Heads),
                                        {New_blocks, remove_transactions(Transactions, Block)};
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
                                                            {New_blocks, remove_transactions(Transactions, Block), remove_transactions(Trans_mining, Block)};
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
          io:format("Number of transections~p~n", [length(New_transactions)]),
          storage_loop(Blocks, Heads, Remaing_transactions, Mining_transactions);
        true -> storage_loop(Blocks, Heads, Transactions, Trans_mining)
      end;
    {get_previous, Nonce, Sender, Prev_block_id} ->
      msg_anomaly(Sender,
                  {previous, Nonce, get_block(Blocks, Prev_block_id)}),
      storage_loop(Blocks, Heads, Transactions, Trans_mining);
    {get_head, Sender, Nonce} ->
      msg_anomaly(Sender, {head, Nonce, get_longest_head(Heads)}),
      storage_loop(Blocks, Heads, Transactions, Trans_mining)
  end.

% This takes a list of nodes and sends a get_head request to every node
% It returns a list of Nonces
request_head_all([]) -> [];
request_head_all([H | T]) ->
  Nonce = make_ref(),
  msg_anomaly(H , {get_head, self(), Nonce}),
  [{head, Nonce} | request_head_all(T)].

send_all([], _) -> none;
send_all([H | T], Data) -> msg_anomaly(H , Data), send_all(T, Data).

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
  io:format("Start exploring chain ~p Prev: ~p~n", [Head_id, Prev_head_id]),
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
          storage ! {add_head, {Head_to_insert, 1}, []},
          jsparber_node ! {request_previous, Prev_head_id}
      end
  end.

get_block_by_id(Blocks, Id) ->
  lists:keyfind(Id, 2, Blocks).

mine_block({Id, Transactions}) ->
  io:format("Started mining~n"),
  Solution = proof_of_work:solve({Id, Transactions}),
  io:format("Finished mining~n"),
  storage ! {add_block, {make_ref(), Id, Transactions, Solution}}.

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
remove_transactions(Transections, {_, _, List_of_transection, _}) ->
  Transections -- List_of_transection.

% Return longest head out of a list of heads of form {Block_id, Length}
get_longest_head(Heads) ->
  {Head_id, _} = get_longest_head(Heads, {none, 0}),
  io:format("Longest head ~p~n", [Head_id]),
  Head_id.

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

% Returns a random friend from Nodes or TEACHER_NODE if there are no friends
get_random_friend_or_teacher([]) -> ?TEACHER_NODE;
get_random_friend_or_teacher(Nodes) -> get_random_friend(Nodes).

% Returns a random friend from Nodes
get_random_friend([]) -> none;
get_random_friend(Nodes) -> lists:nth(rand:uniform(length(Nodes)), Nodes).

add_nodes([], Nodes) -> Nodes;
add_nodes(New_nodes, Nodes) ->
  H = get_random_friend(New_nodes),
  if
    H == none ->
      Nodes;
    length(Nodes) > 3 ->
      Nodes;
    self() /= H ->
       case lists:member(H, Nodes) of
         true -> add_nodes(New_nodes -- [H], Nodes);
         false ->
           io:format("New node ~p~n", [H]),
           Self = self(),
           watch(Self, H),
           add_nodes(New_nodes -- [H], [H | Nodes])
       end;
     true -> add_nodes(New_nodes -- [H], Nodes)
  end.

main() ->
  register(jsparber_node, self()),
  %global:register_name(jsparber_node, self()),
  io:format("A new jsparber_node registered~n"),
  register(storage,
           spawn(fun () -> storage_loop([], [], [], []) end)),
  friends_loop(),
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
  test_transaction(),
  test_transaction().
