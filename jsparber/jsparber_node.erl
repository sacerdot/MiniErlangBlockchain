-module(jsparber_node).
-export([main/0, test/0, push/1]).
-on_load(load_externals_modules/0).

load_externals_modules() ->
    compile:file('proof_of_work.erl'),
ok.

% This is a blockchain node, based on teacher_node.
% Currently we need at least one friend to create chain

-define(NODE, jsparber_node).
-define(TEACHER_NODE, {teacher_node, teacher_node@librem}).
-define(DEAD_TOLERANZ, 0).
-define(REQUEST_TRIES, 10).
%Timeout for request in seconds
-define(TIMEOUT, 2).

msg_anomaly(Sender, Msg) ->
  X = rand:uniform(10),
  if
    % If sender is none then don't do anything
    Sender == none -> none;
    ( X == 1 ) -> none; %Lost msg
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
% Keep track of the Nonce and reply failed if now response within ?TIMEOUT
get_previous(Reply, Storage, Block_id) ->
  spawn(fun () ->
            Nonce = make_ref(),
            ?NODE ! {ask, {get_previous, self(), Nonce, Block_id}},
            receive
              {previous, Nonce, none} -> io:format("Didn't find block~p~n", [Block_id]), Reply ! {result, none};
              {previous, Nonce, Block} -> case check_block(Block) of
                                            true -> Reply ! {result, Block},
                                                    % Add the block also to the block storage
                                                    Storage ! {add, Block};
                                            false -> Reply ! {result, failed}
                                          end
            after ?TIMEOUT * 1000 -> Reply ! {result, timeout}
            end
        end).

% Request chain Head (get_head)
% Keep track of the Nonce and remake the request when no reply within ?TIMEOUT
get_head(Reply, Storage) ->
  spawn(fun () ->
            io:format("Request head from a friend~n"),
            Nonce = make_ref(),
            ?NODE ! {ask, {get_head, self(), Nonce}},
            receive
              {head, Nonce, none} -> io:format("Got none as a head~n"), Reply ! {result, none};
              {head, Nonce, Block} -> case check_block(Block) of
                                        true -> {Head_id, _, _, _} = Block,
                                          Reply ! {result, Head_id},
                                                % Add the block also to the head storage
                                                Storage ! {add, Block};
                                        false -> Reply ! {result, failed}
                                      end
            after ?TIMEOUT * 1000 -> Reply ! {result, timeout}
            end
        end).

% Send push (update)
% We don't need to track this message
push(Transaction) ->
  jsparber_node ! {push, Transaction}.

% Loop to maintain 3 friends, it checks every 5 secounds if there are enough friends
friends_loop() ->
  Main = self(),
  spawn(fun () -> friends_loop(Main, []) end).
friends_loop(Main, Asked_Nodes) ->
  sleep(5),
  io:format("Check friends~n"),
  Main ! {check_friends, self()},
  receive
    {need_friends, Nodes} ->
      io:format("Need more friends~n"),
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

loop() -> loop([], []).
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
      block_storage ! {get_previous, Nonce, Sender, Prev_block_id},
      loop(Nodes, Nonces);
    {get_head, Sender, Nonce} ->
      head_storage ! {get_head, Sender, Nonce},
      loop(Nodes, Nonces);

    % Update Nodes storage
    {friends, Nonce, Incomming_nodes} ->
      {New_nodes, New_nonces} = case lists:member({friends, Nonce}, Nonces) of
                                  true -> {add_nodes(Incomming_nodes, Nodes), Nonces -- [Nonce]};
                                  false -> io:format("INVALID MESSAGE: We got a wrong nonce "
                                                     "with for friends~n")
                                end,
      loop(New_nodes, New_nonces);
    {dead, Node} ->
      io:format("Dead node ~p~n", [Node]),
      loop(Nodes -- [Node], Nonces);

    % Update Transaction storage
    {push, Trans} ->
      transaction_storage ! {add, Trans},
      loop(Nodes, Nonces);

    % Update Block storage
    {update, Block} ->
      head_storage ! {add, Block},
      loop(Nodes, Nonces);

    % Ask a friend for a Block or Head, in other words just send Data to a random friend
    {ask, Data} ->
      msg_anomaly(get_random_friend(Nodes), Data),
      loop(Nodes, Nonces);
    %Gossip
    {gossip, Data} ->
      send_all(Nodes, Data),
      loop(Nodes, Nonces)
  end.

get_block(Reply, Blocks, Block_id) ->
  case get_block_by_id(Blocks, Block_id) of
    false -> get_previous(Reply, self(), Block_id);
    Block -> Reply ! {result, Block}
  end.

% Keep track of all blocks, drop allows to remove all Blocks and replace them with a new list of Blocks
block_storage() ->
  register(block_storage,
           spawn(fun () -> block_storage([]) end)).
block_storage(Blocks) ->
  receive
    {add, Block} ->
      New_blocks = case is_new_block(Blocks, Block) of
                     true -> {_, _, List_of_transaction, _} = Block,
                             transaction_storage ! {remove, List_of_transaction},
                             [Block | Blocks];
                     false -> Blocks
                   end,
      block_storage(New_blocks);
    {clean, Head_id} -> collect_chain(Head_id), block_storage(Blocks);
    {replace, New_blocks} -> block_storage(New_blocks);
    {get_all, Reply} -> Reply ! {result, Blocks}, block_storage(Blocks);
    {get, Reply, Block_id} -> 
      get_block(Reply, Blocks, Block_id),
                              block_storage(Blocks);
    %Reply for get_previous friends requests
    {get_previous, Nonce, Sender, Block_id} ->
      Reply = case get_block_by_id(Blocks, Block_id) of
                false -> none;
                Block -> Block
              end,
      msg_anomaly(Sender, {previous, Nonce, Reply}),
      block_storage(Blocks)
  end.

collect_chain(Head_id) ->
  spawn(fun() ->
  collect_chain(Head_id, [])
        end).
collect_chain(Head_id, Blocks) ->
  case yield_block_by_id(Blocks, Head_id) of
        {Block_id, Prev_block_id, List, Solution} -> collect_chain(Prev_block_id, {Block_id, Prev_block_id, List, Solution} );
        none -> block_storage ! {replace, Blocks}
  end.



transaction_storage() ->
  register(transaction_storage,
           spawn(fun () -> transaction_storage([], []) end)).
transaction_storage(Trans, Mining_trans) ->
  receive
    {add, T} ->
      io:format("Add new transaction ~p~n", [T]),
      N_trans = case is_new_transaction(yield_blocks(), Trans ++ Mining_trans, T) of
                  true -> % We need to gossip the new Transaction
                    ?NODE ! {gossip, {push, T}},
                    [T | Trans];
                  false -> Trans
                end,
      {New_trans, New_mining_trans} = case length(N_trans) > 9 of
                                        true ->
                                          {Block_trans, Remaing_trans} = lists:split(10, N_trans),
                                          mine_block(Block_trans),
                                          {Remaing_trans, Mining_trans ++ Block_trans};
                                        false -> {N_trans, Mining_trans}
                                      end,
      transaction_storage(New_trans, New_mining_trans);
    {remove, T} -> transaction_storage(Trans -- T, Mining_trans -- T)
  end.

% reply with head or request it from an other node if no head is avaible
get_chain(Reply, Heads) ->
  case get_longest_head(Heads) of
    none -> get_head(Reply, self());
    false -> get_head(Reply, self());
    Head -> Reply ! {result, Head}
  end.

head_storage() ->
  register(head_storage,
           spawn(fun () -> head_storage([]) end)).
head_storage(Heads) ->
  receive
    {int_add, {Id, Length}} ->
      io:format("Internal Add Head ~p~n", [Id]),
      {Old_id, Old_length} = get_longest_head(Heads, {none, 0}),
      % We only need to store the longest head
      New_heads = case Length > Old_length of
                    true -> block_storage ! {clean, Id},
                    [{Id, Length}];
                    false -> [{Old_id, Old_length}]
                  end,
      io:format("New longest head ~p~n", [New_heads]),
      head_storage([{Id, Length} | Heads]);
    {add, Head} ->
      case is_new_block(yield_blocks(), Head) of
        true ->
      io:format("Add new head ~p~n", [Head]),
      block_storage ! {add, Head},
      % We need to gossip the new block
      jsparber_node ! {gossip, {update, Head}},
      explore_chain(Head);
        false -> none
      end,
      head_storage(Heads);
    {get_longest, Reply} -> get_chain(Reply, Heads),
                            head_storage(Heads);

    {get, Reply, Block_id} ->
      io:format("Return get head~n"),
      Reply ! {result, get_head_by_id(Heads, Block_id)},
      head_storage(Heads);

    %Reply for get_head friends requests
    {get_head, Sender, Nonce} ->
      % yield_block_by_id could take a way to reply, but shouldn't be a problem
      This_head = get_longest_head(Heads),
      This = yield_block_by_id(This_head),
      io:format("respnded with this head ~p and block ~p~n", [This_head, This]),
      msg_anomaly(Sender, {head, Nonce, This}),
      head_storage(Heads)
  end.

send_all([], _) -> none;
send_all([H | T], Data) -> msg_anomaly(H , Data), send_all(T, Data).

% Explore chain starting with the new Head
explore_chain(none) -> none;
explore_chain({Head_id, Prev_head_id, _, _}) ->
  Main = self(),
  spawn(fun () ->
            io:format("Start exploring chain ~p Prev: ~p~n", [Head_id, Prev_head_id]),
            explore_chain(Main, Head_id, Prev_head_id, 1)
        end).

% In case we don't have the explored chain at all (we are hiting none)
explore_chain(Main, Head_to_insert, none, Depth) ->
  Main ! {int_add, {Head_to_insert, Depth}};
explore_chain(Main, Head_to_insert, Prev_head_id, Depth) ->
  case yield_head_by_id(Prev_head_id) of
    {_, Length} ->
      %send new head to storage and the head to replace
      Main ! {int_add, {Head_to_insert, Depth + Length}};
    false ->
      io:format("No head, looking at old blocks, explore ~p ~n", [Prev_head_id]),
      case yield_block_by_id(Prev_head_id) of
        {_, Prev_block_id, _, _} ->
          io:format("Go deeper ~p~n", [Prev_block_id]),
          explore_chain(Main, Head_to_insert, Prev_block_id, Depth + 1);
        _ -> io:format("Block with id ~p doesn't exist. Drop chain.~n", [Prev_head_id])
      end
  end.

get_block_by_id(Blocks, Id) ->
  lists:keyfind(Id, 1, Blocks).

get_head_by_id(Heads, Id) ->
  lists:keyfind(Id, 1, Heads).

% Request head by id from head_storage
% Keep requesting a head till a vaild one is found
yield_head_by_id(Id) ->
  head_storage ! {get, self(), Id},
  receive
    {result, R} -> R
  end.

% Request longest head from head_storage
% Keep requesting a head till a vaild one is found
yield_longest_head() ->
  head_storage ! {get_longest, self()},
  receive
    {result, timeout} -> io:format("get head timeout"), yield_longest_head();
    {result, failed} -> io:format("No valid block as head: replying with none"), none;
    {result, R} -> R
  end.

% Request all blocks from block_storage
yield_blocks() ->
  block_storage ! {get_all, self()},
  receive
    {result, R} -> R
  end.

% Request all blocks from block_storage
yield_block_by_id(Block_id) ->
  yield_block_by_id(Block_id, 0).
yield_block_by_id(Block_id, Try) ->
  block_storage ! {get, self(), Block_id},
  receive
    {result, timeout} -> retry(Try, yield_block_by_id(Block_id, Try + 1));
    {result, failed} -> retry(Try, yield_block_by_id(Block_id, Try + 1));
    {result, R} -> R
  end.

retry(Try, Work) ->
  case Try > ?REQUEST_TRIES of
    true -> none;
    false -> Work
  end.

mine_block(Transactions) ->
  spawn(fun () ->
            Id = yield_longest_head(),
            io:format("Started mining~n"),
            Solution = proof_of_work:solve({Id, Transactions}),
            io:format("Finished mining~n"),
            Block_id = make_ref(),
            head_storage ! {add, {Block_id, Id, Transactions, Solution}}
        end).

% Checks if block is valid
check_block({_, Prev_block_id, List_of_transection, Solution}) ->
  io:format("Check block~p~n", [{Prev_block_id, Solution}]),
  proof_of_work:check({Prev_block_id, List_of_transection}, Solution).

% Checks if block is new
is_new_block(Blocks, {Block_id, _, _, _}) ->
  case get_block_by_id(Blocks, Block_id) of
    false -> true;
    _ -> false
  end.

% Checks if a transection is new
is_new_transaction(Blocks, Transactions, T) ->
  case lists:member(T, Transactions) of
    false -> is_new_transaction_blocks(Blocks, T);
    true -> false
  end.

is_new_transaction_blocks([], _) -> true;
is_new_transaction_blocks([{_, _, List_of_transection, _} | T],
                          {Trans_id, Payload}) ->
  case lists:keyfind(Trans_id, 1, List_of_transection) of
    false -> is_new_transaction_blocks(T, {Trans_id, Payload});
    true -> false
  end.

% Return longest head out of a list of heads of form {Block_id, Length}
get_longest_head(Heads) ->
  {Head_id, _} = get_longest_head(Heads, {none, 0}),
  io:format("Longest head ~p~n", [Head_id]),
  Head_id.

get_longest_head([], Max) -> Max;
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
  register(?NODE, self()),
  %global:register_name(jsparber_node, self()),
  io:format("A new jsparber_node registered~n"),
  friends_loop(),
  block_storage(),
  head_storage(),
  transaction_storage(),
  loop().

%%%%%%%%%%%%%%%% Testing only, do not use! %%%%%%%%%%%%%%%%%%%%

test_transaction() ->
  jsparber_node ! {push, {make_ref(), "Some random data"}}.

test() ->
  spawn(fun main/0),
  sleep(10),
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
