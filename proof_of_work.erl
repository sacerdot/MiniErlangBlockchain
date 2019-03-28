-module(proof_of_work).
-export([solve/1,check/2]).

% This module implements proof-of-work:
%  - Result = solve(Transactions)
%    an hard* computational problem
%  - check(Transactions,Result)
%    quickly returns true iff the solution is correct
%
%  * hardness depends on the size of the input
%    and on the magic number DIFFICULTY below.
%
%  solve looks for the smallest natural Result s.t. the hash
%  of {Transactions,Result} is smaller or equal than ?DIFFICULTY

-define(DIFFICULTY,8).

check(Transactions,Solution) ->
   erlang:phash2({Solution,Transactions}) =< ?DIFFICULTY.

solve(Transactions) -> solve(Transactions,0).

solve(Transactions,Solution) ->
   case check(Transactions,Solution) of
      true  -> Solution ;
      false -> solve(Transactions,Solution+1)
   end.

