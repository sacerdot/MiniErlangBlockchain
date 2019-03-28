-module(proof_of_work).
-export([solve/1,check/2]).

% This module implements proof-of-work:
%  - Result = solve(Input)
%    an hard* computational problem
%  - check(Input,Result)
%    quickly returns true iff the solution is correct
%
%  * hardness depends on the size of the input
%    and on the magic number DIFFICULTY below.
%
%  solve looks for the smallest natural Result s.t. the hash
%  of {Input,Result} is smaller or equal than ?DIFFICULTY

-define(DIFFICULTY,8).

check(Input,Solution) ->
   erlang:phash2({Solution,Input}) =< ?DIFFICULTY.

solve(Input) -> solve(Input,0).

solve(Input,Solution) ->
   case check(Input,Solution) of
      true  -> Solution ;
      false -> solve(Input,Solution+1)
   end.
