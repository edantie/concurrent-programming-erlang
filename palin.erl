-module(palin).
-export([palin/1, nopunct/1, palindrome/1, palindrome_check/1, server/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
  palin(nocaps(nopunct(Xs))).

nopunct([]) ->
  [];
nopunct([X | Xs]) ->
  case lists:member(X, ".,\ ;:\t\n\'\"") of
    true ->
      nopunct(Xs);
    false ->
      [X | nopunct(Xs)]
  end.

nocaps([]) ->
  [];
nocaps([X | Xs]) ->
  [nocap(X) | nocaps(Xs)].

nocap(X) ->
  case $A =< X andalso X =< $Z of
    true ->
      X + 32;
    false ->
      X
  end.

% literal palindrome

palin(Xs) ->
  Xs == reverse(Xs).

reverse(Xs) ->
  shunt(Xs, []).

shunt([], Ys) ->
  Ys;
shunt([X | Xs], Ys) ->
  shunt(Xs, [X | Ys]).


rem_punct(String) -> lists:filter(fun(Ch) ->
  not(lists:member(Ch, "\"\'\t\n "))
                                  end,
  String).

to_small(String) -> lists:map(fun(Ch) ->
  case ($A =< Ch andalso Ch =< $Z) of
    true -> Ch + 32;
    false -> Ch
  end
                              end,
  String).

palindrome_check(String) ->
  Normalise = to_small(rem_punct(String)),
  lists:reverse(Normalise) == Normalise.


%Custom functions

build_result({true, S}) ->
  {result, string:concat(S, " is a palindrome")};
build_result({_, S}) ->
  {result, string:concat(S, " is NOT a palindrome")}.

server(Pid) ->
  receive
    {check, S} ->
      Pid ! build_result({palindrome_check(S), S}),
      server(Pid);
    _ ->
      io:format("stopped~n")
  end.
