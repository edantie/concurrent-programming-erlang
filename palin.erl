-module(palin).
-export([palin/1, nopunct/1, palindrome/1, palindrome_check/1]).

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


server(Pid) ->
  receive
    {check, Msg} ->        %Check for a palindrome.
      Pid ! {result,
        lists:flatten(
          io_lib:format(
            "\"~s\"~sa palindrome.",
            [Msg,
              case (palindrome_check(Msg)) of
                true -> " is ";
                false -> " is not "
              end]))},
      server(Pid);
    _ ->          %Stop for ANY other message.
      ok
  end.