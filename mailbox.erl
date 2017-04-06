-module(mailbox).

-export([receiver_in_sequence/0, receiver_in_order/0]).

receiver_in_sequence() ->
  receive
    msg1 -> io:format("~s~n", [msg1])
  end,
  receive
    msg2 -> io:format("~s~n", [msg2])
  end.



receiver_in_order()->
  receive
    Msg ->
      io:format("~s~n",[Msg]), %% prints the pulled msg from the mailbox.
      timer:sleep(10000), %% pulls out each msg from the mailbox after 10 seconds.
      receiver_in_order() %% recursive call
  end.