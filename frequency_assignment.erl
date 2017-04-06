%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_assignment).
-export([start/0, allocate/0, deallocate/1, stop/0, test/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency_assignment, spawn(frequency_assignment, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10, 11, 12, 13, 14, 15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      timer:sleep(1200), % Simulate an overloaded server
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, {deallocate, Freq}} ->
      timer:sleep(1200), % Simulate an overloaded server
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() ->
  clear(),
  frequency_assignment ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  % In case the frequency server is heavily loaded
  after 1000 ->
    timeout
  end.

deallocate(Freq) ->
  clear(),
  frequency_assignment ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Reply} -> Reply
  % In case the frequency server is heavily loaded
  after 1000 ->
    timeout
  end.

stop() ->
  clear(),
  frequency_assignment ! {request, self(), stop},
  receive
    {reply, Reply} -> Reply
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq | Free], NewAllocated}.

% Flushing the mailbox
clear() ->
  receive
    Msg ->
      % Print what's being cleared
      io:format("Clearing message ~p~n~n", [Msg]),
      clear()
  after 0 ->
    timeout
  end.

% Function to test module
test() ->
  start(),

  % first request to allocate. Should be allocated the frequency 10.
  {reply, {ok, 10}} = test_allocate(),

  % then request to deallocate. Should return an ok
  {reply, ok} = test_deallocate(10),

  stop_server().


% function to test a allocate and wait for response
test_allocate() ->
  frequency_assignment ! {request, self(), allocate},
  receive
    Any ->
      Any
  end.

% function to test a deallocate and wait for response
test_deallocate(Freq) ->
  frequency_assignment ! {request, self(), {deallocate, Freq}},
  receive
    Any ->
      Any
  end.

% Stop the server and unregister frequency_assignment
stop_server() ->
  frequency_assignment ! {request, self(), stop},
  unregister(frequency_assignment),
  receive
    {reply, stopped} -> true
  end.
