%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0, start/0]).

%% Usage in shell :
%%  f(),c(frequency).
%%  frequency:start().
%%  frequency ! {request,  self(),  allocate}.
%%  flush().
%%  frequency ! {request,  self(),  allocate}.
%%  flush().
%%  frequency ! {request, self(), {deallocate, 9999}}.
%%  flush().
%%  frequency ! {request, self(), {deallocate, 10}}.
%%  flush().
%%  frequency ! {request, self(), {deallocate, 10}}.
%%  flush().
%%  frequency ! {request, self(), stop }.


%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(?MODULE, Pid = spawn(?MODULE, init, [])),
  Pid.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10, 11, 12, 13, 14, 15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);

    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);

    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};

allocate({Frequencies = [Freq | Free], Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    false ->
      {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}};
    _ ->
      {{Frequencies, Allocated}, {error, pid_has_already_allocated}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:keyfind(Pid, 1, Allocated) of
    {Freq, Pid} ->
      {[Freq | Free], lists:keydelete(Freq, 1, Allocated)};

    _ ->
      {{Free, Allocated}, {error, freq_was_not_allocated}}
  end.
