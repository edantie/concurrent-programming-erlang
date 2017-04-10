%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(week2).

-export([init/1, supervisor/0, init_supervisor/1, main_supervisor/0]).

% Start the main supervisor: the supervisor that supervise the supervisor of the server
% (if this one died then it will restart the supervisor)
supervisor() ->
  register(main_supervisor, Pid = spawn(?MODULE, main_supervisor, [])),
  Pid.

main_supervisor() ->
  % Not killed if a trap is received
  process_flag(trap_exit, true),

  % Init frequencies
  Frequencies = {get_frequencies(), []},

  % Start the supervisor of the supervisor
  start_supervisor_of_frequency_server(Frequencies),

  % Loop for supervisor messages
  loop_main_supervisor(Frequencies).

% Just wait for supervisor died messages
loop_main_supervisor(Frequencies) ->
  SupervisorPid = whereis(supervisor),
  receive
    {'EXIT', SupervisorPid, _Reason} ->
      start_supervisor_of_frequency_server(Frequencies),
      loop_main_supervisor(Frequencies);
    _ ->
      loop_main_supervisor(Frequencies)
  end.

% Start the supervisor of the frequency server
start_supervisor_of_frequency_server(Frequencies) ->
  io:format("(Re)Start the supervisor of the frequency server~n"),
  register(supervisor, Pid = spawn_link(?MODULE, init_supervisor, [Frequencies])),
  Pid.

% init the supervisor of the frequency server
init_supervisor(Frequencies) ->
  % Not killed if a trap is received
  process_flag(trap_exit, true),

  % Start the frequency server
  start_server(Frequencies),

  % Loop for supervisor messages
  loop_supervisor(Frequencies).

% Loop for supervisor messages
loop_supervisor(Frequencies) ->
  FreqPid = whereis(frequency),
  receive
  % Information message
    {info, NewFrequencies} ->
      loop_supervisor(NewFrequencies);

  % Server has died... Restart one with last known frequencies
    {'EXIT', FreqPid, _Reason} ->
      % restart server with actual frequencies
      start_server(Frequencies),
      loop_supervisor(Frequencies);
  % Whatever
    _ ->
      loop_supervisor(Frequencies)
  end.


%% These are the start functions used to create and
%% initialize the server.

% Now we keep the frequencies
start_server(Frequencies) ->
  io:format("(Re)Start the server server~n"),
  register(frequency, spawn_link(?MODULE, init, [Frequencies])).

init(Frequencies) ->
  process_flag(trap_exit, true),
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10, 11, 12, 13, 14, 15].

%% The Main Loop

loop(Frequencies) ->
  supervisor ! {info, Frequencies},
  SupervisorPid = whereis(supervisor),
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
      Pid ! {reply, stopped};

  %% The supervisor died
    {'EXIT', SupervisorPid, Reason} ->
      io:format("Supervisor died ~p~n", [Reason]),
      %% So we commit suicide...
      exit(killed);

  %% The client at Pid died
    {'EXIT', Pid, Reason} -> % A client died
      io:format("Client died ~p~n", [Reason]),
      loop(exited(Frequencies, Pid))
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};

allocate({Frequencies = [Freq | Free], Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    false ->
      % With the link we assure that if server died, client also does
      link(Pid),
      {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}};
    _ ->
      {{Frequencies, Allocated}, {error, pid_has_already_allocated}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:keyfind(Pid, 1, Allocated) of
    {Freq, Pid} ->
      % Not killing client anymore with the server
      unlink(Pid),
      {[Freq | Free], lists:keydelete(Freq, 1, Allocated)};
    _ ->
      {{Free, Allocated}, {error, freq_was_not_allocated}}
  end.

%% Called when a client died (force a deallocate)
exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of
    {value, {Freq, Pid}} ->
      {[Freq | Free], lists:keydelete(Freq, 1, Allocated)};
    false ->
      {Free, Allocated}
  end.