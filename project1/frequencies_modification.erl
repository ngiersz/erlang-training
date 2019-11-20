-module(frequencies_modification).
-compile(export_all).

%% These are the start functions used to create and
%% initialize the server.
start() ->
    % List = [1,2,3,4],
    % io:format("~p~n", [List]),
    register(frequency_server, spawn(frequencies_modification, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded -> {Pid, Frequency}
get_frequencies() -> [10,11].
% get_frequencies() -> [{0, 10},{0, 11}].

% Phone
start_phone() ->
    Pid = spawn(frequencies_modification, init_phone, []),
    Name = "phone_" ++ [get_pid_value(Pid)],
    register(list_to_atom(Name), Pid),
    Pid.

get_pid_value(Pid) ->
    PidStr = pid_to_list(Pid),
    PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
    [N, P1, P2] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
    P1.

init_phone() ->
    Frequencies = [],
    loop_phone(Frequencies).


loop_phone(Frequencies) ->
    receive
        hello ->
            io:format("Hello from phone!~n"),
            loop_phone(Frequencies);
        allocate ->
            % io:format("~p~n", [Frequencies]),
            % {NewFrequencies, Reply} = allocate(Frequencies, self()),
            {NewFrequencies, Reply} = call(self(), allocate),
            % reply(self(), Reply),
            % io:format("~p~n", [NewFrequencies]),
            io:format("~p~n", [Reply]),
            loop_phone(NewFrequencies);
            % loop_phone(Frequencies);
        {request, Pid , {deallocate, Freq}} ->
            erlang:display(Frequencies),
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop_phone(NewFrequencies);
        stop ->
            reply(self(), ok)
    end.

% Client
stop() -> call(self(), stop).

% allocate() -> call(allocate).

% deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.
call(Pid, Message) ->
    frequency_server ! {request, Pid, Message},
    receive
        {reply, Reply} -> Reply
    end.

% /Client

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.
% allocate({[], Allocated}, _Pid) ->
%     {{[], Allocated}, {error, no_frequency}};
% allocate({[Freq|Free], Allocated}, Pid) ->
%     {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

% deallocate({Free, Allocated}, Freq, Pid) ->
%     case {lists:member({Freq, Pid}, Allocated)} of 
%         {true} -> true;
%         {false} -> false
%     end.
%     % NewAllocated=lists:keydelete({Freq, Pid}, 1, Allocated),
%     % {[Freq|Free], NewAllocated}.

