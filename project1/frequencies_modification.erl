-module(frequencies_modification).
-compile(export_all).

%% These are the start functions used to create and
%% initialize the server.
start() ->
    register(frequency_server, spawn(frequencies_modification, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11].

% Phone
start_phone() ->
    Pid = spawn(frequencies_modification, init_phone, []),
    Name = "phone_" ++ [get_pid_value(Pid)],
    register(list_to_atom(Name), Pid),
    Pid.

get_pid_value(Pid) ->
    PidStr = pid_to_list(Pid),
    PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
    [_, P1, _] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
    P1.

init_phone() ->
    Frequencies = [],
    loop_phone(Frequencies).

loop_phone(_) ->
    receive
        allocate ->
            {NewFrequencies, Reply} = call(self(), allocate),
            io:format("Allocated frequency: ~p~n", [Reply]),
            loop_phone(NewFrequencies);
        {deallocate, Freq} ->
            NewFrequencies = call(self(), {deallocate, Freq}),
            loop_phone(NewFrequencies);
        stop ->
            ok
    end.

% Client
stop() -> call(self(), stop).

%% We hide all message passing and the message
%% protocol in a functional interface.
call(Pid, Message) ->
    frequency_server ! {request, Pid, Message},
    receive
        {reply, Reply} -> Reply
    end.


loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq, Pid),
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

deallocate({Free, Allocated}, Freq, Pid) ->
    IsMember = lists:member({Freq, Pid}, Allocated),
    if 
        IsMember == true -> io:format("Frequency can be allocated.~n"),
                            NewAllocated=lists:keydelete(Freq, 1, Allocated),
                            {[Freq|Free], NewAllocated};
        true -> io:format("Frequency can not be allocated.~n"),
                {Free, Allocated}
    end.