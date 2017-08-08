-module(sandbox).

-export([
    start_link/0,
    insert/1, insert/2,
    priority_read/0,
    read/1
]).

%% Demo
-export([
    start/0,
    insert_loop/3,
    priority_read_loop/1
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, sandbox_state).
-record(?STATE, {
    pqueue
}).

%% ---------------
% -20 HIGHEST priority, +20 LOWEST priority

%% insert to the highest priority
insert(Item) ->
    insert(Item, -20).

%% insert to a specific priority
insert(Item, Priority) ->
    gen_server:call(sandbox, {insert, Item, Priority}).

%% Highest priority first.
priority_read() ->
    gen_server:call(sandbox, {priority_read}).

read(Priority) ->
    gen_server:call(sandbox, {read, Priority}).

%% ---------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    {ok, #?STATE{pqueue=pqueue:new()}}.

handle_call({priority_read}, _From, #?STATE{pqueue=Q} = State) ->
    {V, NewQ} = pqueue:pout(Q),
    {reply, V, State#?STATE{pqueue = NewQ}};
handle_call({read, Priority}, _From, #?STATE{pqueue=Q} = State) ->
    {V, NewQ} = pqueue:out(Priority, Q),
    {reply, V, State#?STATE{pqueue = NewQ}};
handle_call({insert, Item, Priority}, _From, #?STATE{pqueue=Q} = State) ->
    {reply, ok, State#?STATE{pqueue = pqueue:in(Item, Priority, Q)}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------
%% Demo helpers

%% Here -20(Highest) priority reads happen faster than the lower prio reads
%% So the printout should be all the -20 ones first, then the -10 ones
start() ->
    {ok, _Pid} = sandbox:start_link(),
    insert_loop(500, -20, 50),
    insert_loop(1000, -10, 50),
    priority_read_loop(750).

insert_loop(_SleepTime, _Prio, Count) when Count =< 0 ->
    io:format("insert loop done!~n");
insert_loop(SleepTime, Prio, Count) ->
    ok = insert({erlang:now(), Prio}, Prio),
    timer:apply_after(SleepTime, ?MODULE, insert_loop, [SleepTime, Prio, Count-1]).

%% Read the highest priority
priority_read_loop(SleepTime) ->
    case priority_read() of
        empty ->
            io:format("priority_read empty~n"),
            timer:apply_after(SleepTime, ?MODULE, priority_read_loop, [SleepTime]);
        V ->
            io:format("PRIO READ:~p~n", [V]),
            timer:apply_after(SleepTime, ?MODULE, priority_read_loop, [SleepTime])
    end.

%% ---------------