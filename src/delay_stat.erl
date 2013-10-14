-module(delay_stat).
-export([
    start/0,
    start_link/0,
    stop/0,
    code_change/3,
    handle_call/3,
    init/1,
    terminate/2,
    handle_info/2,
    handle_cast/2,
    current/0,
    update/1
]).

-behaviour(gen_server).

-record(delay_stat_state, {
          intervals = queue:new(),
          border = 0
      }).

-define(SHORT_INTERVAL, 500).
-define(LONG_INTERVAL, 15000).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, none, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(none) ->
    process_flag(priority, high),
    {ok, #delay_stat_state{}}.

handle_call(current, _From, #delay_stat_state{
        intervals = Intervals
    } = State) ->
    LastInterval = case queue:is_empty(Intervals) of
        true -> queue:new();
        _ -> queue:get_r(Intervals)
    end,
    ShortISumm = lists:foldl(fun(E, A) -> E + A end, 0, queue:to_list(LastInterval)),
    ShortI = case ShortISumm of
        0 -> 0;
        _ -> ShortISumm / queue:len(LastInterval)
    end,
    {LongISumm, LongIN} = lists:foldl(fun(I, A) -> lists:foldl(fun(E, {A2, N}) -> {E + A2, N + 1} end, A, queue:to_list(I)) end,
        {0, 0}, queue:to_list(Intervals)),
    LongI = case LongIN of
        0 -> 0;
        _ -> LongISumm / LongIN
    end,
    {reply, nmea_utils:pretty([{delay_500ms, round(ShortI/1000)}, {delay_15s, round(LongI/1000)}]), State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast({new_delay, Delay}, #delay_stat_state{
        intervals = Intervals,
        border = Border
    } = State) ->
    NewNow = os:timestamp(),
    NewPeriod = case Border of
        0 -> 1;
        _ -> nmea_utils:calculate_delay(Border, NewNow) div (?SHORT_INTERVAL * 1000)
    end,
    Intervals2 = lists:foldl(fun(_, Intervals1) ->
                queue:in(queue:new(), Intervals1)  
        end, Intervals, lists:seq(1, NewPeriod)),
    Intervals4 = lists:foldl(fun(_, Intervals3) ->
                queue:drop(Intervals3)
        end, Intervals2, lists:seq(1, max(queue:len(Intervals2) - (?LONG_INTERVAL div ?SHORT_INTERVAL), 0))),
    {{value, LastInterval}, Intervals5} = queue:out_r(Intervals4),
    Intervals6 = queue:in(queue:in(Delay, LastInterval), Intervals5),
    {noreply, State#delay_stat_state{intervals = Intervals6, border = NewNow}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

update(Delay) ->
    gen_server:cast(?MODULE, {new_delay, Delay}).

current() ->
    gen_server:call(?MODULE, current).
