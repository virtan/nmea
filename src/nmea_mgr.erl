-module(nmea_mgr).
-export([
    start/0,
    start_link/0,
    stop/0,
    code_change/3,
    handle_call/3,
    init/1,
    terminate/2,
    handle_info/2,
    handle_cast/2
]).

-behaviour(gen_server).

-record(nmea_mgr, {rate = 0, increase_timer = undefined, report_timer = undefined}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, none, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(none) ->
    {ok, Remote} = application:get_env(nmea, remote),
    nmea:activate(Remote),
    {ok, TRef1} = timer:send_interval(5000, self(), increase),
    {ok, TRef2} = timer:send_interval(500, self(), report),
    {ok, #nmea_mgr{increase_timer = TRef1, report_timer = TRef2}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(increase, #nmea_mgr{rate = Rate} = State) ->
    NewRate = case Rate of
        0 -> 10240;
        _ -> Rate + 10240
    end,
    nmea:set_rate(NewRate),
    {noreply, State#nmea_mgr{rate = NewRate}};
handle_info(report, #nmea_mgr{rate = Rate} = State) ->
    Stat = [{rate, Rate}] ++ delay_stat:current() ++ linux_stat:current(),
    lager:info("~p", [Stat]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.
