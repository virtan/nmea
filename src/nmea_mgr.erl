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
    handle_cast/2,
    get_system_info/0
]).

-behaviour(gen_server).

-record(nmea_mgr, {rate = 0, increase_timer = undefined, report_timer = undefined, start_time, log_file}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, none, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

stop() ->
    gen_server:cast(?MODULE, stop).

get_system_info() ->
    PayloadSize = case application:get_env(nmea, payload) of
        undefined -> 0;
        {ok, Val} -> Val
    end,
    [case application:get_env(kernel, dist_nodelay) of
        undefined ->
            {nodelay, true};
        {ok, true} ->
            {nodelay, true};
        {ok, false} ->
            {nodelay, false};
        _ ->
            {nodelay, true}
    end,
    {payload, integer_to_list(round(nmea_utils:significant_round(PayloadSize, 1))) ++ "+"},
    {async, erlang:system_info(thread_pool_size)},
    {kpoll, erlang:system_info(kernel_poll)}].

system_info_to_filename(SystemInfo) ->
    case proplists:get_value(nodelay, SystemInfo) of
        true -> "nonagle";
        _ -> "nagle"
    end ++ "_" ++
    "async:" ++ integer_to_list(proplists:get_value(async, SystemInfo)) ++ "_" ++
    case proplists:get_value(kpoll, SystemInfo) of
        true -> "kpoll";
        _ -> "nokpoll"
    end ++ "_" ++
    "payload:" ++ proplists:get_value(payload, SystemInfo).

init(none) ->
    process_flag(priority, high),
    {ok, Remote} = application:get_env(nmea, remote),
    case nmea:activate(Remote) of
        {ok, _} ->
            c:nl(nmea_utils),
            c:nl(?MODULE),
            SystemInfo = get_system_info(),
            RemoteSystemInfo = rpc:call(Remote, ?MODULE, get_system_info, []),
            case {{proplists:get_value(nodelay, SystemInfo), proplists:get_value(nodelay, RemoteSystemInfo)},
                  {proplists:get_value(async, SystemInfo), proplists:get_value(async, RemoteSystemInfo)},
                  {proplists:get_value(kpoll, SystemInfo), proplists:get_value(kpoll, RemoteSystemInfo)}} of
                {{A, A}, {B, B}, {C, C}} ->
                    {ok, IoD} = file:open("log/attempt_" ++ system_info_to_filename(SystemInfo) ++ ".log",
                        [write, binary, raw, delayed_write]),
                    {ok, TRef1} = timer:send_interval(5000, self(), increase),
                    {ok, TRef2} = timer:send_interval(500, self(), report),
                    {ok, #nmea_mgr{increase_timer = TRef1, report_timer = TRef2, start_time = os:timestamp(), log_file = IoD}};
                _ ->
                    io:format("Local and remote configurations differ~n", []),
                    {stop, normal} 
            end;
        _ ->
            {stop, normal}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(increase, #nmea_mgr{rate = Rate} = State) ->
    NewRate = case Rate of
        0 -> 10240;
        _ -> Rate + 10240
    end,
    nmea:set_rate(NewRate),
    {noreply, State#nmea_mgr{rate = NewRate}};
handle_info(report, #nmea_mgr{
        rate = Rate,
        start_time = StartTime,
        log_file = IoD
    } = State) ->
    DelayStat = delay_stat:current(),
    LinuxStat = linux_stat:current(),
    SecondsSinceStart = round(nmea_utils:calculate_delay(StartTime, os:timestamp()) div 1000000),
    ToLog = io_lib:format("~B\t~B\t~B\t~B\t~B\t~B\t~B~n", [
            SecondsSinceStart, Rate,
            round(proplists:get_value(delay_500ms, DelayStat)),
            round(proplists:get_value(tx_packet_rate, LinuxStat)),
            round(proplists:get_value(rx_packet_rate, LinuxStat)),
            round(proplists:get_value(tx_byte_rate, LinuxStat)),
            round(proplists:get_value(rx_byte_rate, LinuxStat))
        ]),
    file:write(IoD, list_to_binary(ToLog)),
    Delay = proplists:get_value(delay_15s, DelayStat),
    if
        Delay > 3000 -> stop();
        true -> do_nothing
    end,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #nmea_mgr{increase_timer = TRef1, report_timer = TRef2, log_file = IoD}) ->
    timer:cancel(TRef1),
    timer:cancel(TRef2),
    file:close(IoD),
    io:format("done~n", []),
    ok.
