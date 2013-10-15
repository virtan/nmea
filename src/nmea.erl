-module(nmea).
-export([
    start/1,
    start_link/1,
    stop/1,
    code_change/3,
    handle_call/3,
    init/1,
    terminate/2,
    handle_info/2,
    handle_cast/2,
    remote_activate/2,
    ping/2,
    activate/1,
    set_rate/1,
    set_amount/2,
    apply_to_group/2,
    extra_payload/0
]).

-behaviour(supervisor).
-behaviour(gen_server).

-record(nmea_state, {pair_remote = undefined, rate = 0, amount = 1, timer = undefined}).

gen_server_name(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ Id).

start({gen_server, Id}) ->
    gen_server:start({global, gen_server_name(Id)}, ?MODULE, {gen_server, Id}, []).

start_link({gen_server, Id}) ->
    gen_server:start_link({global, gen_server_name(Id)}, ?MODULE, {gen_server, Id}, []).

stop(Id) ->
    gen_server:call({global, gen_server_name(Id)}, stop).

supervisor_activate(Procs, Location) ->
    {ok, SupPid} = supervisor:start_link(?MODULE, supervisor),
    [supervisor:start_child(SupPid, {list_to_atom(atom_to_list(?MODULE) ++ "_" ++ Location ++ "_" ++ integer_to_list(Id) ++ "_supid"),
                {?MODULE, start_link, [{gen_server, Location ++ "_" ++ integer_to_list(Id)}]}, permanent, 5000, worker, [linux_stat]})
        || Id <- lists:seq(1, Procs)].

wait_children(Total, Total) ->
    done;
wait_children(Already, Total) ->
    receive
        i_am_ready -> wait_children(Already + 1, Total)
    end.

remote_activate(Activator, GroupLeader) ->
    group_leader(GroupLeader, self()),
    case global:whereis_name(nmea_activator_remote) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            timer:sleep(1000),
            global:sync()
    end,
    global:register_name(nmea_activator_remote, self()),
    Procs = erlang:system_info(logical_processors),
    supervisor_activate(Procs, "remote"),
    wait_children(0, Procs),
    Activator ! {ok, Procs},
    receive
        stop -> done
    end.

local_activate({ok, RemoteProcesses}) ->
    global:register_name(nmea_activator_local, self()),
    global:sync(),
    supervisor_activate(RemoteProcesses, "local"),
    wait_children(0, RemoteProcesses),
    global:sync(),
    set_amount("local", RemoteProcesses),
    {ok, RemoteProcesses}.

remote_resolve(Remote) ->
    case global:whereis_name(Remote) of
        undefined ->
            io:format("Can't resolve ~p [~p], waiting~n", [Remote, global:registered_names()]),
            timer:sleep(1000),
            remote_resolve(Remote);
        Pid -> Pid
    end.

init(supervisor) ->
    {ok, {{one_for_one, 5, 10}, []}};

init({gen_server, "local_" ++ NId}) ->
    global:send(nmea_activator_local, i_am_ready),
    {ok, #nmea_state{pair_remote = remote_resolve(gen_server_name("remote_" ++ NId))}};
init({gen_server, _Id}) ->
    global:send(nmea_activator_remote, i_am_ready),
    {ok, #nmea_state{}}.

handle_call({set_amount, Processes}, _From, State) ->
    {reply, done, State#nmea_state{amount = Processes}};
handle_call({set_rate, Rate}, _From, #nmea_state{amount = Amount, timer = Timer} = State) ->
    case Timer of
        undefined -> do_nothing;
        _ -> timer:cancel(Timer)
    end,
    MyRate = Rate / Amount,
    {Delay, EveryCycle} = if
        MyRate == 0 -> {undefined, undefined};
        MyRate < 40 -> {1000/MyRate, 1};
        true -> {100, MyRate / 10}
    end,
    TRef = case Delay of
        undefined -> undefined;
        _ ->
            timer:sleep(random:uniform(round(Delay))),
            {ok, TRef1} = timer:send_interval(round(Delay), self(), {timer_event, round(EveryCycle)}),
            TRef1
    end,
    {reply, done, State#nmea_state{rate = Rate, timer = TRef}};
handle_call(stop, _From, #nmea_state{timer = Timer} = State) ->
    case Timer of
        undefined -> do_nothing;
        _ -> timer:cancel(Timer)
    end,
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast({ping, Remote, Payload}, State) ->
    gen_server:cast(Remote, {pong, Payload}),
    {noreply, State};
handle_cast({pong, {Payload, _}}, State) ->
    Delay = nmea_utils:calculate_delay(Payload, os:timestamp()),
    delay_stat:update(Delay),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

extra_payload() ->
    {ok, PayloadSize} = application:get_env(nmea, payload),
    binary:copy(<<11>>, PayloadSize).

handle_info({timer_event, EveryCycle}, #nmea_state{pair_remote = Remote} = State) ->
    [ping(Remote, {os:timestamp(), extra_payload()}) || _ <- lists:seq(1, EveryCycle)],
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #nmea_state{pair_remote = Remote}) ->
    case Remote of
        undefined -> do_nothing;
        Pid -> gen_server:call(Pid, stop)
    end,
    ok.

ping(Remote, Payload) ->
    gen_server:cast(Remote, {ping, self(), Payload}).

apply_to_group(Group, F) ->
    nmea_utils:pmap(F, [RegName || RegName <- global:registered_names(), lists:prefix(atom_to_list(?MODULE) ++ "_" ++ Group, atom_to_list(RegName))]).

set_amount(Group, Processes) ->
    apply_to_group(Group, fun(PName) -> gen_server:call({global, PName}, {set_amount, Processes}) end).

set_rate(Rate) ->
    apply_to_group("local", fun(PName) -> gen_server:call({global, PName}, {set_rate, Rate}, 30000) end).

activate(Node) ->
    case net_adm:ping(Node) of
        pang ->
            io:format("Node ~p is unreachable~n", [Node]),
            error;
        pong ->
            case c:nl(?MODULE) of
                error ->
                    io:format("Can't load module ~p on ~p~n", [?MODULE, Node]),
                    error;
                _ ->
                    rpc:cast(Node, ?MODULE, remote_activate, [self(), group_leader()]),
                    receive
                        {ok, _} = GoodRes -> local_activate(GoodRes)
                    after 15000 ->
                            io:format("Remote replier is not started~n", []),
                            error
                    end
            end
    end.
