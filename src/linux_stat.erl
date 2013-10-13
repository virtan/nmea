-module(linux_stat).
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
    command/0
]).

-behaviour(gen_server).

-record(linux_stat_state, {
          tx_packet_rate = 0,
          rx_packet_rate = 0,
          tx_byte_rate = 0,
          rx_byte_rate = 0,
          tx_packet_last_value = undefined,
          rx_packet_last_value = undefined,
          tx_byte_last_value = undefined,
          rx_byte_last_value = undefined,
          tx_packet_rate_queue = queue:new(),
          rx_packet_rate_queue = queue:new(),
          tx_byte_rate_queue = queue:new(),
          rx_byte_rate_queue = queue:new(),
          queues_size = 0,
          timer}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, none, []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(none) ->
    process_flag(priority, high),
    {ok, TRef} = timer:send_interval(500, self(), timeout),
    {ok, #linux_stat_state{timer = TRef}}.

handle_call(stat, _From, #linux_stat_state{tx_packet_rate = TX_packet_rate,
                                           rx_packet_rate = RX_packet_rate,
                                           tx_byte_rate = TX_byte_rate,
                                           rx_byte_rate = RX_byte_rate
                                          } = State) ->
    {reply, [{tx_packet_rate, TX_packet_rate}, {rx_packet_rate, RX_packet_rate},
             {tx_byte_rate, TX_byte_rate}, {rx_byte_rate, RX_byte_rate}], State};
handle_call(stop, _From, #linux_stat_state{timer = Timer} = State) ->
    timer:cancel(Timer),
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast(_, State) ->
    {noreply, State}.

read_int_from_file(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    list_to_integer(lists:takewhile(fun(C) when C > 32 -> true ; (_) -> false end,
                                    binary_to_list(Binary))).
handle_info(timeout, #linux_stat_state{
                        tx_packet_rate = TPR,
                        rx_packet_rate = RPR,
                        tx_byte_rate = TBR,
                        rx_byte_rate = RBR,
                        tx_packet_last_value = TPLV,
                        rx_packet_last_value = RPLV,
                        tx_byte_last_value = TBLV,
                        rx_byte_last_value = RBLV,
                        tx_packet_rate_queue = TPRQ,
                        rx_packet_rate_queue = RPRQ,
                        tx_byte_rate_queue = TBRQ,
                        rx_byte_rate_queue = RBRQ,
                        queues_size = QS
                       } = State) ->
    [RP, TP, RB, TB] = [read_int_from_file(FN) || FN <- [
                           "/sys/class/net/eth0/statistics/rx_packets",
                           "/sys/class/net/eth0/statistics/tx_packets",
                           "/sys/class/net/eth0/statistics/rx_bytes",
                           "/sys/class/net/eth0/statistics/tx_bytes"]],
    {TPR1, RPR1, TBR1, RBR1} = case TPLV of
        undefined -> {TPR, RPR, TBR, RBR};
        _ ->
            {(TP - TPLV) / 0.5, (RP - RPLV) / 0.5, (TB - TBLV) / 0.5, (RB - RBLV) / 0.5}
    end,
    {Queuer, QS1} = if
        QS >= 30 -> {fun(Queue, NewValue) ->
                            queue:in(NewValue, element(2, queue:out(Queue))) end, QS};
        true -> {fun(Queue, NewValue) ->
                            queue:in(NewValue, Queue) end, QS + 1}
    end,
    [TPRQ1, RPRQ1, TBRQ1, RBRQ1] = [Queuer(Queue1, NewValue1) || {Queue1, NewValue1} <-
                          [{TPRQ, TPR1}, {RPRQ, RPR1}, {TBRQ, TBR1}, {RBRQ, RBR1}]],
    {noreply, State#linux_stat_state{
                        tx_packet_rate = TPR1,
                        rx_packet_rate = RPR1,
                        tx_byte_rate = TBR1,
                        rx_byte_rate = RBR1,
                        tx_packet_last_value = TP,
                        rx_packet_last_value = RP,
                        tx_byte_last_value = TB,
                        rx_byte_last_value = RB,
                        tx_packet_rate_queue = TPRQ1,
                        rx_packet_rate_queue = RPRQ1,
                        tx_byte_rate_queue = TBRQ1,
                        rx_byte_rate_queue = RBRQ1,
                        queues_size = QS1
                       }};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

command() ->
    gen_server:call(?MODULE, command).
