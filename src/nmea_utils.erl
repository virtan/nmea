-module(nmea_utils).
-export([
        calculate_delay/2,
        pretty/1,
        pmap/2,
        floor/1,
        ceiling/1,
        significant_round/2
    ]).

calculate_delay({Mega, Sec, Micro} = _Past, {Mega1, Sec1, Micro1} = _Future) ->
    ((Mega1 - Mega) * 1000000000000) + ((Sec1 - Sec) * 1000000) + Micro1 - Micro.

pretty(PList) ->
    [{Name, pretty(value, Value)} || {Name, Value} <- PList].

pretty(value, Value) when is_float(Value) ->
    round(Value * 100) / 100;
pretty(value, Value) -> Value.

pmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

significant_round(0, _) -> 0;
significant_round(Number, Digits) ->
    D = ceiling(math:log10(abs(Number))),
    P = Digits - round(D),
    M = math:pow(10, P),
    S = round(Number * M),
    S/M.
