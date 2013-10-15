-module(nmea_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0, stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    nmea_sup:start_link(),
    nmea_mgr:start_link().

stop(_State) ->
    ok.

start() ->
    start(one, two).

stop() ->
    stop(one).
