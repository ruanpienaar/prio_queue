-module(prio_queue_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("prio_queue.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    prio_queue_sup:start_link().

stop(_State) ->
    ok.
