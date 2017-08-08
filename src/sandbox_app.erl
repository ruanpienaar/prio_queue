-module(sandbox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("sandbox.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sandbox_sup:start_link().

stop(_State) ->
    ok.
