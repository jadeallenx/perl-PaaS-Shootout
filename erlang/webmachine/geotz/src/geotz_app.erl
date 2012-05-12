%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the geotz application.

-module(geotz_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for geotz.
start(_Type, _StartArgs) ->
    geotz_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for geotz.
stop(_State) ->
    ok.
