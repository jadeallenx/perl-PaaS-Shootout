%% @author Mark Allen <mrallen1@yahoo.com>
%% @copyright 2012 Mark Allen.

-module(geotz_resource).
-export([init/1, from_json/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

from_json(ReqData, Context) ->
    {ok, ReqData, Context}.

to_json(ReqData, Context) ->
    {"<html><body>Hello, new world</body></html>", ReqData, Context}.
