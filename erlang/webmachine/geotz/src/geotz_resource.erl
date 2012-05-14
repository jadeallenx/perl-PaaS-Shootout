%% @author Mark Allen <mrallen1@yahoo.com>
%% @copyright 2012 Mark Allen.

-module(geotz_resource).
-export([init/1, process_post/2, post_is_create/2, to_json/2, allowed_methods/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("egeoip/include/egeoip.hrl").

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context) ->
    L0 = mochijson2:decode(wrq:req_body(ReqData)),
    Out = process_ips([ binary_to_list(Ip) || Ip <- L0 ]),
    io:format("Out> ~p~n", [Out]),
    to_json(ReqData, [{output, Out}] ++ Context),
    {true, ReqData, Context}.

to_json(ReqData, Context) ->
    io:format("Context> ~p~n", [Context]),
    Json = mochijson2:encode({struct, proplists:get_value(output, Context)}),
    io:format("Json> ~p~n", [Json]),
    {Json, ReqData, Context}.

process_ips(IpList) ->
    io:format("IpList> ~p~n", [IpList]),
    F = fun (Ip, Acc) ->
        case egeoip:lookup(Ip) of
            {ok, R} ->
                [{struct, [
                    {<<"country">>, ensure_binary(egeoip:get(R, country_name))},
                    {<<"region">>, ensure_binary(egeoip:get(R, region))},
                    {<<"city">>, ensure_binary(egeoip:get(R, city))},
                    %% this is an integer, so no need to make it a binary
                    {<<"utc_offset">>, egeoip_tz:utc_offset(R)} ]
                }] ++ Acc;
            Error ->
                erlang:error(Error)
        end
    end,

    lists:foldr(F, [], IpList).

ensure_binary(Term) ->
    case Term of
        _ when is_list(Term) ->
            list_to_binary(Term);
        _ when is_binary(Term) ->
            Term;
        _ ->
            erlang:error(badtype)
    end.
    
