%% @author Mark Allen <mrallen1@yahoo.com>
%% @copyright 2012 Mark Allen.

-module(geotz_resource).
-export([init/1, process_post/2, post_is_create/2, allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("egeoip/include/egeoip.hrl").

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context) ->
    L0 = mochijson2:decode(wrq:req_body(ReqData)),
    Out = process_ips([ binary_to_list(Ip) || Ip <- L0 ]),
    {true, set_content_type(wrq:set_resp_body(to_json(Out), ReqData), "application/json"), Context}.

to_json(Out) ->
    Json = mochijson2:encode(Out),
    list_to_binary(lists:flatten(Json)).

set_content_type(ReqData, ContentType) ->
    wrq:set_resp_headers([{"Content-Type", ContentType}], ReqData).

process_ips(IpList) ->
    F = fun (Ip, Acc) ->
        case egeoip:lookup(Ip) of
            {ok, R} ->
                [ { ensure_binary(Ip), 
                    [ {<<"country">>, geoip_get(R, country_name)},
                      {<<"region">>, geoip_get(R, region)},
                      {<<"city">>, geoip_get(R, city)},
                      %% this is an integer, so no need to make it a binary
                      {<<"utc_offset">>, egeoip_tz:utc_offset(R)} 
                    ] 
                  } 
                ] ++ Acc;
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
    
geoip_get(Record, Field) ->
    ensure_binary(egeoip:get(Record, Field)).
