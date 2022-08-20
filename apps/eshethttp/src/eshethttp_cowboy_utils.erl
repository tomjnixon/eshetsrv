-module(eshethttp_cowboy_utils).

-export([eshet_reply/3]).
-export([eshet_reply_json/3]).
-export([json_req_resp/3]).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

read_body(Req) ->
    read_body(Req, <<>>).

read_json(Req) ->
    {ok, Body, Req2} = read_body(Req),
    try
        {ok, jiffy:decode(Body, [return_maps]), Req2}
    catch
        error:{Character, Error} ->
            {error, {Character, Error}, Req2}
    end.

reply_json(StatusCode, Json, Req) ->
    Headers = #{<<"Content-Type">> => <<"application/json">>},
    Body = jiffy:encode(Json),
    {ok, cowboy_req:reply(StatusCode, Headers, Body, Req)}.

json_req_resp(F, Req0, State0) ->
    case read_json(Req0) of
        {ok, JsonReq, Req1} ->
            {Status, JsonRep, Req2, State1} = F(JsonReq, Req1, State0),
            {ok, Req3} = reply_json(Status, JsonRep, Req2),
            {ok, Req3, State1};
        {error, {Char, Error}, Req1} ->
            Msg = io_lib:format(<<"error at character ~p: ~p">>, [Char, Error]),
            {ok, cowboy_req:reply(400, #{}, Msg, Req1), State0}
    end.

eshet_reply({ok, Result}, Req, State) ->
    ResultFmt = eshet_common:format_json(Result),
    {200, ResultFmt, Req, State};
eshet_reply(ok, Req, State) ->
    eshet_reply({ok, null}, Req, State);
eshet_reply({error, Result}, Req, State) ->
    ResultFmt = eshet_common:format_json(Result),
    {<<"520 ESHET Error">>, ResultFmt, Req, State}.

eshet_reply_json(Response, Req, State) ->
    {Status, JsonRep, Req1, State1} = eshet_reply(Response, Req, State),
    {ok, Req2} = reply_json(Status, JsonRep, Req1),
    {ok, Req2, State1}.
