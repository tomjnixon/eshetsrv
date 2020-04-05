-module(eshethttp_cowboy_utils).

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
      {ok, jiffy:decode(Body), Req2}
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
