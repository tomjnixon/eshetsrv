-module(eshethttp_set).

-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    eshethttp_cowboy_utils:json_req_resp(fun handle_json/3, Req, State).

handle_json(JsonReq, Req1, State1 = [Server]) ->
    Path = eshet:path_unsplit(cowboy_req:path_info(Req1)),
    Resp = eshet:set(Server, Path, JsonReq),
    eshethttp_cowboy_utils:eshet_reply(Resp, Req1, State1).
