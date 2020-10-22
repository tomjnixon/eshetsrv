-module(eshethttp_get).

-behavior(cowboy_handler).

-export([init/2]).

init(Req, State = [Server]) ->
    Path = eshet:path_unsplit(cowboy_req:path_info(Req)),
    Resp = eshet:get(Server, Path),
    eshethttp_cowboy_utils:eshet_reply_json(Resp, Req, State).
