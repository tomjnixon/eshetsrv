-module(eshethttp_action_call).

-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    eshethttp_cowboy_utils:json_req_resp(fun handle_json/3, Req, State).

handle_json(JsonReq, Req1, State1 = [Server]) ->
    Path = eshet:path_unsplit(cowboy_req:path_info(Req1)),
    case eshet:action_call(Server, Path, JsonReq) of
      {ok, Result} ->
          ResultFmt = eshet_common:format_json(Result),
          {200, ResultFmt, Req1, State1};
      {error, Result} ->
          ResultFmt = eshet_common:format_json(Result),
          {<<"520 ESHET Error">>, ResultFmt, Req1, State1}
    end.
