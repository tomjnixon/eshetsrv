-module(eshethttp_ws).

% websocket handler for the native ESHET protocol, but in json format. This
% ultimately uses eshetproto_generic to implement the protocol, but the cowboy
% websocket protocol makes this hard because it swallows '$gen_call' messages,
% which are needed to implement the erlang ESHET protocol. The interaction with
% the rest of the system is therefore performed in another gen_server,
% eshethttp_ws_server.

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts, #{idle_timeout => 600000}}.

websocket_init([Srv, Registry]) ->
    {ok, WSServer} = eshethttp_ws_server:start_link(self(), Srv, Registry),
    {[], WSServer}.

% convert json messages to the internal tuple format for eshetproto_generic
from_json([<<"reply">>, Id, [<<"ok">>, Value]]) ->
    {reply, Id, {ok, Value}};
from_json([<<"reply">>, Id, [<<"error">>, Value]]) ->
    {reply, Id, {error, Value}};
from_json([<<"reply_state">>, Id, [<<"ok">>, [<<"known">>, Value]]]) ->
    {reply_state, Id, {ok, {known, Value}}};
from_json([<<"reply_state">>, Id, [<<"ok">>, <<"unknown">>]]) ->
    {reply_state, Id, {ok, unknown}};
from_json([<<"state_changed">>, Path, [<<"known">>, Value]]) ->
    {state_changed, Path, {known, Value}};
from_json([<<"state_changed">>, Path, <<"unknown">>]) ->
    {state_changed, Path, unknown};
from_json([Type | Rest]) ->
    erlang:list_to_tuple([erlang:binary_to_atom(Type) | Rest]).

% convert internal message format to json
to_json(Message) ->
    eshet_common:format_json(erlang:tuple_to_list(Message)).

websocket_handle({_Type, Data}, WSServer) ->
    try jiffy:decode(Data, [return_maps]) of
      Message ->
          gen_server:cast(WSServer, {messages, [from_json(Message)]}),
          {[], WSServer}
    catch
      {error, {Char, ErrorAtom}} ->
          Error = erlang:iolist_to_binary(io_lib:format("json error at char ~p: ~p~n",
                                                        [Char, ErrorAtom])),
          error_resp(Error)
    end;
websocket_handle(_Data, WSServer) ->
    {[], WSServer}.

websocket_info({send, Messages}, WSServer) ->
    {[{text, jiffy:encode(to_json(Message))} || Message <- Messages], WSServer};
% cowboy websocket server traps exits
websocket_info({'EXIT', WSServer, timeout}, WSServer) ->
    {[{close, 4000, <<"timeout">>}], closed};
websocket_info({'EXIT', WSServer, _Reason}, WSServer) ->
    {[{close, 1011, <<"internal error">>}], closed}.

error_resp(Message) ->
    {[{close, 4000, Message}], closed}.
