-module(eshetsrv_meta).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          server
}).

%% API.

-spec start_link(term()) -> {ok, pid()}.
start_link(Srv) ->
    gen_server:start_link(?MODULE, [Srv], []).

%% gen_server.

init([Srv]) ->
    eshet:action_register(Srv, <<"/meta/ls">>),
    {ok, #state{server=Srv}}.

handle_call({action_call, Path, Args}, _From, State=#state{server=_Srv}) ->
    case {Path, Args} of
        {<<"/meta/ls">>, [Dir]} when is_binary(Dir) ->
            {reply, {ok, list_dir(Dir)}, State};
        _ -> {reply, {error, bad_args}, State}
    end;

handle_call(_Request, _From, State=#state{server=_Srv}) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% implementation

node_type(action_owner) -> action;
node_type(prop_owner) -> prop;
node_type(event_owner) -> event;
node_type(event_listener) -> event.

list_dir(Path) ->
    Parts = eshet:path_split(Path),
    SubParts = ets:select(eshetsrv_state,
                          [{{Parts ++ '$1', '$2', '_'}, [], [['$1', '$2']]}]),

    maps:from_list([{Entry,
                     case Rest of
                         [_|_] -> dir;
                         [] -> node_type(Type)
                     end} || [[Entry | Rest], Type] <- SubParts]).
