-module(eshetsrv_state_api).

%% API.
-export([link/2]).
-export([register/4]).
-export([lookup/3]).
-export([state_changed/4]).

link(Srv, Client) ->
    gen_server:call(Srv, {link, Client}).

register(Srv, Type, Path, Pid) ->
    gen_server:call(Srv, {register, Type, Path, Pid}).

lookup(Srv, Type, Path) ->
    gen_server:call(Srv, {lookup, Type, Path}).

state_changed(Srv, Path, Pid, NewState) ->
    gen_server:call(Srv, {state_changed, Path, Pid, NewState}).
