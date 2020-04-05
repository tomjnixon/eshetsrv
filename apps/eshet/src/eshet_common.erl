-module(eshet_common).

-export([format_json/1]).

% replace:
% - unrecognised atoms with binaries (which are packed as strings)
% - tuples with lists
% this is intended to make erlang data compatible with JSON and msgpack
% libraries
format_json(true) ->
    true;
format_json(false) ->
    false;
format_json(null) ->
    null;
format_json(X) when is_atom(X) ->
    erlang:atom_to_binary(X, utf8);
format_json(X) when is_list(X) ->
    [format_json(Item) || Item <- X];
format_json(X) when is_tuple(X) ->
    format_json(erlang:tuple_to_list(X));
format_json(X) when is_map(X) ->
    maps:from_list([{format_json(Key), format_json(Value)}
                    || {Key, Value} <- maps:to_list(X)]);
format_json(X) ->
    X.
