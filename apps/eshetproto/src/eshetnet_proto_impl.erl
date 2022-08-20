-module(eshetnet_proto_impl).
-export([parse/1]).
-export([parse_payload/1]).
-export([pack/1]).

msgpack_pack(Value) ->
    msgpack:pack(eshet_common:format_json(Value), [
        {pack_str, from_binary},
        {pack_binary, from_tagged_binary}
    ]).

msgpack_unpack(Msgpack) ->
    msgpack:unpack(Msgpack, [
        {unpack_str, as_binary},
        {unpack_binary, as_tagged_binary}
    ]).

parse_one(<<16#47, Len:16, Payload:Len/binary, Rest/binary>>) ->
    case parse_payload(Payload) of
        error -> error;
        Message -> {Message, Rest}
    end;
parse_one(<<Byte:8, _Rest/binary>>) when Byte =/= 16#47 ->
    error;
parse_one(Rest) ->
    {wait, Rest}.

parse(Msg) ->
    case parse_one(Msg) of
        {wait, Rest} ->
            {[], Rest};
        {FirstMessage, FirstRest} ->
            case parse(FirstRest) of
                {Messages, Rest} -> {[FirstMessage | Messages], Rest};
                error -> error
            end;
        error ->
            error
    end.

parse_payload(<<16#01, Version:8, TimeoutS:16>>) ->
    {hello, Version, TimeoutS};
parse_payload(<<16#02, Version:8, TimeoutS:16, ClientIDPack/binary>>) ->
    case msgpack_unpack(ClientIDPack) of
        {ok, ClientID} -> {hello_id, Version, TimeoutS, ClientID};
        _ -> error
    end;
parse_payload(<<16#03>>) ->
    {hello};
parse_payload(<<16#04, ClientIDPack/binary>>) ->
    case msgpack_unpack(ClientIDPack) of
        {ok, ClientID} -> {hello_id, ClientID};
        _ -> error
    end;
parse_payload(<<16#05, Id:16, Msgpack/binary>>) ->
    case msgpack_unpack(Msgpack) of
        {ok, Msg} -> {reply, Id, {ok, Msg}};
        _ -> error
    end;
parse_payload(<<16#06, Id:16, Msgpack/binary>>) ->
    case msgpack_unpack(Msgpack) of
        {ok, Msg} -> {reply, Id, {error, Msg}};
        _ -> error
    end;
parse_payload(<<16#07, Id:16, Msgpack/binary>>) ->
    case msgpack_unpack(Msgpack) of
        {ok, Msg} -> {reply_state, Id, {ok, {known, Msg}}};
        _ -> error
    end;
parse_payload(<<16#08, Id:16>>) ->
    {reply_state, Id, {ok, unknown}};
parse_payload(<<16#09, Id:16>>) ->
    {ping, Id};
parse_payload(<<16#10, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {action_register, Id, Path};
        _ -> error
    end;
parse_payload(<<16#11, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {action_call, Id, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#20, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {prop_register, Id, Path};
        _ -> error
    end;
parse_payload(<<16#21, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {prop_get, Id, Path};
        _ -> error
    end;
parse_payload(<<16#22, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {prop_set, Id, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#23, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {get, Id, Path};
        _ -> error
    end;
parse_payload(<<16#24, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {set, Id, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#30, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {event_register, Id, Path};
        _ -> error
    end;
parse_payload(<<16#31, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {event_emit, Id, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#32, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {event_listen, Id, Path};
        _ -> error
    end;
parse_payload(<<16#33, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {event_notify, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#40, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {state_register, Id, Path};
        _ -> error
    end;
parse_payload(<<16#41, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {state_changed, Id, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#42, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {state_unknown, Id, Path};
        _ -> error
    end;
parse_payload(<<16#43, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {state_observe, Id, Path};
        _ -> error
    end;
parse_payload(<<16#44, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {state_changed, Path, {known, Msg}};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(<<16#45, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {state_changed, Path, unknown};
        _ -> error
    end;
parse_payload(_) ->
    error.

% client to server
pack_payload({hello, Version, TimeoutS}) ->
    <<16#01, Version:8, TimeoutS:16>>;
pack_payload({hello_id, Version, TimeoutS, ClientID}) ->
    <<16#02, Version:8, TimeoutS:16, (msgpack_pack(ClientID))/binary>>;
% server to client
pack_payload({hello}) ->
    <<16#03>>;
pack_payload({hello_id, ClientID}) ->
    <<16#04, (msgpack_pack(ClientID))/binary>>;
% bidirectional
pack_payload({reply, Id, {ok, Msg}}) ->
    <<16#05, Id:16, (msgpack_pack(Msg))/binary>>;
pack_payload({reply, Id, {error, Msg}}) ->
    <<16#06, Id:16, (msgpack_pack(Msg))/binary>>;
% server to client
pack_payload({reply_state, Id, {ok, {known, Msg}}}) ->
    <<16#07, Id:16, (msgpack_pack(Msg))/binary>>;
pack_payload({reply_state, Id, {ok, unknown}}) ->
    <<16#08, Id:16>>;
% client to server
pack_payload({ping, Id}) ->
    <<16#09, Id:16>>;
% client to server
pack_payload({action_register, Id, Path}) ->
    <<16#10, Id:16, Path/binary, 0>>;
% bidirectional
pack_payload({action_call, Id, Path, Msg}) ->
    <<16#11, Id:16, Path/binary, 0, (msgpack_pack(Msg))/binary>>;
% client to server
pack_payload({prop_register, Id, Path}) ->
    <<16#20, Id:16, Path/binary, 0>>;
% server to client
pack_payload({prop_get, Id, Path}) ->
    <<16#21, Id:16, Path/binary, 0>>;
pack_payload({prop_set, Id, Path, Msg}) ->
    <<16#22, Id:16, Path/binary, 0, (msgpack_pack(Msg))/binary>>;
% client to server
pack_payload({get, Id, Path}) ->
    <<16#23, Id:16, Path/binary, 0>>;
pack_payload({set, Id, Path, Msg}) ->
    <<16#24, Id:16, Path/binary, 0, (msgpack_pack(Msg))/binary>>;
% client to server
pack_payload({event_register, Id, Path}) ->
    <<16#30, Id:16, Path/binary, 0>>;
% client to server
pack_payload({event_emit, Id, Path, Msg}) ->
    <<16#31, Id:16, Path/binary, 0, (msgpack_pack(Msg))/binary>>;
% client to server
pack_payload({event_listen, Id, Path}) ->
    <<16#32, Id:16, Path/binary, 0>>;
% server to client
pack_payload({event_notify, Path, Msg}) ->
    <<16#33, Path/binary, 0, (msgpack_pack(Msg))/binary>>;
% client to server
pack_payload({state_register, Id, Path}) ->
    <<16#40, Id:16, Path/binary, 0>>;
% client to server
pack_payload({state_changed, Id, Path, State}) ->
    <<16#41, Id:16, Path/binary, 0, (msgpack_pack(State))/binary>>;
% client to server
pack_payload({state_unknown, Id, Path}) ->
    <<16#42, Id:16, Path/binary, 0>>;
% client to server
pack_payload({state_observe, Id, Path}) ->
    <<16#43, Id:16, Path/binary, 0>>;
% server to client
pack_payload({state_changed, Path, {known, State}}) ->
    <<16#44, Path/binary, 0, (msgpack_pack(State))/binary>>;
pack_payload({state_changed, Path, unknown}) ->
    <<16#45, Path/binary, 0>>.

pack(Payload) ->
    PackedPayload = pack_payload(Payload),
    case byte_size(PackedPayload) of
        Len when Len < 1 bsl 16 -> {ok, <<16#47, Len:16, PackedPayload/binary>>};
        _ -> {error, too_long}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pack_unpack_test() ->
    Messages = [
        {hello},
        {hello, 1, 30},
        {hello_id, <<"foo">>},
        {hello_id, 1, 30, <<"foo">>},
        {reply, 42, {ok, 5}},
        {reply, 42, {error, 5}},
        {reply_state, 42, {ok, {known, [<<"foo">>, 5]}}},
        {reply_state, 42, {ok, unknown}},
        {ping, 42},
        {action_register, 42, <<"/path">>},
        {action_call, 42, <<"/path">>, [<<"foo">>, 5]},
        {prop_register, 42, <<"/path">>},
        {prop_get, 42, <<"/path">>},
        {prop_set, 42, <<"/path">>, [<<"foo">>, 5]},
        {get, 42, <<"/path">>},
        {set, 42, <<"/path">>, [<<"foo">>, 5]},
        {event_register, 42, <<"/path">>},
        {event_emit, 42, <<"/path">>, [<<"foo">>, 5]},
        {event_listen, 42, <<"/path">>},
        {event_notify, <<"/path">>, [<<"foo">>, 5]},
        {state_register, 42, <<"/path">>},
        {state_changed, 42, <<"/path">>, [<<"foo">>, 5]},
        {state_unknown, 42, <<"/path">>},
        {state_observe, 42, <<"/path">>},
        {state_changed, <<"/path">>, {known, [<<"foo">>, 5]}},
        {state_changed, <<"/path">>, unknown}
    ],

    [
        ?assertEqual(Message, parse_payload(pack_payload(Message)))
     || Message <- Messages
    ].

-endif.
