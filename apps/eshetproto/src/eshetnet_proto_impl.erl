-module(eshetnet_proto_impl).
-export([parse/1]).
-export([parse_payload/1]).
-export([pack/1]).

-include("messages.hrl").

-dialyzer({no_underspecs, [msgpack_pack/1, pack/1]}).

%% .. _binary_protocol:
%%
%% Binary Protocol
%% ===============
%%
%% This document defines the ESHET binary protocol, which consists of a
%% bidirectional stream of bytes, encoded as described below, representing a
%% bi-directional stream of messages defined in the :ref:`generic_protocol`.
%%
%% This protocol is typically carried over TCP, and assumes that the
%% connection-oriented semantics of TCP are present, meaning that the client
%% and server both observe a "connection" event, after which all messages are
%% delivered in both directions, until the connection closes. Specifically, the
%% client assumes that its hello message will be received, and the server
%% assumes that its reply will be received by the client.
%%
%% Other transports (e.g. SSL) may meet these requirements, but serial ports do
%% not, for example.
%%
%% Protocol errors (see :ref:`errors`) are handled by closing the connection.
%%
%% The conventions used in this document are descried in :ref:`conventions`.
%%
%% Utility Functions
%% -----------------
%%
%% ``msgpack_pack`` turns an internal representation of msgpack values into
%% binary strings:

-spec msgpack_pack(any()) -> binary() | {error, _}.
%%% SKIP
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

%% ``pack_time`` converts the native time format into a number of milliseconds,
%% currently only used in ``reply_state_t()`` messages.
-spec pack_time(integer()) -> integer().
%%% SKIP
pack_time(Time) ->
    erlang:convert_time_unit(Time, native, millisecond).

unpack_time(T) ->
    erlang:convert_time_unit(T, millisecond, native).

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

-spec parse_payload(binary()) -> message() | error.
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
parse_payload(<<16#0a, Id:16, T:32, Msgpack/binary>>) ->
    case msgpack_unpack(Msgpack) of
        {ok, Msg} -> {reply_state, Id, {ok, {known, Msg}, unpack_time(T)}};
        _ -> error
    end;
parse_payload(<<16#0b, Id:16, T:32>>) ->
    {reply_state, Id, {ok, unknown, unpack_time(T)}};
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
parse_payload(<<16#46, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {state_observe_t, Id, Path};
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
parse_payload(<<16#47, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack_unpack(Msgpack) of
                {ok, Msg} -> {state_set, Id, Path, Msg};
                _ -> error
            end;
        _ ->
            error
    end;
parse_payload(_) ->
    error.

%% Message Format
%% --------------
%%
%% Single messages are packed using the ``pack_payload`` function. In general
%% this returns:
%%
%% * A single byte, whose value indicates the type and structure of the message
%%   (e.g. ``{reply, Id, {ok, Msg}}`` has a different first byte from ``{reply,
%%   Id, {error, Msg}}``).
%%
%% * All non-msgpack elements of the message, packed depending on the type:
%%
%%     * Integers are packed in network byte order (big endian), with an
%%       appropriate number of bits (defined below).
%%     * Strings (for paths) are zero-terminated.
%%
%% * If the message contains a msgpack element, it is packed with
%%   ``msgpack_pack``, with no termination.

-spec pack_payload(message()) -> binary().

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
pack_payload({reply_state, Id, {ok, {known, Msg}, Time}}) ->
    T = pack_time(Time),
    <<16#0a, Id:16, T:32, (msgpack_pack(Msg))/binary>>;
pack_payload({reply_state, Id, {ok, unknown, Time}}) ->
    T = pack_time(Time),
    <<16#0b, Id:16, T:32>>;
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
pack_payload({state_observe_t, Id, Path}) ->
    <<16#46, Id:16, Path/binary, 0>>;
% server to client
pack_payload({state_changed, Path, {known, State}}) ->
    <<16#44, Path/binary, 0, (msgpack_pack(State))/binary>>;
pack_payload({state_changed, Path, unknown}) ->
    <<16#45, Path/binary, 0>>;
pack_payload({state_set, Id, Path, Msg}) ->
    <<16#47, Id:16, Path/binary, 0, (msgpack_pack(Msg))/binary>>.

%% Framing
%% -------
%%
%% Before messages are transmitted, they are wrapped in a frame structure,
%% applied by ``pack``. This takes a ``message()``, and returns a binary containing:
%%
%% * A byte with value 0x47, used by parsers to check that messages are
%%   properly framed.
%% * The 2 byte length of the serialised message (not including this framing).
%% * The serialised message, according to ``pack_payload``.

-spec pack(message()) -> {ok, binary()} | {error, too_long}.
pack(Payload) ->
    PackedPayload = pack_payload(Payload),
    case byte_size(PackedPayload) of
        Len when Len < 16#10000 -> {ok, <<16#47, Len:16, PackedPayload/binary>>};
        _ -> {error, too_long}
    end.

%% A correctly-formatted stream consists of the concatenated return values of
%% ``pack``.
%%
%%% SKIP

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pack_unpack_test() ->
    Time = erlang:convert_time_unit(1000, millisecond, native),

    Messages = [
        {hello},
        {hello, 1, 30},
        {hello_id, <<"foo">>},
        {hello_id, 1, 30, <<"foo">>},
        {reply, 42, {ok, 5}},
        {reply, 42, {error, 5}},
        {reply_state, 42, {ok, {known, [<<"foo">>, 5]}}},
        {reply_state, 42, {ok, unknown}},
        {reply_state, 42, {ok, {known, [<<"foo">>, 5]}, Time}},
        {reply_state, 42, {ok, unknown, Time}},
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
        {state_observe_t, 42, <<"/path">>},
        {state_changed, <<"/path">>, {known, [<<"foo">>, 5]}},
        {state_changed, <<"/path">>, unknown},
        {state_set, 42, <<"/path">>, [<<"foo">>, 5]}
    ],

    [
        ?assertEqual(Message, parse_payload(pack_payload(Message)))
     || Message <- Messages
    ].

-endif.
