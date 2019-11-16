-module(eshetnet_proto_impl).
-export([parse/1]).
-export([parse_payload/1]).
-export([pack/1]).

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
        {wait, Rest} -> {[], Rest};
        {FirstMessage, FirstRest} ->
            case parse(FirstRest) of
                {Messages, Rest} -> {[FirstMessage | Messages], Rest};
                error -> error
            end;
        error -> error
    end.

parse_payload(<<16#01, Id:16, Msgpack/binary>>) ->
    case msgpack:unpack(Msgpack) of
        {ok, Msg} -> {reply, Id, {ok, Msg}};
        _ -> error
    end;
parse_payload(<<16#02, Id:16, Msgpack/binary>>) ->
    case msgpack:unpack(Msgpack) of
        {ok, Msg} -> {reply, Id, {error, Msg}};
        _ -> error
    end;

parse_payload(<<16#03, Id:16>>) ->
    {ping, Id};

parse_payload(<<16#10, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {action_register, Id, Path};
        _ -> error
    end;

parse_payload(<<16#11, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack:unpack(Msgpack) of
                {ok, Msg} -> {action_call, Id, Path, Msg};
                _ -> error
            end;
        _ -> error
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
            case msgpack:unpack(Msgpack) of
                {ok, Msg} -> {prop_set, Id, Path, Msg};
                _ -> error
            end;
        _ -> error
    end;

parse_payload(<<16#30, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, <<>>] -> {event_register, Id, Path};
        _ -> error
    end;

parse_payload(<<16#31, Id:16, Rest/binary>>) ->
    case binary:split(Rest, <<0>>) of
        [Path, Msgpack] ->
            case msgpack:unpack(Msgpack) of
                {ok, Msg} -> {event_dispatch, Id, Path, Msg};
                _ -> error
            end;
        _ -> error
    end;

parse_payload(_) -> error.

pack_payload({reply, Id, {ok, Msg}}) ->
    <<16#01, Id:16, (msgpack:pack(Msg))/binary>>;
pack_payload({reply, Id, {error, Msg}}) ->
    <<16#02, Id:16, (msgpack:pack(Msg))/binary>>;
pack_payload({action_register, Id, Path}) ->
    <<16#10, Id:16, Path/binary, 0>>;
pack_payload({action_call, Id, Path, Msg}) ->
    <<16#11, Id:16, Path/binary, 0, (msgpack:pack(Msg))/binary>>;
pack_payload({prop_register, Id, Path}) ->
    <<16#20, Id:16, Path/binary, 0>>;
pack_payload({prop_get, Id, Path}) ->
    <<16#21, Id:16, Path/binary, 0>>;
pack_payload({prop_set, Id, Path, Msg}) ->
    <<16#22, Id:16, Path/binary, 0, (msgpack:pack(Msg))/binary>>;
pack_payload({event_register, Id, Path}) ->
    <<16#30, Id:16, Path/binary, 0>>;
pack_payload({event_dispatch, Id, Path, Msg}) ->
    <<16#31, Id:16, Path/binary, 0, (msgpack:pack(Msg))/binary>>.

pack(Payload) ->
    PackedPayload = pack_payload(Payload),
    case byte_size(PackedPayload) of
        Len when Len < 1 bsl 16 -> {ok, <<16#47, Len:16, PackedPayload/binary>>};
        _ -> {error, too_long}
    end.
