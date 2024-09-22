% type definitions of messages used in the generic protocol
% lines starting with %% are turned into an RST document that describes the
% protocol

%% .. _generic_protocol:
%%
%% Generic Protocol
%% ================
%%
%% This document describes the generic ESHET protocol, which consists of a
%% series of messages transmitted between the client and the server. A binary
%% format of these message is described in :ref:`binary_protocol`, and a
%% websocket format in :ref:`websocket_protocol`.
%%
%% The conventions used in this document are descried in :ref:`conventions`.
%%
%% Common Types
%% ------------
%%
%% The following types are used in many messages.
%%
%% Basic Integer Types
%% ~~~~~~~~~~~~~~~~~~~

-type u8() :: 0..16#ff.
-type u16() :: 0..16#ffff.
-type u32() :: 0..16#ffffffff.

%% Msgpack
%% ~~~~~~~
%% Any term which can be packed using msgpack.
%%
-type msgpack() :: any().

%% Paths
%% ~~~~~

-type path() :: binary().

%% Objects in ESHET are identified by their path, which is a string matching the
%% regex ``^(/\\w+)+$``, i.e. a series of segments preceded by forward slashes and
%% containing at least one alphanumeric character or underscore, for example
%% ``/foo/bar_baz``.
%%
%% Each registered object has a type, and may only be interacted with using the
%% messages defined for that type. Messages defined for other types will be met
%% with an error response.
%%
%% An object is considered to be registered if there is a connection open to the
%% server, which has sent a message which registers that object.
%%
%% Any message which causes an on-going interaction with an object (for example,
%% listening to an event or observing a state) will register that object -- the
%% client that owns an object (by sending a ``_register`` message) does not have
%% to be connected for an object to be considered registered.
%%
%% When an object is registered, every path which can be formed from its path by
%% removing all characters from an ``/`` character to the end, is registered as a
%% directory (type ``dir``), and therefore can not be registered as any other
%% type. For example, if ``/foo/bar`` is registered as an action, ``/foo`` is
%% registered as type ``dir``, and can therefore not be registered as any other
%% type.
%%
%% Messages
%% --------
%%
%% Hello
%% ~~~~~
%%
%% The following messages are used to initialise a connection.

% version of the protocol that the client speaks
-type version() :: u8().

% maximum time between messages in seconds before the server considers the
% connection to be dead
-type timeout_s() :: u16().

% arbitrary ID that represents a client to the server
-type client_id() :: msgpack().

-type client_hello() :: {hello, version(), timeout_s()}.
-type client_hello_id() :: {hello_id, version(), timeout_s(), client_id()}.
-type server_hello() :: {hello}.
-type server_hello_id() :: {hello_id, client_id()}.

%% On opening the connection, the client must send a hello message, and wait for a
%% hello message in response, before other messages are transferred. Thereafter,
%% hello messages must not be used. Either:
%%
%% - The client sends `client_hello()` and the server will respond with `server_hello_id()`.
%% - The client sends `client_hello_id()` and the server will respond with `server_hello()`.
%%
%% `version()` is the version number that the client speaks, currently always
%% zero. Use of an unknown version number is an error.
%%
%% `timeout_s()` sets the timeout; if no messages are received for greater than
%% the given number of seconds, the server will assume that the connection is
%% stale, and close it. Clients should use the `ping()` message to keep the
%% connection open, and check its health.
%%
%% `client_id()` is the ID of the client. On connection, if an ID is provided,
%% (with `client_hello_id()`), any existing client connections with the same ID
%% will be closed, and their registrations cleared before subsequent messages are
%% processed. If an ID is not provided by the client (with `client_hello()`, the
%% server will generate a unique ID for the client, and this ID should be provided
%% by the client if it subsequently reconnects to the server.
%%
%% Client-generated IDs should be used wherever possible, especially for
%% microcontrollers and other devices which may experience uncontrolled restarts.
%%
%% Replies
%% ~~~~~~~

% ID used to associate replies with their request message
-type msg_id() :: u16().

% reply to the message in the other direction with a matching msg_id()
-type reply() :: {reply, msg_id(), {ok | error, msgpack()}}.

%% Most messages carry a message ID, which allows the other side to reply
%% to a specific message, asynchronously. For example, if the client sends
%% ``{ping, 42}``, it should expect to receive a response in the form of
%% ``{reply, 42, {ok, null}}``.
%%
%% Each message which expects a reply should get exactly one reply; ongoing
%% communication is always done using different messages, rather than multiple
%% replies. IDs can be re-used, but IDs with outstanding replies in a particular
%% direction must be unique.
%%
%% Ping
%% ~~~~
%%
%% Clients may ping the server to keep the connection alive and assess its health.

-type ping() :: {ping, msg_id()}.

%% After sending a `ping()`, clients should expect a matching reply from
%% the server containing ``{ok, null}``.
%%
%% Actions
%% ~~~~~~~
%%
%% Actions are used to implement remote procedure calls, with the following
%% messages:

% arguments provided when calling an action
-type call_args() :: [msgpack()].

% register an action
-type action_register() :: {action_register, msg_id(), path()}.
% call an action
-type action_call() :: {action_call, msg_id(), path(), call_args()}.

%% Clients can register an action at a given path by sending `action_register()`,
%% and should expect a reply containing ``{ok, null}``.
%%
%% `action_call()` is sent by the server to call an action that this client has
%% registered, or can be sent by a client to call an action that another client
%% has registered.
%%
%% The reply will be forwarded (including any error value, and with an appropriate
%% ID) to the client which initiated the call. It is not possible (in general) to
%% discriminate between an error from the server (because there is no object of
%% that path, or because it is of the wrong type), and the client (for any
%% reason).
%%
%% Properties
%% ~~~~~~~~~~
%%
%% Properties have essentially the same semantics as actions, except that they
%% have two different verbs, ``get`` and ``set``, and are used to allow remote
%% access to some state.

% register a property
-type prop_register() :: {prop_register, msg_id(), path()}.
% server to client: get the value of a registered property (requested by
% another client with get)
-type prop_get() :: {prop_get, msg_id(), path()}.
% server to client: set the value of a registered property (requested by
% another client with set)
-type prop_set() :: {prop_set, msg_id(), path(), msgpack()}.

%% Clients can register a property at a given path by sending `prop_register()`,
%% and should expect a reply containing ``{ok, null}``.
%%
%% When another client sends a `get()` or `set()` message for this property (see
%% :ref:`get_set_prop_state`), the server will send a `prop_get()` or
%% `prop_set()` message, and the client should reply with ``{ok, Value}`` (for
%% get), or ``{ok, null}`` (for set). The client may reply to either with an error
%% (e.g. if the state is not available, or the value is not appropriate).
%%
%% The value in the ``ok`` reply to `prop_set()` is sent to the client which
%% initiated the request, and may be used to provide feedback in cases where the
%% actual value set does not match the provided value (e.g. because of some type
%% conversion).
%%
%% Events
%% ~~~~~~
%%
%% Events implement a single-producer multi-consumer event bus. They are used to
%% notify clients when something has happened on another client. Event owners use
%% the following two messages:

% client to server: register an event
-type event_register() :: {event_register, msg_id(), path()}.
% client to server: emit an event
-type event_emit() :: {event_emit, msg_id(), path(), msgpack()}.

%% Clients can register an event at a given path by sending `event_register()`,
%% and should expect a reply containing ``{ok, null}``.
%%
%% They can then send `event_emit()` when the event occurs, and should expect a
%% reply containing ``{ok, null}``. The event (and attached data) will be
%% propagated to clients listening to this event via `event_notify()`.
%%
%% The reply normally carries no information, but can be useful to rate-limit
%% events which otherwise might end up in an ever-growing queue.

% client to server: listen to an event
-type event_listen() :: {event_listen, msg_id(), path()}.
% server to client: notification that an event has been emitted (the owner
% sent an event_emit message)
-type event_notify() :: {event_notify, path(), msgpack()}.

%% To listen to an event, a client should send an `event_listen()` message, and
%% should expect a reply containing ``{ok, null}``. The server will send an
%% `event_notify()` message when the owner of the event sends `event_emit()`.
%%
%% An event object can be listened to by many clients.
%%
%% States
%% ~~~~~~
%%
%% State objects are used to propagate some value from one client to others.  The
%% client which owns the state sends updates to the server whenever the value
%% changes. Clients can observe the state, to be sent the current value, and an
%% update whenever the value changes, or becomes unknown.
%%
%% Additionally, clients can that the owner sets the state to another value.
%%
%% States are the most complex aspect of eshet, but are the key to building
%% reliable, low-jank systems using it.
%%
%% Publishing a State
%% ++++++++++++++++++
%%
%% State owners send the following messages to publish a state:

% client to server: register a state
-type state_register() :: {state_register, msg_id(), path()}.

% client to server: set the value of the state on the server to some known
% value, and send a state_changed message to observing clients
-type state_changed() :: {state_changed, msg_id(), path(), msgpack()}.

% client to server: set the value of the state on the server to unknown,
% and send a state_changed message to observing clients
-type state_unknown() :: {state_unknown, msg_id(), path()}.

%% Clients can register a state at a given path by sending `state_register()`,
%% and should expect a reply containing ``{ok, null}``.
%%
%% Clients should send a `state_changed()` message to set the state to a known
%% value (for example, after registering the state, or when it changes), and
%% should expect a reply containing ``{ok, null}``.
%%
%% Clients should send a `state_unknown()` message to mark the state as unknown,
%% and should expect a reply containing ``{ok, null}``.
%%
%% As mentioned above, states can have two types of value; they are:
%%
%% * ``{known, Value}``: some known value. States have this value when the client
%%   which owns this state is connected to the server, and the last state update
%%   message it sent was `state_changed()`.
%% * ``unkonwn``: no value. States have this value whenever the state is non known
%%   (so, the opposite of the above condition).
%%
%% Other clients may request that the state is set to another value (see below);
%% when this happens, the server will send a `state_set()` message to the client
%% which owns the state:

-type state_set() :: {state_set, msg_id(), path(), msgpack()}.

%% The client should set the state to the requested value and reply with ``{ok,
%% null}`` if possible, or otherwise reply with an error. As with properties, the
%% value in the ``ok`` reply is sent to the client which initiated the request.
%%
%% This exchange has no effect on the value of the state (as held by the server,
%% and propagated to clients). If it does result in a change to the state, the
%% server should be notified with a `state_changed()` message.
%%
%%
%% Observing a State
%% +++++++++++++++++
%%
%% The following messages are used by clients observing states.

% observed value of a state
-type state_value() :: {known, msgpack()} | unknown.

% the time since the state last change. in erlang this is represented as
% an integer (native time), and converted for different serialisations
-type change_time() :: integer().

% client to server: start observing a state
-type state_observe_t() :: {state_observe_t, msg_id(), path()}.

% server to client: reply for state_observe_t (in addition to error)
-type reply_state_t() :: {reply_state, msg_id(), {ok, state_value(), change_time()}}.

% server to client: a notification that a state has changed
-type state_notify() :: {state_changed, path(), state_value()}.

%% Clients can start observing a state at a given path by sending
%% `state_observe_t()` and should expect either a `reply_state_t()` containing
%% the current state, or a regular error reply (type ``{reply, msg_id(), {error,
%% msgpack()}}``). The server will then send a `state_notify()` message whenever
%% the state changes.
%%
%% The `change_time()` value in `reply_state_t()` is intended to be used to
%% replicate states whose values change smoothly over time. For example, if a
%% state contains the current play-head position of a music player, clients can
%% use this to display an accurate indication of the play-head position, even if
%% the state does not update frequently.
%%
%% Additionally, two messages for observing states without the `change_time()`
%% are retained for compatibility with older clients:

% client to server: start observing a state (no time since last change)
-type state_observe() :: {state_observe, msg_id(), path()}.
% server to client: reply for state_observe (in addition to error)
-type reply_state() :: {reply_state, msg_id(), {ok, state_value()}}.

%% These behave identically to the above messages, except that the reply to
%% `state_observe()` may be `reply_state()`, rather than `reply_state_t()`.
%%
%% .. _get_set_prop_state:
%%
%% Getting and Setting Properties and States
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%
%% The following messages can be sent by clients to get and set properties or states.

% client to server: get the value of a property or state registered by
% another client
-type get() :: {get, msg_id(), path()}.

% client to server: set the value of a property or state registered by
% another client
-type set() :: {set, msg_id(), path(), msgpack()}.

%% * If path is a property, `get()` and `set()` causes the server to send
%%   `prop_get()` or `prop_set()` to the client which owns it, and forward the
%%   reply.
%%
%% * If path is a state, `get()` causes the server to reply (with `reply()`,
%%   not `reply_state()`) with the current state if it is ``known``, or an error
%%   if it is ``unknown``.
%%
%%   `set()` causes the server to send `state_set()` to the client which owns
%%   it, and forward the reply.
%%
%% .. _errors:
%%
%% Errors
%% ------
%%
%% Error structures returned by the server in `reply()` are not currently
%% defined, but may be in the future if use cases are found where varying client
%% behaviour based on error values is the best course of action.
%%
%% Handling of protocol errors (e.g. sending any message for which there is not an
%% obvious path for the server to reply with an error, for example sending a
%% `reply()` message with an unused ID) is not defined by the generic protocol.
%% In the future, a message may be added to indicate this condition, only to aid
%% in debugging.
%%
%% Wrapping up
%% -----------
%%
%% Finally, we can write the types for messages used in different situations:

% bidirectional messages
-type bidir_msg() :: action_call() | reply().

% server-to-client only messages
-type server_msg() ::
    server_hello()
    | server_hello_id()
    | prop_get()
    | prop_set()
    | event_notify()
    | reply_state()
    | reply_state_t()
    | state_notify()
    | state_set().

% client-to-server only messages
-type client_msg() ::
    client_hello()
    | client_hello_id()
    | ping()
    | action_register()
    | prop_register()
    | event_register()
    | event_emit()
    | event_listen()
    | state_register()
    | state_changed()
    | state_unknown()
    | state_observe_t()
    | state_observe()
    | get()
    | set().

% all messages
-type message() :: bidir_msg() | server_msg() | client_msg().
