# ESHET Design

This page attempts to document the design decisions behind ESHET, and the
reasons for those decisions.

The core structure is a tree of nodes, each of which may be one of four types
(action, property, event, state).

## Tree

The tree structure mirrors that of a UNIX file system. Each node is uniquely
identified by its path through the tree, which consists of a sequence of
nonempty strings separated by `/` characters, with a leading `/` character,
like `/foo/bar/baz`.

Nodes may not be nested below other nodes -- e.g. nodes `/foo/bar` and
`/foo/bar/baz` may not coexist. This is easier to understand (a path always
refers to a single thing), makes UIs simpler, and makes tab completion of paths
easier to implement and use. Nesting nodes may be desirable to hide more
detailed nodes within a simpler interface, however this can easily be
implemented with a `detail` folder if desired.

Unlike a UNIX file system, directories are automatically created when nodes
within them are added, and automatically removed when they are empty.

## Node Types

There are four types of node:

### Action

Action nodes have an `owner`, and no other state.

Action nodes implement function-call semantics; a client may `call` an action
with an argument list; this argument list is passed to the owner, which takes
some action in response, and responds with a result value or an error, which is
passed to the calling client.

`call`ing a non-existent action results in an error.

### Property

Properties are like actions, containing an `owner` and no state, except that a
client may `get` or `set` them.

`get`ing a property causes the owner to respond with a single value or an
error.

`set`ing a property to some value causes the value to be passed to the owner,
which may respond with an error.

`get`ing or `set`ing a non-existent property results in an error.

### Event

Events are a single-producer multiple-consumer publish/subscribe mechanism,
containing an `owner` and some `listeners`.

The owner may `emit` an event containing a value, which is passed to all
listening clients as an `event_notify` message.

Clients may `listen` to an event before it has an owner without the result
being an error; this causes an event node to be visible in the tree. Events
exist as long as they have at least one `listener` or an `owner`. This is done
to ensure that clients can be started in any order.

### State

State nodes are designed to replicate some state from one `owner` to many `observers`.

The state is kept on the server. The `owner` may send a `changed` message to
the server to update the state. When a client starts observing a state, it
receives the current version of the state, then `state_changed` messages
whenever the state changes.

The state being stored on the server allows serialisation of the initial state
and `state_changed` messages, without adding additional responsibilities for
the `owner`.

As with events, clients can start observing a state before its `owner` has
registered it.

States can take two forms: `unknown` with no additional value, or `known` with
the client-provided state.

`unknown` states are observed for:

- states which have no `owner` (because the state was created by a client
  observing it, or because the owner has disconnected)
- states whose `owner` has not yet sent a `changed` message
- states whose `owner` has sent a `state_unknown` message

This is done because:

- From the perspective of a typical observer all three should be handled in the
  same way, by the same code path. For debugging and monitoring it's easy to
  provide an API which lists all nodes without an `owner`.

- It is helpful for registration and the sending of the initial value to be
  decoupled (second reason), because this simplifies clients, as registration
  can happen in one place, and change notification can happen in another. If
  these were coupled, either:

    - The state would have to be read during the setup phase. This might be
      painful to implement (e.g. in asynchronous environments, or with
      sensors which take a while to warm up), might make bugs more likely
      (now you have to change the read function in multiple places), and is
      harder to debug (because now you have to wait for all your sensors to
      warm up before they appear in the tree).

    - The client library would have to keep track of which states it has
      registered, or the user would have to implement this themselves.

  This is also consistent with the idea that nodes can exist before the owner
  has started. Decoupling these two actions isn't harmful because it is
  possible to provide an API which registers a state and sends the initial
  change request in one go, if this is more convenient.

  This also makes reconnection easier, for obvious reasons.

- The state can be explicitly set to unknown, because it makes things like
  aggregators and aliasing easier/possible, and because it's potentially useful
  for clients which want to indicate that something has broken beyond their
  control (e.g. a web service that they are polling is not responding).

  This is potentially an annoying feature if it's abused as a general-purpose
  null value, so don't do that. This is for cases where something is
  unavailable, and clients should behave the same as if the client providing it
  was not running. Clients should not have to use `{known, null}` for this,
  because now observers have another null state to detect and handle, in
  addition to the `unknown` which they _have_ to be able to handle to be
  reliable.

  For a particular state, if `unknown` and `null` have different semantics,
  then that should be used; this is included because sometimes they don't.

Another option to consider is adding some extra information to the `unknown`
state, which says why it is unknown. Is there a reason why a normal client
(i.e. not a human debugging the system) wants this?

## Reconnection

The system consists of one server and many clients, connecting to the server
over TCP. The system should be designed to not break when clients reconnect to
the server after their old connection is not closed cleanly.

The chosen method is to provide a mechanism for clients to identify themselves
to the server, so that their previous connection can be tidied up.

This identity can take the form of:

- A persistent ID, if one is built into the client. A good example of this is a
  device with a MAC address which only ever makes a single connection (e.g. an
  ESP32). When a client connects with such an ID, it is always safe to
  drop any connection with that ID.

  This might be useful in other cases too, but it's not always easy and has to
  be done at the application level; it can't be done automagically by a
  library.

- An ID allocated by the server. When a client makes its first connection, it
  is given an ID by the server. If it provides this ID when it reconnects, any
  existing connection with this ID can disconnected.

  This requires the server to allocate unique IDs, even over restarts of the
  server. The easiest way to do this is probably to generate a unique 'server
  instance ID' when it's started, then allocate client IDs sequentially or
  randomly; the full ID given is then the combination of these two.

  This could be handled nicely in a library, e.g. by reading/writing a PID file
  containing a connection ID and process ID.

It's useful to have both mechanisms, because not all clients are easy to
identify (use the second option), and it's not always easy to store a generated
ID across restarts (use the first option).
