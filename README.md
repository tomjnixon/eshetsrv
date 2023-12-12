an ESHET server, written in erlang

# build

The project is built using [rebar3](https://www.rebar3.org/). Either download a
release, or install it with your package manager (e.g. [this
package](https://aur.archlinux.org/packages/rebar3) on arch).

The product of building an erlang application is a 'release', a directory (or
tarball) containing the code for multiple 'applications' which when ran
together form a complete system, and the configuration for those applications.
A script is included to run and interact with the release.

Releases often include a copy of the runtime system, and can be relocated to a
different directory or machine.

To build the default development release, run:

```bash
rebar3 release
```

or a specific release called `prod`:

```bash
rebar3 as prod release
```

Available releases:

- `default`: development mode enabled, for hot code reloading
- `prod`: production mode

# running

built releases are in `_build/release_name/rel`. Run the prod release with:

```bash
_build/prod/rel/eshetsrv_release/bin/eshetsrv_release foreground
```

There are other options (e.g. `console` to start with an interactive console),
but foreground is generally what you'll want. This will notify systemd when
it's started, so can be ran with `Type=notify`, something like this:

```
[Unit]
Description=ESHET Server

[Service]
Type=notify
Restart=on-failure
Environment="ERL_EPMD_ADDRESS=127.0.0.1"
ExecStart=/path/to/release/bin/eshetsrv_release foreground
```

`ERL_EPMD_ADDRESS` is set to stop [EPMD](https://www.erlang.org/doc/man/epmd)
from listening on external interfaces, which is potentially surprising (though
not actually a problem) from a security point-of-view.

# development

To run the tests and type checks, run:

```bash
rebar3 do dialyzer, ct, eunit
```

During development, the most convenient way to run it is with `rebar3 shell`,
which will give you an erlang shell with the applications running. Run
`r3:do(compile).` to compile and hot-reload any changes you've made.

# functionality

The main purpose of the software is to serve clients talking the native ESHET
protocol, but it also allows for basic access over HTTP, full access using
websockets, and contains a `meta` client to access system information over
ESHET, and can be used with the erlang distribution protocol. These are
described below.

## ESHET protocol

By default, the server listens to clients speaking the native protocol on
port 11236. This can be configured by putting something like this in
`config/prod/vm.args`:

```
-eshetproto port 11236
```

This protocol is currently undocumented. The serialisation (same in both
directions) is implemented in
[eshetnet_proto_impl.erl](apps/eshetproto/src/eshetnet_proto_impl.erl), and the
flow of messages is implemented in
[eshetproto_generic.erl](apps/eshetproto/src/eshetproto_generic.erl).

This protocol is also implemented in
[eshet.py](https://github.com/tomjnixon/eshet.py) and
[eshetcpp](https://github.com/tomjnixon/eshetcpp).

## HTTP access

By default, the server listens for HTTP on port 11237. This can be configured
by putting something like this in `config/prod/vm.args`:

```
-eshethttp port 11237
```

This is mainly useful for basic integration with other systems, like automate.

Endpoints generally take JSON in their body if required, and return JSON in the
body. Add `?wrap` to any URL to wrap the result in an object to satisfy the JS
JSON parser. On error, the result will still be JSON, but with status `520
ESHET Error`.

The following endpoints are defined:

### /call/[path]

Call the action with the given path. The body should be JSON containing a list
of arguments.

For example, to list the root directory:

```
$ curl -s -d '["/"]' localhost:11237/call/meta/ls
["dir",[["meta","dir"]]]
```

### /get/[path]

Get the value of a state or property.

```
$ curl -s localhost:11237/get/some_state
5
```

### /set/[path]

Set the value of a state or property. The body should be JSON containing the
new value.

```
$ curl -s -d '7' localhost:11237/set/some_state
null
```

## websockets

The http server also speaks websockets on `/ws`, a straightforward translation
of the internal representation of the native ESHET protocol messages into JSON:
atoms become strings, tuples become lists, and times are in seconds. This is
implemented in a currently-unpublished (because I don't want to think about JS
packaging) typescript client for building web interfaces.

## meta client

Currently the meta client exposes a single action:

### /meta/ls path

Get the contents of a directory or the type of the object at a given path.

For directories, this returns `["dir", contents]`, where `contents` is a list
of directory entries, with each entry being `[name, type]`, where `type` is one
of `"dir"`, `"action"`, `"event"`, `"state"`, `"prop"`.

For example:

```
eshet call /meta/ls '"/"' | jq
[
  "dir",
  [
    [
      "meta",
      "dir"
    ],
    [
      "some_state",
      "state"
    ]
  ]
]
```

For objects, this just returns the `type` (one of the options from the list above):

```
$ eshet call /meta/ls '"/meta/ls"'
"action"
```

## erlang distribution

This server was designed to be accessed through the native erlang distribution
protocol, to enable ESHET clients written in erlang to communicate with less
serialisation overhead (and implementation complexity) than using the native
protocol. I still use this, but don't recommend it, as erlang is not the best
language for writing this kind of code.

The API for this is provided in a separate package,
[here](https://github.com/tomjnixon/eshet/blob/master/src/eshet.erl).

For example, you can use something like this from the console:

```erlang
eshet:action_call(eshetsrv_state, <<"/meta/ls">>, [<<"/">>]).
{ok,{dir,[{<<"meta">>,dir},{<<"some_state">>,state}]}}
```

Here, `eshetsrv_state` is the name of the state process in the eshetsrv
application. In another process this would be replaced with something like
`{eshetsrv_state, 'eshet@hostname.lan'}`.

Note that the distribution protocol by default is [completely
insecure](https://www.erlang.org/doc/reference_manual/distributed) and
shouldn't be exposed to the network.

# license

```
Copyright 2023 Thomas Nixon

This program is free software: you can redistribute it and/or modify it under the terms of version 3 of the GNU General Public License as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

See LICENSE.
```
