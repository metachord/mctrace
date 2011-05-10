; -*- Mode: Markdown; -*-

# mctrace

`mctrace` is an Erlang tool that makes it easy to trace process messages
and produce some meaningful output like dot sequence diagrams (planned).
[Project Page](https://github.com/metachord/mctrace)

# Building

You need `rebar` (http://github.com/basho/rebar) for
building `mctrace`.

Just add following lines in `rebar.config`:

    {deps, [
            {mctrace, ".*", {git, "git://github.com/metachord/mctrace.git", "master"}}
           ]}.

# Planned

 * Arbitrary (user-defined) behaviours support
 * Generating dot-file for Graphviz
 * Use state of process in events handling
 * Arbitrary actions on trace events (write to file, send over network)
 * Use more general hooks instead of format functions

# Description

`mctrace` uses parse_transform to inject own hooks in some functions
(now only init/1 and terminate/2,3).

Now only gen_server and gen_fsm is supported, but in future you can
define your own behaviours of processes and trace it.

Traced process sends message to `mctrace` on startup which contains its `pid()`
and some options, like list of format functions and traced events.
`mctrace` call `erlang:trace/3` function to trace specified events on process
and print it on stdout using specified format functions.

# Usage

 1. Add section in head of your erl-file:


    ```erlang
    -ifdef(MCTRACE).
     ...
    -endif.
    ```

 2. Put here following attributes:

   Header file of mctrace with record definition:

    ```erlang
    -include_lib("mctrace/include/mctrace.hrl").
    ```

   Compile option:

    ```erlang
    -compile({parse_transform, mctrace}).
    ```

   Export formatting functions (for gen_server here) if they defined
   in this module:

    ```erlang
    -export([
          format_send_cast/5,
          format_send_call/5,
          format_send_info/5,
          format_receive_cast/4,
          format_receive_call/5,
          format_receive_info/4,
          format_exit/4
         ]).
    ```

   Say to `mctrace` what you want to trace and which functions use to
   format specified trace events:

    ```erlang
    -mct_opts([
          {tracing, [send, procs, 'receive', timestamp]},
          {format_send_cast, format_send_cast},
          {format_send_call, format_send_call},
          {format_send_info, format_send_info},
          {format_receive_cast, format_receive_cast},
          {format_receive_call, format_receive_call},
          {format_receive_info, format_receive_info},
          {format_exit, format_exit}
         ]).
    ```

 3. Implement format functions (see examples in `examples/ttm/src/ttm_server.erl`)

 3. Compile source code with `-D MCTRACE`, or put analog to `rebar.config`

 4. Run `mctrace` application before yours

 5. Start your application with traced processes
