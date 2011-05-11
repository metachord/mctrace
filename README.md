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

 * Generating dot-file for Graphviz
 * Use state of process in events handling

# Description

`mctrace` uses parse_transform to inject own hooks in some functions
(now only init/1 and terminate/2,3).

Traced process sends message to `mctrace` on startup which contains its `pid()`
and some options, like list of hooks functions and traced events.
`mctrace` call `erlang:trace/3` function to trace specified events on process
and print it on stdout using specified hooks functions.

# Usage

 1. Add section in head of your erl-file:


    ```erlang
    -ifdef(MCTRACE).
     ...
    -endif.
    ```

 2. Put here following attributes:

   * Header file of mctrace with record definition:

     ```erlang
     -include_lib("mctrace/include/mctrace.hrl").
     ```

   * Compile option:

     ```erlang
     -compile({parse_transform, mctrace}).
     ```

   * Export hooks functions (for gen_server here) if they defined
   in this module:

     ```erlang
     -export([
          hook_send_cast/5,
          hook_send_call/5,
          hook_send_info/5,
          hook_receive_cast/4,
          hook_receive_call/5,
          hook_receive_info/4,
          hook_exit/4
         ]).
     ```

   * Say to `mctrace` what you want to trace and which functions use to
   handle specified trace events:

     ```erlang
     -mct_opts([
          {tracing, [send, procs, 'receive', timestamp]},
          {hook_send_cast,       hook_send_cast},
          {hook_send_call,       hook_send_call},
          {hook_send_info,       hook_send_info},
          {hook_receive_cast,    hook_receive_cast},
          {hook_receive_call,    hook_receive_call},
          {hook_receive_info,    hook_receive_info},
          {hook_exit,            hook_exit}
         ]).
     ```

   * If your hooks defined in other module, write following:

     ```erlang
     ...
     {hook_send_info,       {my_hooks_mod, hook_send_info}},
     ...
     ```

 3. Implement hook functions (see examples in `examples/ttm/src/ttm_hooks.erl`)

 4. Compile source code with `-D MCTRACE`, or put analog to `rebar.config`

 5. Run `mctrace` application before yours

 6. Start your application with traced processes
