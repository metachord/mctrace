% -*- Mode: Erlang; tab-width: 2 -*-

%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <zerthurd@gmail.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2011 by Maxim Treskin <zerthurd@gmail.com>
%%%-------------------------------------------------------------------
-module(mctrace).
-author('Maxim Treskin <zerthurd@gmail.com>').


%% API
-export([
         parse_transform/2,
         start/0,
         init_tracing/1,
         terminate_tracing/0
        ]).

parse_transform(Forms, Opts) ->
  mctrace_pt:parse_transform(Forms, Opts).


start() ->
  mctrace_server:start().

init_tracing(Opts) ->
  mctrace_server:init_tracing(Opts).

terminate_tracing() ->
  mctrace_server:terminate_tracing().
