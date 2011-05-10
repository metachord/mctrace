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
         terminate_tracing/0,

         %% gen_server
         format_send_cast/5,
         format_send_call/5,

         format_receive_cast/4,
         format_receive_call/5,

         %% gen_fsm
         format_send_event/5,
         format_send_sync_event/5,

         format_send_all_state_event/5,
         format_send_sync_all_state_event/5,

         format_receive_event/4,
         format_receive_sync_event/5,

         format_receive_all_state_event/4,
         format_receive_sync_all_state_event/5,

         %% Common
         format_send_info/5,
         format_receive_info/4,
         format_exit/4

        ]).

parse_transform(Forms, Opts) ->
  mctrace_pt:parse_transform(Forms, Opts).


start() ->
  mctrace_server:start().

init_tracing(Opts) ->
  mctrace_server:init_tracing(Opts).

terminate_tracing() ->
  mctrace_server:terminate_tracing().


format_send_cast(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND CAST: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_call(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND CALL: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_info(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND INFO: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_receive_cast(_St, Timestamp, Pid, Msg) ->
  {ok, io_lib:format("RECV CAST: ~p, ~p, ~p", [Timestamp, Pid, Msg])}.

format_receive_call(_St, Timestamp, Pid, FromPid, Msg) ->
  {ok, io_lib:format("RECV CALL: ~p, ~p, ~p, ~p", [Timestamp, Pid, FromPid, Msg])}.

format_receive_info(_St, Timestamp, Pid, Msg) ->
  {ok, io_lib:format("RECV INFO: ~p, ~p, ~p", [Timestamp, Pid, Msg])}.



format_send_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_sync_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND SYNC EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_all_state_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND ALL STATE EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_sync_all_state_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND SYNC ALL STATE EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.


format_receive_event(_St, Timestamp, Pid, Msg) ->
  {ok, io_lib:format("RECV EVENT: ~p, ~p, ~p", [Timestamp, Pid, Msg])}.

format_receive_sync_event(_St, Timestamp, Pid, FromPid, Msg) ->
  {ok, io_lib:format("RECV SYNC EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, FromPid, Msg])}.

format_receive_all_state_event(_St, Timestamp, Pid, Msg) ->
  {ok, io_lib:format("RECV ALL STATE EVENT: ~p, ~p, ~p", [Timestamp, Pid, Msg])}.

format_receive_sync_all_state_event(_St, Timestamp, Pid, FromPid, Msg) ->
  {ok, io_lib:format("RECV SYNC ALL STATE EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, FromPid, Msg])}.

format_exit(_St, Timestamp, Pid, Reason) ->
  {ok, io_lib:format("EXIT: ~p, ~p, ~p", [Timestamp, Pid, Reason])}.
