%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <zerthurd@gmail.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2011 by Maxim Treskin <zerthurd@gmail.com>
%%%-------------------------------------------------------------------
-module(ttm_fsm).

-behaviour(gen_fsm).

-export([start_link/1]).

-export([
         init/1,
         state_name/2,
         state_name/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).


-ifdef(MCTRACE).
-include_lib("mctrace/include/mctrace.hrl").

-export([
         format_send_event/5,
         format_send_sync_event/5,

         format_send_all_state_event/5,
         format_send_sync_all_state_event/5,

         format_send_info/5,

         format_receive_event/4,
         format_receive_sync_event/5,

         format_receive_all_state_event/4,
         format_receive_sync_all_state_event/5,

         format_receive_info/4,

         format_exit/4

        ]).

-compile({parse_transform, mctrace}).
-mct_opts([
           {tracing, [send, procs, 'receive', timestamp]},

           {format_send_event, format_send_event},
           {format_send_sync_event, format_send_sync_event},

           {format_send_all_state_event, format_send_all_state_event},
           {format_send_sync_all_state_event, format_send_sync_all_state_event},

           {format_send_info, format_send_info},

           {format_receive_event, format_receive_event},
           {format_receive_sync_event, format_receive_sync_event},

           {format_receive_all_state_event, format_receive_all_state_event},
           {format_receive_sync_all_state_event, format_receive_sync_all_state_event},

           {format_receive_info, format_receive_info},

           {format_exit, format_exit}

          ]).
-endif.


-define(SERVER, ?MODULE).

-record(state, {
          srv
         }).

start_link(Srv) ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Srv], []).

%%% gen_fsm callbacks
init([Srv]) ->
  {ok, state_name, #state{srv = Srv}}.

state_name(info, State) ->
  ttm_server ! info_to_server,
  {next_state, state_name, State};
state_name(call, State) ->
  gen_server:call(ttm_server, call_to_server),
  {next_state, state_name, State};
state_name(_Event, State) ->
  {next_state, state_name, State}.

state_name(_Event, _From, State) ->
  {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
  Error = {unknown_sync_event, Event},
  {reply, Error, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%% Internal functions


-ifdef(MCTRACE).


format_send_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_sync_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND SYNC EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_all_state_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND ALL STATE EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_sync_all_state_event(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND SYNC ALL STATE EVENT: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_info(_St, _Timestamp, _Pid, _ToPid, {Ref, _}) when is_reference(Ref) ->
  ignore;
format_send_info(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND INFO: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.


format_receive_event(St, Timestamp, Pid, Msg) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV EVENT: ~p, ~p, ~p", [Module, Timestamp, Pid, Msg])}.

format_receive_sync_event(St, Timestamp, Pid, FromPid, Msg) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV SYNC EVENT: ~p, ~p, ~p, ~p", [Module, Timestamp, Pid, FromPid, Msg])}.

format_receive_all_state_event(St, Timestamp, Pid, Msg) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV ALL STATE EVENT: ~p, ~p, ~p", [Module, Timestamp, Pid, Msg])}.

format_receive_sync_all_state_event(St, Timestamp, Pid, FromPid, Msg) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV SYNC ALL STATE EVENT: ~p, ~p, ~p, ~p", [Module, Timestamp, Pid, FromPid, Msg])}.

format_receive_info(St, Timestamp, Pid, {Ref, Msg}) when is_reference(Ref) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV CALL REPLY: ~p, ~p, ~p", [Module, Timestamp, Pid, Msg])};
format_receive_info(St, Timestamp, Pid, Msg) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV INFO: ~p, ~p, ~p", [Module, Timestamp, Pid, Msg])}.


format_exit(_St, Timestamp, Pid, Reason) ->
  {ok, io_lib:format("EXIT: ~p, ~p, ~p", [Timestamp, Pid, Reason])}.

-endif.
