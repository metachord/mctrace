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

-compile({parse_transform, mctrace}).
-mct_opts([
           {tracing, [send, procs, 'receive', timestamp]},

           {hook_send_event,                     {ttm_hooks, h_s_event}},
           {hook_send_sync_event,                {ttm_hooks, h_s_sync_event}},
           {hook_send_all_state_event,           {ttm_hooks, h_s_all_state_event}},
           {hook_send_sync_all_state_event,      {ttm_hooks, h_s_sync_all_state_event}},
           {hook_send_info,                      {ttm_hooks, h_s_info}},
           {hook_receive_event,                  {ttm_hooks, h_r_event}},
           {hook_receive_sync_event,             {ttm_hooks, h_r_sync_event}},
           {hook_receive_all_state_event,        {ttm_hooks, h_r_all_state_event}},
           {hook_receive_sync_all_state_event,   {ttm_hooks, h_r_sync_all_state_event}},
           {hook_receive_info,                   {ttm_hooks, h_r_info}},
           {hook_exit,                           {ttm_hooks, h_exit}}

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
