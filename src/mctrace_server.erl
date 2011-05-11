% -*- Mode: Erlang; tab-width: 2 -*-

%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <zerthurd@gmail.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2011 by Maxim Treskin <zerthurd@gmail.com>
%%%-------------------------------------------------------------------
-module(mctrace_server).
-author('Maxim Treskin <zerthurd@gmail.com>').

-include("mctrace.hrl").

-behaviour(gen_server).

-export([
         start/0,
         init_tracing/1,
         terminate_tracing/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


-define(DBG(F, A), io:format("(~w:~b) " ++ F ++ "~n", [?MODULE, ?LINE | A])).

-define(TR(F, A), io:format("#### " ++ F ++ "~n", A)).

-define(SERVER, mctrace).

-record(state, {
          tid        :: ets:tid()
         }).

%%% API
start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

init_tracing(Opts) ->
  gen_server:cast(?SERVER, {init_tracing, self(), Opts}).

terminate_tracing() ->
  gen_server:cast(?SERVER, {terminate_tracing, self()}).

%%% gen_server callbacks

init([]) ->
  Tid = ets:new(?SERVER, [set]),
  {ok, #state{tid = Tid}}.

handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast({init_tracing, Pid,
             #mctrace_init_opts{module = Module,
                                behaviour = Behaviour,
                                hooks = Hooks,
                                tracing = TraceOpts}},
            #state{tid = Tid} = State) ->
  ?DBG("Init tracing: ~p", [Pid]),
  %%?DBG("Process info: ~p", [erlang:process_info(Pid)]),
  erlang:trace(Pid, true, TraceOpts),
  Ref = erlang:monitor(process, Pid),
  %% Store to ETS
  ets:insert(Tid, {Pid, Ref, Module, Behaviour, Hooks}),

  {noreply, State};
handle_cast({terminate_tracing, Pid}, State) ->
  ?DBG("Terminate tracing: ~p", [Pid]),
  {noreply, State};
handle_cast(_Msg, State) ->
  ?DBG("Cast message: ~p", [_Msg]),
  {noreply, State}.

%% Trace message has variable length depend on timestamp option
%% so handle it in that way
handle_info(Trace, #state{tid = Tid} = State)
  when (element(1, Trace) =:= trace orelse
        element(1, Trace) =:= trace_ts) ->
  Pid = element(2, Trace),
  Action = element(3, Trace),
  Timestamp = element(1, Trace) =:= trace_ts,

  case ets:lookup(Tid, Pid) of
    [{Pid, _Ref, Module, Behaviour, Hooks}] ->
      St = [{module, Module}, {behaviour, Behaviour} | Hooks],
      case Action of
        _ when
            (Action =:= send orelse
             Action =:= send_to_non_existing_process)
            ->
          %% SEND
          {Msg, ToPid, Ts} =
            if Timestamp ->
                {trace_ts, _, _, MsgV, ToPidV, TsV} = Trace,
                {MsgV, ToPidV, TsV};
               true ->
                {trace, _, _, MsgV, ToPidV} = Trace,
                {MsgV, ToPidV, undefined}
            end,
          if (ToPid =/= self() andalso
              ToPid =/= ?SERVER) ->
              RealToPid =
                if is_atom(ToPid) -> whereis(ToPid);
                   true -> ToPid
                end,
              NewSt =
                case ets:lookup(Tid, RealToPid) of
                  [{ToPid, _ToRef, _ToModule, ToBehaviour, _ToHooks}] ->
                    [{to_behaviour, ToBehaviour} | St];
                  _ ->
                    %% Use process own behaviour if behaviour of destination is unknown
                    [{to_behaviour, Behaviour} | St]
                end,
              trace_pr(NewSt, Ts, Pid, Action, {Msg, ToPid});
             true ->
              %% Ignore
              ok
          end;
        'receive' ->
          %% RECEIVE
          {Msg, Ts} =
            if Timestamp ->
                {trace_ts, _, _, MsgV, TsV} = Trace,
                {MsgV, TsV};
               true ->
                {trace, _, _, MsgV} = Trace,
                {MsgV, undefined}
            end,
          trace_pr(St, Ts, Pid, Action, Msg);
        exit ->
          %% EXIT
          {Reason, Ts} =
            if Timestamp ->
                {trace_ts, _, _, ReasonV, TsV} = Trace,
                {ReasonV, TsV};
               true ->
                {trace, _, _, ReasonV} = Trace,
                {ReasonV, undefined}
            end,
          trace_pr(St, Ts, Pid, Action, Reason);
        _ ->
          ok
      end;
    Other ->
      ?DBG("There is no pid ~p in mctrace ets table: ~p", [Pid, Other])
  end,
  {noreply, State};


handle_info({delete_pid, Pid}, #state{tid = Tid} = State) ->
  %% Remove from ETS
  ets:delete(Tid, Pid),
  {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  %% Delayed remove Pid from ETS
  timer:send_after(1000, self(), {delete_pid, Pid}),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions

trace_pr(St, Ts, Pid, Action, Args) ->
  try
    trace_str(St, Ts, Pid, Action, Args)
  catch
    C:W ->
      ?DBG("Exception: ~p:~p, ~p", [C, W, erlang:get_stacktrace()])
  end.

trace_str(St, undefined, Pid, Action, Args) ->
  trace_str(St, now(), Pid, Action, Args);

trace_str(St, Ts, Pid, send, {Data, ToPid}) ->
  Behaviour = proplists:get_value(to_behaviour, St),
  Default = proplists:get_value(hook_send_info, St),
  case Behaviour of
    gen_server ->
      case Data of
        {'$gen_call', {Pid, _Ref}, Msg} ->
          {M, F} = proplists:get_value(hook_send_call, St, Default),
          check_apply(M, F, [St, Ts, Pid, ToPid, Msg]);
        {'$gen_cast', Msg} ->
          {M, F} = proplists:get_value(hook_send_cast, St, Default),
          check_apply(M, F, [St, Ts, Pid, ToPid, Msg]);
        Msg ->
          {M, F} = Default,
          check_apply(M, F, [St, Ts, Pid, ToPid, Msg])
      end;
    gen_fsm ->
      case Data of
        Msg ->
          {M, F} = Default,
          check_apply(M, F, [St, Ts, Pid, ToPid, Msg])
      end;
    _ ->
      {M, F} = Default,
      check_apply(M, F, [St, Ts, Pid, ToPid, Data])
  end;


trace_str(St, Ts, Pid, 'receive', Data) ->
  Behaviour = proplists:get_value(behaviour, St),
  Default = proplists:get_value(hook_receive_info, St),
  case Behaviour of
    gen_server ->
      case Data of
        {'$gen_call', {FromPid, _Ref}, Msg} ->
          {M, F} = proplists:get_value(hook_receive_call, St, Default),
          check_apply(M, F, [St, Ts, Pid, FromPid, Msg]);
        {'$gen_cast', Msg} ->
          {M, F} = proplists:get_value(hook_receive_cast, St, Default),
          check_apply(M, F, [St, Ts, Pid, Msg]);
        Msg ->
          {M, F} = Default,
          check_apply(M, F, [St, Ts, Pid, Msg])
      end;
    gen_fsm ->
      case Data of
        {'$gen_sync_event', {FromPid, _Ref}, Msg} ->
          {M, F} = proplists:get_value(hook_receive_sync_event, St, Default),
          check_apply(M, F, [St, Ts, Pid, FromPid, Msg]);
        {'$gen_event', Msg} ->
          {M, F} = proplists:get_value(hook_receive_event, St, Default),
          check_apply(M, F, [St, Ts, Pid, Msg]);
        {'$gen_sync_all_state_event', {FromPid, _Ref}, Msg} ->
          {M, F} = proplists:get_value(hook_receive_sync_all_state_event, St, Default),
          check_apply(M, F, [St, Ts, Pid, FromPid, Msg]);
        {'$gen_all_state_event', Msg} ->
          {M, F} = proplists:get_value(hook_receive_all_state_event, St, Default),
          check_apply(M, F, [St, Ts, Pid, Msg]);
        Msg ->
          {M, F} = Default,
          check_apply(M, F, [St, Ts, Pid, Msg])
      end;
    _ ->
      {M, F} = Default,
      check_apply(M, F, [St, Ts, Pid, Data])
  end;

trace_str(St, Ts, Pid, exit, Reason) ->
  {M, F} = proplists:get_value(hook_exit, St),
  check_apply(M, F, [St, Ts, Pid, Reason]).

check_apply(M, F, Args) ->
  code:ensure_loaded(M),
  case erlang:function_exported(M, F, length(Args)) of
    true -> apply(M, F, Args);
    false -> ok
  end.
