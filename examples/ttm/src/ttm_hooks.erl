-module(ttm_hooks).

-compile([export_all]).


h_s_event(_St, Timestamp, Pid, ToPid, Msg) ->
  io:format("SEND EVENT: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg]).

h_s_sync_event(_St, Timestamp, Pid, ToPid, Msg) ->
  io:format("SEND SYNC EVENT: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg]).

h_s_all_state_event(_St, Timestamp, Pid, ToPid, Msg) ->
  io:format("SEND ALL STATE EVENT: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg]).

h_s_sync_all_state_event(_St, Timestamp, Pid, ToPid, Msg) ->
  io:format("SEND SYNC ALL STATE EVENT: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg]).

h_s_info(_St, _Timestamp, _Pid, _ToPid, {Ref, _}) when is_reference(Ref) ->
  ignore;
h_s_info(St, Timestamp, Pid, ToPid, Msg) ->
  Module = proplists:get_value(module, St),
  case Module of
    ttm_server -> ignore;
    _ -> io:format("SEND INFO: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg])
  end.


h_r_event(St, Timestamp, Pid, Msg) ->
  Module = proplists:get_value(module, St),
  io:format("~p: RECV EVENT: ~p, ~p, ~p~n", [Module, Timestamp, Pid, Msg]).

h_r_sync_event(St, Timestamp, Pid, FromPid, Msg) ->
  Module = proplists:get_value(module, St),
  io:format("~p: RECV SYNC EVENT: ~p, ~p, ~p, ~p~n", [Module, Timestamp, Pid, FromPid, Msg]).

h_r_all_state_event(St, Timestamp, Pid, Msg) ->
  Module = proplists:get_value(module, St),
  io:format("~p: RECV ALL STATE EVENT: ~p, ~p, ~p~n", [Module, Timestamp, Pid, Msg]).

h_r_sync_all_state_event(St, Timestamp, Pid, FromPid, Msg) ->
  Module = proplists:get_value(module, St),
  io:format("~p: RECV SYNC ALL STATE EVENT: ~p, ~p, ~p, ~p~n", [Module, Timestamp, Pid, FromPid, Msg]).

h_r_info(St, Timestamp, Pid, {Ref, Msg}) when is_reference(Ref) ->
  Module = proplists:get_value(module, St),
  io:format("~p: RECV CALL REPLY: ~p, ~p, ~p~n", [Module, Timestamp, Pid, Msg]);
h_r_info(_St, _Timestamp, _Pid, _) ->
  ignore.

h_s_cast(_St, Timestamp, Pid, ToPid, Msg) ->
  io:format("SEND CAST: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg]).

h_s_call(_St, Timestamp, Pid, ToPid, Msg) ->
  io:format("SEND CALL: ~p, ~p, ~p, ~p~n", [Timestamp, Pid, ToPid, Msg]).

h_r_cast(St, Timestamp, Pid, Msg) ->
  Module = proplists:get_value(module, St),
  case Module of
    ttm_server when Msg =:= {message} orelse
                    Msg =:= {asd} ->
      io:format("~p: RECV CAST: ~p, ~p, ~p~n", [Module, Timestamp, Pid, Msg]);
    _ ->
      ignore
  end.

h_r_call(St, Timestamp, Pid, FromPid, Msg)
  when Msg =:= test ->
  Module = proplists:get_value(module, St),
  io:format("~p: RECV CALL: ~p, ~p, ~p, ~p~n", [Module, Timestamp, Pid, FromPid, Msg]);
h_r_call(_St, _Timestamp, _Pid, _FromPid, _) ->
  ignore.


h_exit(_St, Timestamp, Pid, Reason) ->
  io:format("EXIT: ~p, ~p, ~p~n", [Timestamp, Pid, Reason]).
