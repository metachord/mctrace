%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <zerthurd@gmail.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2011 by Maxim Treskin <zerthurd@gmail.com>
%%%-------------------------------------------------------------------
-module(ttm_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-ifdef(MCTRACE).
-include_lib("mctrace/include/mctrace.hrl").

-export([
         format_send_cast/5,
         format_send_call/5,
         format_send_info/5,

         format_receive_cast/4,
         format_receive_call/5,
         format_receive_info/4,

         format_exit/4

        ]).

-compile({parse_transform, mctrace}).
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
-endif.


-define(SERVER, ?MODULE).

-define(DBG(F, A), io:format("(~w:~b) " ++ F ++ "~n", [?MODULE, ?LINE | A])).

-record(state, {
          id,
          name
         }).

%%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
  {ok, #state{
     id = 123,
     name = "qqqq"
    }}.

handle_call(test, _From, State) ->
  {reply, ok, State};
handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {reply, Error, State}.

handle_cast({message} = Msg, State) ->
  timer:sleep(1),
  ?DBG("Msg: ~p", [Msg]),
  {noreply, State};
handle_cast({asd} = Msg, #state{id = Id} = State)
  when is_integer(Id) ->
  timer:sleep(1),
  ?DBG("Msg: ~p", [Msg]),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  ?DBG("Info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions

-ifdef(MCTRACE).
format_send_cast(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND CAST: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_call(_St, Timestamp, Pid, ToPid, Msg) ->
  {ok, io_lib:format("SEND CALL: ~p, ~p, ~p, ~p", [Timestamp, Pid, ToPid, Msg])}.

format_send_info(_St, _Timestamp, _Pid, _ToPid, {Ref, _}) when is_reference(Ref) ->
  ignore;
format_send_info(_St, _Timestamp, _Pid, _ToPid, _Msg) ->
  ignore.


format_receive_cast(St, Timestamp, Pid, Msg)
  when Msg =:= {message} orelse
       Msg =:= {asd} ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV CAST: ~p, ~p, ~p", [Module, Timestamp, Pid, Msg])};
format_receive_cast(_St, _Timestamp, _Pid, _) ->
  ignore.

format_receive_call(St, Timestamp, Pid, FromPid, Msg)
  when Msg =:= test ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV CALL: ~p, ~p, ~p, ~p", [Module, Timestamp, Pid, FromPid, Msg])};
format_receive_call(_St, _Timestamp, _Pid, _FromPid, _) ->
  ignore.

format_receive_info(St, Timestamp, Pid, {Ref, Msg}) when is_reference(Ref) ->
  Module = proplists:get_value(module, St),
  {ok, io_lib:format("~p: RECV CALL REPLY: ~p, ~p, ~p", [Module, Timestamp, Pid, Msg])};
format_receive_info(_St, _Timestamp, _Pid, _Msg) ->
  ignore.


format_exit(_St, Timestamp, Pid, Reason) ->
  {ok, io_lib:format("EXIT: ~p, ~p, ~p", [Timestamp, Pid, Reason])}.

-endif.
