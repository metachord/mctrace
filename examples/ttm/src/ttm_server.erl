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

-compile({parse_transform, mctrace}).
-mct_opts([
           {tracing, [send, procs, 'receive', timestamp]},

           {hook_send_cast,        {ttm_hooks, h_s_cast}},
           {hook_send_call,        {ttm_hooks, h_s_call}},

           {hook_receive_cast,     {ttm_hooks, h_r_cast}},
           {hook_receive_call,     {ttm_hooks, h_r_call}},
           {hook_receive_info,     {ttm_hooks, h_r_info}},

           {hook_exit,             {ttm_hooks, h_exit}}

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
