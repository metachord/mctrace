% -*- Mode: Erlang; tab-width: 2 -*-

%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <zerthurd@gmail.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2011 by Maxim Treskin <zerthurd@gmail.com>
%%%-------------------------------------------------------------------
-module(mctrace_pt).
-author('Maxim Treskin <zerthurd@gmail.com>').

-include("mctrace.hrl").

-export([
         parse_transform/2,
         format_error/1
        ]).

-record(context, {
          module,
          behaviour,
          mct_opts       = [],
          function,
          arity
         }).

-define(DBG(F, A), io:format("(~w:~b) " ++ F ++ "~n", [?MODULE, ?LINE | A])).

-define(HERE, {?MODULE, ?LINE}).

-define(ERROR(R, F, I),
        begin
          rpt_error(R, F, I),
          throw({error,get_pos(I),{unknown,R}})
        end).

%%% API

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
parse_transform(Forms, Options) ->
  [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
  try do_transform(Forms, Options) of
      Res ->
      %%?DBG("Res:~n~p", [Res]),
      Res
  catch
    throw:{error, Ln, What} ->
	    {error, [{File, [{Ln, ?MODULE, What}]}], []}
  end.


do_transform(Forms, _Options) ->
  Fun1 =
    fun(function, Function, Ctx, _Acc) ->
        RetForms = put_trace(Function, Ctx),
        {RetForms, false, []};
       (_, Fs, _Ctx, _Acc) ->
        {Fs, false, []}
    end,
  {Forms1, _Acc1} = pass(Forms, Fun1, []),
  Forms1.


put_trace({function, Line, init = FunName, 1 = Arity, Clauses},
          #context{module = Module, behaviour = Behaviour,
                   mct_opts = MctOpts}) ->
  PutTr =
    fun
      ({clause, ClLine, ClMatch, ClGuards, ClBody}) ->
        HookGf =
          fun(Key) ->
              case proplists:get_value(Key, MctOpts) of
                undefined -> {undefined, undefined};
                {FMod, FFun} when is_atom(FMod) andalso is_atom(FFun) ->
                  {FMod, FFun};
                FFun when is_atom(FFun) ->
                  {Module, FFun};
                Other ->
                  throw({error, {bad_hook_function, Other}})
              end
          end,
        Hooks =
          case Behaviour of
            gen_server ->
              [
               {hook_receive_cast, HookGf(hook_receive_cast)},
               {hook_receive_call, HookGf(hook_receive_call)},
               {hook_receive_info, HookGf(hook_receive_info)},
               {hook_send_cast, HookGf(hook_send_cast)},
               {hook_send_call, HookGf(hook_send_call)},
               {hook_send_info, HookGf(hook_send_info)},
               {hook_exit, HookGf(hook_exit)}
              ];
            gen_fsm ->
              [
               {hook_receive_event, HookGf(hook_receive_event)},
               {hook_receive_sync_event, HookGf(hook_receive_sync_event)},
               {hook_receive_all_state_event, HookGf(hook_receive_all_state_event)},
               {hook_receive_sync_all_state_event, HookGf(hook_receive_sync_all_state_event)},
               {hook_receive_info, HookGf(hook_receive_info)},
               {hook_send_info, HookGf(hook_send_info)},
               {hook_exit, HookGf(hook_exit)}
              ];
            _ ->
              [
               {hook_receive_info, HookGf(hook_receive_info)},
               {hook_send_info, HookGf(hook_send_info)},
               {hook_exit, HookGf(hook_exit)}
              ]
          end,
        Tracing = proplists:get_value(tracing, MctOpts, []),

        {ok, TsCall, _} = erl_scan:string(
                            lists:flatten(
                              io_lib:format("mctrace:init_tracing("
                                            "#mctrace_init_opts{"
                                            "module = ~p,"
                                            "behaviour = ~p,"
                                            "hooks = ~p,"
                                            "tracing = ~p"
                                            "}"
                                            ").",
                                            [Module, Behaviour, Hooks, Tracing])), ClLine),
        {ok, [TraceFunCall]} = erl_parse:parse_exprs(TsCall),
        {clause, ClLine, ClMatch, ClGuards, [TraceFunCall | ClBody]};
      (Other) ->
        Other
    end,
  NewClauses = [PutTr(Cl) || Cl <- Clauses],
  {function, Line, FunName, Arity, NewClauses};
put_trace({function, Line, terminate = FunName, Arity, Clauses},
          #context{module = _Module, behaviour = _Behaviour}) ->
  PutTr =
    fun
      ({clause, ClLine, ClMatch, ClGuards, ClBody}) ->
        {ok, TsCall, _} = erl_scan:string("mctrace:terminate_tracing().", ClLine),
        {ok, [TraceFunCall]} = erl_parse:parse_exprs(TsCall),
        {clause, ClLine, ClMatch, ClGuards, [TraceFunCall | ClBody]};
      (Other) ->
        Other
    end,
  NewClauses = [PutTr(Cl) || Cl <- Clauses],
  {function, Line, FunName, Arity, NewClauses};
put_trace(Function, _Ctx) ->
  Function.

%%% ========== generic parse_transform stuff ==============
%%% From exprecs by Ulf Wiger <ulf.wiger@ericsson.com>. Great thanks, Ulf!

transform(Forms, F, Acc) ->
  CtxFun =
    fun({_L, module, M},
        #context{module = undefined} = Ctx) ->
        Ctx#context{module = M};
       ({_L, behaviour, B},
        #context{behaviour = undefined} = Ctx) ->
        Ctx#context{behaviour = B};
       ({_L, mct_opts, O},
        #context{mct_opts = MOpts} = Ctx) ->
        Ctx#context{mct_opts = MOpts ++ O};
       (_, Ctx) ->
        Ctx
    end,
  Context = lists:foldl(CtxFun, #context{},
                        [{L, Attr, M} || {attribute, L, Attr, M} <- Forms]),
  transform(Forms, F, Context, Acc).

transform(_Forms, _F, #context{module = undefined}, _Acc) ->
  ?ERROR(missing_module_attribute, ?HERE, []);
transform(Forms, F, Context, Acc) ->
  F1 =
    fun(Form, Acc0) ->
        Type = erl_syntax:type(Form),
        {Before1, Form1, After1, Recurse, Acc1} =
          try F(Type, Form, Context, Acc0) of
              {F1, Rec1, A1} ->
              {[], F1, [], Rec1, A1};
              {_Be1, _F1, _Af1, _Rec1, _Ac1} = Res1 ->
              Res1
          catch
            error:Reason ->
              ?ERROR(Reason,
                     ?HERE,
                     [{type, Type},
                      {context, Context},
                      {acc, Acc},
                      {form, Form}])
          end,
        if Recurse == true ->
            case erl_syntax:subtrees(Form1) of
              [] ->
                {Before1, Form1, After1, Acc1};
              ListOfLists ->
                {NewListOfLists, NewAcc} =
                  mapfoldl(
                    fun(L, AccX) ->
                        transform(
                          L, F,
                          new_context(
                            Form1, Context), AccX)
                    end, Acc1, ListOfLists),
                NewForm =
                  erl_syntax:update_tree(
                    Form, NewListOfLists),
                {Before1, NewForm, After1, NewAcc}
            end;
           true ->
            {Before1, Form1, After1, Acc1}
        end
    end,
  mapfoldl(F1, Acc, Forms).


new_context(Form, Context0) ->
  case erl_syntax:type(Form) of
    function ->
	    {Fun, Arity} =
        erl_syntax_lib:analyze_function(Form),
	    Context0#context{function = Fun,
                       arity = Arity};
    _ ->
	    Context0
  end.




%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
mapfoldl(F, Accu0, [Hd|Tail]) ->
  {Before, Res, After, Accu1} =
    case F(Hd, Accu0) of
	    {Be, _, Af, _} = Result when is_list(Be), is_list(Af) ->
        Result;
	    {R1, A1} ->
        {[], R1, [], A1}
    end,
  {Rs, Accu2} = mapfoldl(F, Accu1, Tail),
  {Before ++ [Res| After ++ Rs], Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[], Accu}.

rpt_error(Reason, Fun, Info) ->
  Fmt = lists:flatten(
          ["*** ERROR in parse_transform function:~n"
           "*** Reason     = ~p~n",
           "*** Location: ~p~n",
           ["*** ~10w = ~p~n" || _ <- Info]]),
  Args = [Reason, Fun |
          lists:foldr(
            fun({K,V}, Acc) ->
                [K, V | Acc]
            end, [], Info)],
  io:format(Fmt, Args).


format_error({_Cat, Error}) ->
  Error.

pass(Forms, Fun, Acc) ->
  {NewTree, NewAcc} = transform(Forms, Fun, Acc),
  NewForms = [erl_syntax:revert(T) || T <- lists:flatten(NewTree)],
  {NewForms, NewAcc}.


get_pos(I) ->
  case proplists:get_value(form, I) of
    undefined ->
	    0;
    Form ->
	    erl_syntax:get_pos(Form)
  end.
