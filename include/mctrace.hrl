-ifndef(MCTRACE_H).
-define(MCTRACE_H, 1).


-record(mctrace_init_opts, {
          module            :: atom(),
          behaviour         :: atom(),
          format            :: [{atom(), atom()}],
          tracing           :: [atom()]
         }).

-endif.                                         % MCTRACE_H
