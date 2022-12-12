-module(job_center_serv_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_stop_test_() -> [tuple()].
start_stop_test_() ->
    [{"The server can be started, stopped and has a registered name",
      ?setup(fun is_registered/1)},
     {"The queue, in_progress and done status parameters are initially empty",
      ?setup(fun initially_empty_params/1)}].

-spec add_job_test_() -> tuple().
add_job_test_() ->
    {"Adding a job returns job number and adds the job to the queue",
     ?setup(fun add_job/1)}.

-spec work_wanted_test_() -> tuple().
work_wanted_test_() ->
    [{"A worker process can request for work",
      ?setup(fun work_wanted/1)},
     {"Requested work is moved from the queue to in_progress list",
      ?setup(fun queue_to_progress_list/1)}].

-spec job_done_test_() -> tuple().
job_done_test_() ->
    {"Signal that a work has been done and that moves it to done list",
      ?setup(fun job_done/1)}.

-spec statistics_test_() -> tuple().
statistics_test_() ->
    {"Reports the status of the jobs in the queue and of jobs that "
     "are in progress and that have been done",
     ?setup(fun statistics/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> pid().
start() ->
    {ok, Pid} = job_center_serv:start_link(),
    Pid.

-spec stop(pid()) -> ok.
stop(_Pid) ->
    job_center_serv:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
-spec is_registered(pid()) -> [ok].
is_registered(Pid) ->
    [?_assert(is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(job_center_serv))].

-spec initially_empty_params(pid()) -> [ok].
initially_empty_params(_Pid) ->
    S = job_center_serv:get_state(),
    #{int := N, queue := Q, in_progress := L1, done := L2 } = S,
    [?_assert(is_map(S)),
     ?_assert(is_integer(N)),
     ?_assertEqual(1, N),
     ?_assert(queue:is_queue(Q)),
     ?_assert(queue:is_empty(Q)),
     ?_assertEqual([], L1),
     ?_assertEqual([], L2)].

-spec add_job(pid()) -> [ok].
add_job(_Pid) ->
    F1 = fun() -> some_job end,
    F2 = fun(X) -> X * 2 end,
    F3 = fun(X, Y) -> X * Y end,
    L1 = [job_center:add_job(X) || X <- [F1, F2, F3]],
    L2 = [Z || Z <- L1, is_integer(Z)],
    #{queue := Q} = job_center_serv:get_state(),
    L3 = queue:to_list(Q),
    [?_assertEqual(L1, L2),
     ?_assertEqual(L1, [1,2,3]),
     ?_assert(queue:is_queue(Q)),
     ?_assertEqual([{1,F1},{2,F2},{3,F3}], L3)].

-spec work_wanted(pid()) -> [ok].
work_wanted(_Pid) ->
    J1 = job_center:work_wanted(),
    F1 = fun() -> uno end,
    F2 = fun() -> 2 end,
    F3 = fun() -> trois end,
    L1 = [{1, F1}, {2, F2}, {3, F3}],
    [job_center:add_job(X) || X <- [F1, F2, F3]],
    L2 = [job_center:work_wanted() || _W <- lists:seq(1, 3)], 
    J2 = job_center:work_wanted(),
    job_center:add_job(fun() -> 77 * 7 end),
    {N, Fun} = job_center:work_wanted(),
    L3 = [Y() || {_, Y} <- L2],
    [?_assertEqual(no, J1),
     ?_assertEqual(no, J2),
     ?_assertEqual(L1, L2),
     ?_assertEqual([uno, 2, trois], L3),
     ?_assert(is_integer(N)),
     ?_assert(is_function(Fun)),
     ?_assertEqual(539, Fun())
    ].

-spec queue_to_progress_list(pid()) -> [ok].
queue_to_progress_list(_Pid) ->
    F1 = fun() -> 1 end,
    N1 = job_center_serv:add_job(F1),
    #{queue := Q1, in_progress := PL1} = job_center_serv:get_state(),
    W1 = job_center:work_wanted(),
    #{queue := Q2, in_progress := PL2} = job_center_serv:get_state(),
    Lx = [fun() -> Xx end || Xx <- lists:seq(2, 5)],
    L3 = [job_center:add_job(X) || X <- Lx],
    #{queue := Q3, in_progress := [W1]} = job_center_serv:get_state(),
    L4 = [job_center:work_wanted() || _Yy <- L3],
    #{queue := Q4, in_progress := PL4} = job_center_serv:get_state(),
    [?_assert(false == queue:is_empty(Q1)),
     ?_assert(queue:member({N1, F1}, Q1)),
     ?_assertEqual([], PL1),
     ?_assert(queue:is_empty(Q2)),
     ?_assert(lists:member(W1, PL2)),
     ?_assertEqual(lists:zip(lists:seq(2, 5), Lx), queue:to_list(Q3)),
     ?_assert(queue:is_empty(Q4)),
     ?_assertEqual([W1|L4], lists:sort(PL4))].

-spec job_done(pid()) -> [ok].
job_done(_Pid) ->
    #{done := DL1} = job_center_serv:get_state(),
    [job_center:add_job(X) || X <- [fun() -> Y end || Y <- lists:seq(1, 15)]],
    L1 = [job_center:work_wanted() || _ <- lists:seq(1, 15)],
    [job_center:job_done(Xx) || Xx <- lists:seq(16, 30)], 
    #{done := DL2} = job_center_serv:get_state(),
    [job_center:job_done(Wn) || {Wn, _} <- L1],
    #{done := DL3} = job_center_serv:get_state(),
    [?_assertEqual({[],[]}, {DL1, DL2}),
     ?_assertEqual(L1, lists:sort(DL3))].

-spec statistics(pid()) -> [ok].
statistics(_Pid) ->
    #{queue := Q1, in_progress := PL1, done := DL1} = job_center:statistics(),
    [job_center:add_job(X) || X <- [fun() -> Y end || Y <- lists:seq(1, 15)]],
    [job_center:work_wanted() || _ <- lists:seq(1, 10)],
    [job_center:job_done(N) || N <- lists:seq(1, 5)],
    #{queue := Q2, in_progress := PL2, done := DL2} = job_center:statistics(),
    Qn = [Xn || {Xn, _} <- queue:to_list(Q2)],
    PLn = [Yn || {Yn, _} <- PL2],
    DLn = [Zn || {Zn, _} <- DL2],
    [?_assert(queue:is_empty(Q1)),
     ?_assertEqual([], PL1),
     ?_assertEqual([], DL1),
     ?_assertEqual(lists:seq(11, 15), Qn),
     ?_assertEqual(lists:seq(6, 10), lists:sort(PLn)),
     ?_assertEqual(lists:seq(1, 5),lists:sort(DLn))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%


