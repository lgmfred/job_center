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

-spec worker_supervision_test_() -> tuple().
worker_supervision_test_() ->
    {"If a worker dies, the jobs (s)he was doing are returned to the "
     "pool of jobs waiting to be done.",
     ?setup(fun supervise_workers/1)}.

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
    F = fun(X) -> X end,
    N1 = job_center:add_job(3, F(13)),
    N2 = job_center:add_job(4, F(14)),
    #{queue := Q} = job_center_serv:get_state(),
    L = queue:to_list(Q),
    [?_assertEqual([1,2], [N1,N2]),
     ?_assertEqual([{1,3,F(13)},{2,4,F(14)}], L)].

-spec work_wanted(pid()) -> [ok].
work_wanted(_Pid) ->
    J1 = job_center:work_wanted(),
    F1 = fun() -> uno end,
    F2 = fun() -> deux end,
    job_center:add_job(2,F1),
    job_center:add_job(3,F2),
    J2 = [job_center:work_wanted() || _W <- lists:seq(1, 3)], 
    ?_assertEqual([no,{1,2,F1},{2,3,F2},no], [J1|J2]).

-spec queue_to_progress_list(pid()) -> [ok].
queue_to_progress_list(_Pid) ->
    F1 = fun() -> 1 end,
    F2 = fun() -> deux end,
    1 = job_center:add_job(7,F1),
    2 = job_center:add_job(3,F2),
    #{queue:=Q1, in_progress:=L1} = job_center_serv:get_state(),
    {1,7,F1} = job_center:work_wanted(),
    #{queue:=Q2, in_progress:=L2} = job_center_serv:get_state(),
    {2,3,F2} = job_center:work_wanted(),
    #{queue:=Q3, in_progress:=L3} = job_center_serv:get_state(),
    [?_assert([] == L1),
     ?_assertEqual(queue:head(Q1), hd(L2)),
     ?_assertEqual(queue:head(Q2), hd(L3)),
     ?_assert(queue:is_empty(Q3))].

-spec job_done(pid()) -> [ok].
job_done(_Pid) ->
    F1 = fun() -> 1 end,
    F2 = fun() -> deux end,
    1 = job_center:add_job(7,F1),
    2 = job_center:add_job(3,F2), 
    J1 = {N1,7,F1} = job_center:work_wanted(),
    #{done:=L1} = job_center_serv:get_state(),
    job_center:job_done(N1),
    J2 = {N2,3,F2} = job_center:work_wanted(),
    #{done:=L2} = job_center_serv:get_state(),
    job_center:job_done(N2),
    #{done:=L3} = job_center_serv:get_state(),
    [?_assert([] == L1),
     ?_assertEqual(J1, hd(L2)),
     ?_assertEqual([J2,J1], L3)].

-spec statistics(pid()) -> [ok].
statistics(_Pid) ->
    F1 = fun() -> 1 end,
    F2 = fun() -> deux end,
    F3 = fun() -> three end,
    #{queue:=Q1, in_progress:=P1, done:=D1} = job_center:statistics(),
    1 = job_center:add_job(7,F1),
    2 = job_center:add_job(3,F2), 
    3 = job_center:add_job(9,F3),
    J1 = {_N1,_T1,F1} = job_center:work_wanted(),
    J2 = {N2,_T2,F2} = job_center:work_wanted(),
    job_center:job_done(N2),
    #{queue:=Q2, in_progress:=P2, done:=D2} = job_center:statistics(),
    [?_assert(queue:is_empty(Q1)),
     ?_assert([] =:= (P1 = D1 = [])),
     ?_assertEqual({3,9,F3}, queue:head(Q2)),
     ?_assertEqual(J1, hd(P2)),
     ?_assertEqual(J2, hd(D2))].

-spec supervise_workers(pid()) -> [ok].
supervise_workers(_Pid) ->
    F1 = fun() -> 1 end,
    F2 = fun() -> deux end,
    F3 = fun() -> three end,
    1 = job_center:add_job(7,F1),
    2 = job_center:add_job(3,F2), 
    3 = job_center:add_job(9,F3),
    Pid = spawn(fun() ->
        [job_center:work_wanted() || _ <- lists:seq(1, 3)],
        job_center:job_done(2),
        timer:sleep(500)
    end),
    timer:sleep(100),
    #{queue:=Q1, in_progress:=P1, done:=D1} = job_center:statistics(),
    Val = erlang:exit(Pid, kill),
    timer:sleep(100),
    #{queue:=Q2, in_progress:=P2, done:=D2} = job_center:statistics(),
    [?_assert(is_pid(Pid)),
     ?_assert(queue:is_empty(Q1)),
     ?_assertEqual([{1,7,F1},{3,9,F3}], lists:sort(P1)),
     ?_assertEqual({2,3,F2}, hd(D1)),
     ?_assert(Val),
     ?_assertEqual([{1,7,F1},{3,9,F3}], lists:sort(queue:to_list(Q2))),
     ?_assert([] =:= P2),
     ?_assertEqual(D1, D2)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%


