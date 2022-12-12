%%%-------------------------------------------------------------------
%% @doc job_center public API
%% @end
%%%-------------------------------------------------------------------
-module(job_center).
-behaviour(application).

-export([start/2, stop/1]).
-export([add_job/1, work_wanted/0, job_done/1, statistics/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Application behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _Args) ->
    job_center_sup:start_link().

%% @hidden
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add a job to the job queue
-spec add_job(fun()) -> non_neg_integer().
add_job(Fun) ->
    job_center_serv:add_job(Fun).

%% @doc Request work
-spec work_wanted() -> {integer(), fun()} | no.
work_wanted() ->
    job_center_serv:work_wanted().

%% @doc Signal that a job has been done
-spec job_done(integer()) -> ok.
job_done(Int) ->
    job_center_serv:job_done(Int).

%% @doc Add a statistics call to the application
-spec statistics() -> map().
statistics() ->
    job_center_serv:statistics().
