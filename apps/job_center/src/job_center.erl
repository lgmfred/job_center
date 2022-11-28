%%%-------------------------------------------------------------------
%% @doc job_center public API
%% @end
%%%-------------------------------------------------------------------
-module(job_center).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    job_center_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%% internal functions
