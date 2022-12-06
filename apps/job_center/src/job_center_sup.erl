%%%-------------------------------------------------------------------
%% @doc job_center top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(job_center_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 3600},
    ChildSpecs = [child(job_center_serv)],
    {ok, {SupFlags, ChildSpecs}}.


%% @hidden
-spec child(module()) -> supervisor:child_spec().
child(Mod) ->
    #{id => Mod,
      start => {Mod, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [Mod]
     }.
