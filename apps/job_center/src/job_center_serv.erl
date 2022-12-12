%%% The core server in charge of tracking jobs.
-module(job_center_serv).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% Public API
-export([start_link/0,
         stop/0,
         add_job/1,
         work_wanted/0,
         job_done/1,
         statistics/0
        ]).

%% Internal API for tests
-export([get_state/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2
        % handle_info/2,
        % terminate/2,
        % code_change/3
        ]).

-type state() :: #{int => integer(),
                   queue => queue:queue(),
                   in_progress => list(),
                   done => list()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INTERFACE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}. 
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
    gen_server:call(?MODULE, stop).

-spec get_state() -> queue:queue().
get_state() ->
    gen_server:call(?MODULE, get_state).

-spec add_job(fun()) -> non_neg_integer().
add_job(Fun) ->
    gen_server:call(?MODULE, {add_job, Fun}).

-spec work_wanted() -> {integer(), fun()} | no.
work_wanted() ->
    gen_server:call(?MODULE, work_wanted).

-spec job_done(integer()) -> ok.
job_done(Int) ->
    gen_server:cast(?MODULE, {job_done, Int}).

-spec statistics() -> map().
statistics() ->
    gen_server:call(?MODULE, statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(list()) -> {ok, state()}.
init([]) -> 
    State = #{int => 1,
              queue => queue:new(),
              in_progress => [],
              done => []},
    {ok, State}.

-spec handle_call(atom() | tuple(),_,_) -> {'reply',_,_} | {'stop','normal','stopped',_}.
handle_call({add_job, Fun}, _From, State) ->
    #{int := N, queue := Q} = State,
    {reply, N, State#{int := N + 1, queue := queue:in({N, Fun}, Q)}};
handle_call(work_wanted, _From, State) ->
    #{queue := Q, in_progress := L} = State,
    case queue:out(Q) of
        {{value, Work}, Q2} -> 
            {reply, Work, State#{queue := Q2, in_progress := [Work|L]}};
        {empty, _} ->
            {reply, no, State}
    end;
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(statistics, _From, State) ->
    #{queue := Q, in_progress := Pl, done := Dl} = State,
    {reply, #{queue => Q, in_progress => Pl, done => Dl}, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

-spec handle_cast(_,_) -> {noreply,_}.
handle_cast({job_done, N}, #{in_progress := Pl, done := Dl} = State) ->
    case lists:keysearch(N, 1, Pl) of
        {value, Tuple} -> 
            NewPl = lists:keydelete(N, 1, Pl),
            {noreply, State#{in_progress := NewPl, done := [Tuple|Dl]}};
        false ->
            {noreply, State}
    end.
