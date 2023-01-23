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
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-type state() :: #{int => integer(),
                   queue => queue:queue(),
                   refs => list(),
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

-spec add_job({pos_integer(), fun()}) -> pos_integer().
add_job(Job = {_JobTime, _Fun}) ->
    gen_server:call(?MODULE, {add_job, Job}).

-spec work_wanted() -> {integer(), fun()} | no.
work_wanted() ->
    gen_server:call(?MODULE, work_wanted).

-spec job_done(integer()) -> ok.
job_done(Int) ->
    gen_server:cast(?MODULE, {job_done, Int}).

-spec statistics() -> map().
statistics() ->
    gen_server:call(?MODULE, get_statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(list()) -> {ok, state()}.
init([]) -> 
    State = #{int => 1,
              queue => queue:new(),
              refs => [],
              in_progress => [],
              done => []},
    {ok, State}.

-spec handle_call(atom() | tuple(),_, state()) ->
    {'reply',_,_} | {'stop','normal','stopped',_}.
handle_call({add_job, {JobTime, Fun}}, _From, State)->
    #{int:=N, queue:=Q} = State,
    Q2 = queue:in({N, JobTime, Fun}, Q),
    {reply, N, State#{int:=N+1, queue:=Q2}};
handle_call(work_wanted, {Pid,_}, State) ->
    #{queue:=Q, refs:=Refs, in_progress:=L} = State,
    case queue:out(Q) of
        {{value, Work = {N,_T,_F}}, Q2} -> 
            Ref = erlang:monitor(process, Pid),
            NewRefs = [{Ref,N}|Refs],
            L1 = [Work|L],
            NewState = State#{queue:=Q2, refs:=NewRefs, in_progress:=L1},
            {reply, Work, NewState};
        {empty, _} ->
            {reply, no, State}
    end;
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(get_statistics, _From, State) ->
    #{queue := Q, in_progress := Pl, done := Dl} = State,
    {reply, #{queue => Q, in_progress => Pl, done => Dl}, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

-spec handle_cast(_,_) -> {noreply,_}.
handle_cast({job_done, N}, State) ->
    #{refs:=Refs, in_progress:=P, done:=D} = State,
    case lists:keysearch(N, 1, P) of
        {value, Tuple} -> 
            P1 = lists:keydelete(N, 1, P),
            [Ref] = [X || {X, Y} <- Refs, Y =:= N],
            NewRefs = lists:keydelete(Ref, 1, Refs),
            D1 = [Tuple|D],
            NewState = State#{refs:=NewRefs, in_progress:=P1, done:=D1},
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, _}, State) ->
    #{queue:=Q, refs:=Refs, in_progress:=L} = State,
    case lists:keysearch(Ref, 1, Refs) of
        {value, Tup = {Ref, N}} ->
            NewRefs = lists:delete(Tup, Refs),
            Job = lists:keyfind(N, 1, L),
            L1 = lists:delete(Job, L),
            Q2 = queue:in_r(Job, Q),
            NewState = State#{queue:=Q2, refs:=NewRefs, in_progress:=L1},
            {noreply, NewState};
        false -> %% Who cares? Not our responsibility!
            {noreply, State}
    end;

handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(term(), state()) -> no_return().
terminate(_Reason, _State) -> ok.

