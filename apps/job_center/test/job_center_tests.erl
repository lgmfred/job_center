-module(job_center_tests).
-include_lib("eunit/include/eunit.hrl").

-spec app_test_() -> tuple().
app_test_() ->
    {"Job center application can be started and stopped",
     {inorder,
      [?_assert(try application:start(job_center) of
                    ok -> true;
                    {error, {already_started, job_center}} -> true;
                    _ -> false
                catch
                    _:_ -> false
                end),
       ?_assert(try application:stop(job_center) of
                    ok -> true;
                    _ -> false
                catch
                    _:_ -> false
                end)]}}.
