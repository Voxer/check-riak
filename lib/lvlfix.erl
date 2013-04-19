-module(lvlfix).
-compile(export_all).

-define(LOG, "/var/tmp/riak/compaction-status-").
-record(lvl, {ref, name, log}).

main([]) ->
    usage();
main(Args) when is_list(Args), length(Args) == 1 ->
    %%validate the environment
    check_env(),
    [fix_dir(Dir) ||
        Dir <- Args];
main(_) ->
    usage().

usage() ->
    io:format("escript lvlfix.erl </path/to/partition/dir/>~n").

check_env() ->
    LimitStr = os:cmd("ulimit -n"),
    Limit =  string:to_integer(string:strip(LimitStr, right, $\n)),

    case Limit > 200000 of 
        false ->
            io:format("file limit must be at least 200000~n"),
            exit("too few files");
        _ -> ok
    end,
    case os:cmd("ps axww | egrep 'bea[m].*basho'") of
        [] -> ok;
        _ -> exit("riak might still be running")
    end.

fix_dir(Dir) ->
    
    Opts = [{max_open_files, 2000},
            {use_bloomfilter, true},
            {write_buffer_size, 45 * 1024 * 1024},
            {compression,false}],
    maybe_repair(Dir, Opts),
    {ok, Ref} = eleveldb:open(Dir, Opts),
    Name = lists:nth(1, lists:reverse(string:tokens(Dir, "/"))),
    maybe_compact(#lvl{ref=Ref, name=Name, log=undefined}).

maybe_repair(Dir, Opts) ->
    Errors = os:cmd("grep -l 'Compaction error' " ++ Dir ++ "/LOG"),
    Already = os:cmd("tail " ++ Dir ++ "/LOG| grep -l Repaired"),
    
    case Already of 
        [] -> 
            case Errors of 
                [] ->
                    io:format("No errors seen~n"), 
                    "No repair needed";
                _ -> eleveldb:repair(Dir, Opts)
            end;
        _ ->
            io:format("Already repaired~n"), 
            "No repair needed"
    end.

maybe_compact(Lvl=#lvl{ref=Ref}) ->
    
    print_stats(Lvl),
    case num0(Ref) < 10 of
        true ->
            "No compaction necessary";
        false -> 
            compact(Lvl)
    end.

out(#lvl{log=undefined}, Fmt, Args) ->
    io:format(Fmt, Args);
out(#lvl{log=Log}, Fmt, Args) ->
    io:format(Fmt, Args),    
    io:fwrite(Log, Fmt, Args).

compact(Lvl0=#lvl{ref=Ref, name=Name}) ->
    {ok, Log} = file:open(?LOG ++ Name, [append]),
    Lvl = Lvl0#lvl{log=Log},
    Pid = self(),
    spawn(fun() ->
                  print_stats(Lvl),
                  out(Lvl, "Starting compaction for ~p~n", [Name]),
                  trigger_compaction(Ref),
                  watch_progress(Lvl),
                  Pid ! done
          end),
    receive  
        done -> ok
    end.
        

watch_progress(Lvl=#lvl{name=Name, ref=Ref}) ->
    timer:sleep(60*1000),
    print_stats(Lvl),
    case num0(Ref) < 5 of
        true ->
            out(Lvl, "Compaction finished for ~p~n", [Name]),
            ok;
        false ->
            watch_progress(Lvl)
    end.

num0(Ref) ->
    {ok, Val} = eleveldb:status(Ref, <<"leveldb.num-files-at-level0">>),
    list_to_integer(binary_to_list(Val)).

print_stats(Lvl=#lvl{ref=Ref, name=Name}) ->
    {ok, Val} = eleveldb:status(Ref, <<"leveldb.stats">>),
    out(Lvl, "[~p]~n~s", [Name, Val]).

trigger_compaction(Ref) ->
    Key = <<"zzzzzzzzzzz$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$">>,
    eleveldb:put(Ref, Key, <<"val!">>, []),
    eleveldb:delete(Ref, Key, []),
    ok.
