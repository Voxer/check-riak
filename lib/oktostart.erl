-module(oktostart).

-compile(export_all).

 
main([]) ->
    usage();
main(Args) when is_list(Args), length(Args) == 1 ->
    [Dir] = Args,
    ok_p(scan_dir(Dir));
main([Dir, "list"]) ->
    List = scan_dir(Dir),
    io:format("~p\n", [List]),
    ok_p(List);
main(_) ->
    usage().


scan_dir(TopDir) ->
    case file:list_dir(TopDir) of
        {ok, Dirs} ->
            [begin
                 case eleveldb:open(TopDir++Dir, []) of 
                     {ok, Ref} ->
                         {ok, Val0} = eleveldb:status(Ref, <<"leveldb.num-files-at-level0">>),
                         eleveldb:close(Ref),
                         Val = list_to_integer(binary_to_list(Val0)),
                         {Dir,Val};
                     {error, {db_open, Reason}} -> 
                         case Reason of 
                             "IO error:"++_ ->
                                 io:format("Couldn't open a partition:\n"
                                           "~s\n"
                                           "riak may still be running~n",
                                           [Reason]),
                                 halt();
                             "Invalid argument:"++_ ->
                                 {invalid, 0};
                             _ ->
                                 io:format("Unknown error: ~s\n", [Reason]),
                                 halt()
                         end;
                     {error, Reason} -> 
                         io:format("~p~n", [Reason]),
                         halt();
                     _ ->
                         {invalid, 0}
                 end
             end || Dir <- Dirs];
        Else ->
            Else
    end.

ok_p(DirList) ->
    List = lists:filter(fun({_Dir, Val}) ->
                                Val > 12 end, 
                        DirList),
    case List of
        [] ->
            io:format("All Partitions compacted, OK to start.\n");
        SubList ->
            io:format("Some partitons still partially"
                      " uncompacted, procede with caution.\n"
                      "~p\n", [SubList])
    end.

usage() ->
    io:format("escript oktostart.erl </path/to/leveldb/> [list]").
