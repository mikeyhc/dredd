parse_message("mrs", Conn, Chan, _Mesg, State) ->
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(get, {
                        "http://gitlab.tabelard/api/v3/projects/2/merge_requests?state=opened",
                        [{"PRIVATE-TOKEN", "ceg8KGaksQPE2RUQU3Q1"}]}, [], []),
    MRList = jiffy:decode(Body),
    SendMR = fun({Fields}) ->
        ID = proplists:get_value(<<"id">>, Fields),
        Title = proplists:get_value(<<"title">>, Fields),
        Msg0 = io_lib:format("MR~w - \"~s\" <http://gitlab.tabelard.wgtn.cat-it.co.nz/tab/tab/merge_requests/~w>", [ID, Title, ID]),
        Msg = case proplists:get_value(<<"assignee">>, Fields) of
            {AssignList} ->
                Assigned = proplists:get_value(<<"username">>, AssignList),
                io_lib:format("~s: ~s", [Assigned, Msg0]);
            null -> Msg0
        end,
        PrivMsg = dredd_irc:privmsg("dredd", Chan, Msg),
        send_message(Conn, PrivMsg)
    end,
    case MRList of
        [] ->
            PrivMsg = dredd_irc:privmsg("dredd", Chan,
                                        "No current merge requests"),
            send_message(Conn, PrivMsg);
        _ ->
            lists:map(SendMR, MRList)
    end,
    State;
