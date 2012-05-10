-module(couch_dbupdates).

-export([handle_dbupdates/2]).


handle_dbupdates(Fun, Acc) ->
    NotifierPid = db_update_notifier(),
    Pid = spawn(fun() -> loop(NotifierPid, Fun, Acc) end),
    {ok, Pid}.


loop(NotifierPid, Fun, Acc) ->
    receive
        {db_updated, Event} ->
            case Fun(Event, Acc) of
                {ok, Acc1} ->
                    loop(NotifierPid, Fun, Acc1);
                stop ->
                    couch_db_update_notifier:stop(NotifierPid)
            end
    end.

db_update_notifier() ->
    Self = self(),
    {ok, Notifier} = couch_db_update_notifier:start_link(fun(Event) ->
        Self ! {db_updated, Event}
    end),
    Notifier.
