-module(couch_dbupdates).

-export([handle_dbupdates/2]).


handle_dbupdates(Fun, Acc) ->
    NotifierPid = db_update_notifier(),
    try
        loop(Fun, Acc)
    after
        catch(couch_db_update_notifier:stop(NotifierPid))
    end.


loop(Fun, Acc) ->
    receive
        {db_updated, Event} ->
            case Fun(Event, Acc) of
                {ok, Acc1} ->
                    loop(Fun, Acc1);
                stop ->
                    Fun(stop, Acc)

            end
    end.

db_update_notifier() ->
    Self = self(),
    {ok, Notifier} = couch_db_update_notifier:start_link(fun(Event) ->
        Self ! {db_updated, Event}
    end),
    Notifier.
