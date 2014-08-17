-module(couch_dbupdates_httpd).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").

-record(st, {
    resp,
    feed,
    heartbeat,
    timeout
}).


handle_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Qs = couch_httpd:qs(Req),
    Feed = proplists:get_value("feed", Qs, "longpoll"),
    Timeout = list_to_integer(proplists:get_value("timeout", Qs, "60000")),
    Heartbeat0 = proplists:get_value("heartbeat", Qs),
    Heartbeat = case {Feed, Heartbeat0} of
        {"longpoll", _} -> false;
        {_, "false"} -> false;
        _ -> true
    end,

    {ok, Resp} = case Feed of
        "eventsource" ->
            Headers = [
                {"Content-Type", "text/event-stream"},
                {"Cache-Control", "no-cache"}
            ],
            couch_httpd:start_json_response(Req, 200, Headers);
        _ ->
            couch_httpd:start_json_response(Req, 200)
    end,

    St1 = #st{
        resp = Resp,
        feed = Feed,
        heartbeat = Heartbeat,
        timeout = Timeout
    },
    {ok, St2} = run(St1, Timeout),
    {ok, St2#st.resp};

handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


run(St, Timeout) ->
    ok = couch_event:register_all(self()),
    try
        loop(St, Timeout)
    after
        ok = couch_event:unregister(self()),
        drain_events()
    end.


loop(Timeout, St) ->
    Event = receive
        {'$couch_event', _DbName, {index_commit, _}} ->
            loop(Timeout, St);
        {'$couch_event', DbName, Ev} ->
            {DbName, Ev}
    after Timeout ->
        timeout
    end,
    case handle_update(Event, St) of
        {ok, NewSt} ->
            loop(Timeout, NewSt);
        {stop, NewSt} ->
            {ok, NewSt}
    end.


drain_events() ->
    receive
        {'$couch_event', _, _} ->
            drain_events()
    after 0 ->
        ok
    end.


handle_update(timeout, #st{heartbeat=true}=St) ->
    {ok, Resp1} = couch_httpd:send_chunk(St#st.resp, "\n"),
    {ok, St#st{resp=Resp1}};
handle_update(timeout, #st{heartbeat=false}=St) ->
    {ok, Resp1} = couch_httpd:end_json_response(St#st.resp),
    {stop, St#st{resp=Resp1}};
handle_update(Event, #st{feed="eventsource"}=St) ->
    Chunk = ["data: ", ?JSON_ENCODE(event_obj(Event)), "\n\n"],
    {ok, Resp1} = couch_httpd:send_chunk(St#st.resp, Chunk),
    {ok, St#st{resp=Resp1}};
handle_update(Event, #st{feed="continuous"}=St) ->
    Chunk = [?JSON_ENCODE(event_obj(Event)), "\n"],
    {ok, Resp1} = couch_httpd:send_chunk(St#st.resp, Chunk),
    {ok, St#st{resp=Resp1}};
handle_update(Event, #st{feed="longpoll"}=St) ->
    {Props} = event_obj(Event),
    JsonObj = {[{<<"ok">>, true} | Props]},
    {ok, Resp1} = couch_httpd:send_chunk(St#st.resp, ?JSON_ENCODE(JsonObj)),
    {ok, Resp2} = couch_httpd:end_json_response(Resp1),
    {stop, St#st{resp=Resp2}}.


event_obj({DbName, Event}) when is_atom(Event) ->
    {[
        {<<"type">>, couch_util:to_binary(Event)},
        {<<"db_name">>, couch_util:to_binary(DbName)}
    ]};
event_obj({DbName, {ddoc_updated, DDocId}}) ->
    {[
        {<<"type">>, <<"ddoc_updated">>},
        {<<"db_name">>, couch_util:to_binary(DbName)},
        {<<"ddoc_id">>, couch_util:to_binary(DDocId)}
    ]}.


