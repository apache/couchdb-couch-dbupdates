-module(couch_dbupdates_httpd).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").

-record(state, {resp, feed}).

handle_req(#httpd{method='GET'}=Req) ->
    {ok, Resp} = couch_httpd:start_json_response(Req, 200, []),

    Feed = proplists:get_value("feed", couch_httpd:qs(Req), "longpoll"),

    State = #state{resp=Resp, feed=Feed},
    try
        {ok, Pid} = couch_dbupdates:handle_dbupdates(fun handle_update/2,
                                                  State)
    after
        couch_httpd:end_json_response(Resp)
    end.

handle_req(Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


handle_update(Event, #state{resp=Resp, feed="continuous"}=State) ->
    EventObj = event_obj(Event),
    {ok, Resp1} = couch_httpd:send_chunk(Resp, [?JSON_ENCODE(EventObj) |
                            "\n"]);
    {ok, State#state{resp=Resp1}};

handle_update(Event, #state{resp=Resp, feed="longpoll"}=State) ->
    {EventProps} = even_obj(Event),
    JsonObj = {[{<<"ok">>, true} | EventProps]},
    {ok, Resp1} = couch_httpd:send_chunk(Resp, ?JSON_ENCODE(JsonObj)),
    {ok, State#state{resp=Resp1}}.

event_obj({Type, DbName}) ->
    {[{<<"type">>, couch_util:to_binary(Type)},
      {<<"db_name">>, couch_util:to_binary(DbName)}]}.
