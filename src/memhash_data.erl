-module(memhash_data).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, {no_auto_import, [get/1]}]).

-export([start_link/0,
         stop/0,
         get/1,
         get_links/1,
         add/1,
         set/2,
         incr/1,
         decr/1]).

-opaque table_id() :: pos_integer().

-export_type([table_id/0]).

start_link() ->
    Tid = ets:new(?MODULE, [set, public]),
    erlang:put(?MODULE, Tid),
    ok.

stop() ->
    Tid = erlang:get(?MODULE),
    erlang:erase(?MODULE),
    ets:delete(Tid),
    ok.

-spec seq_id() -> table_id().
seq_id() ->
    Tid = erlang:get(?MODULE),
    update_counter(Tid, idx, 1, {idx, 0}).

-spec get(table_id()) -> term().
get(ID) ->
    Tid = erlang:get(?MODULE),
    case ets:lookup(Tid, {data, ID}) of
        [] -> undefined;
        [{{data, ID}, Data}] -> Data
    end.

-spec get_links(table_id()) -> pos_integer().
get_links(ID) ->
    Tid = erlang:get(?MODULE),
    case ets:lookup(Tid, {links, ID}) of
        [] -> undefined;
        [{{links, ID}, Data}] -> Data
    end.

-spec add(Data :: term()) -> table_id().
add(Data) ->
    ID = seq_id(),
    Tid = erlang:get(?MODULE),
    ets:insert(Tid, [{{data, ID}, Data}, {{links, ID}, 1}]),
    ID.

-spec set(table_id(), Data :: term()) -> ok.
set(ID, Data) ->
    Tid = erlang:get(?MODULE),
    ets:insert(Tid, {{data, ID}, Data}),
    ok.

-spec incr(table_id()) -> ok.
incr(ID) ->
    Tid = erlang:get(?MODULE),
    update_counter(Tid, {links, ID}, 1, {idx, 0}),
    ok.

-spec decr(table_id()) -> ok.
decr(ID) ->
    Tid = erlang:get(?MODULE),
    case update_counter(Tid, {links, ID}, -1, {idx, 0}) of
        N when N =< 0 ->
            ets:delete(Tid, {links, ID}),
            ets:delete(Tid, {data, ID}),
            ok;
        _ ->
            ok
    end.

-ifdef(NO_ETS_UPDATE_DEFAULT).
update_counter(Tid, Key, Incr, Default) ->
    try
        ets:update_counter(Tid, Key, Incr)
    catch error:badarg ->
        ets:insert(Tid, Default)
    end.
-else.
update_counter(Tid, Key, Incr, Default) ->
    ets:update_counter(Tid, Key, Incr, Default).
-endif.
