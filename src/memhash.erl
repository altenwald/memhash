-module(memhash).
-author('manuel@altenwald.com').

-export([new/0,
         get/2,
         keys/1,
         rget/2,
         get_all/1,
         set_val/3,
         set_id/3,
         remove/2,
         destroy/1]).

-opaque memhash() :: {tid, ets:tid()}.

-export_type([memhash/0]).

-spec new() -> memhash().
new() ->
    {tid, ets:new(undefined, [ordered_set, public])}.

-spec get(memhash(), Key :: term()) ->
      undefined |
      {value,  memhash_data:table_id(), term()} |
      {error, enomemhash}.
get({tid, Mem}, Key) ->
    try ets:lookup(Mem, Key) of
        [] -> undefined;
        [{Key, ID}] -> {value, ID, memhash_data:get(ID)}
    catch error:badarg ->
        {error, enomemhash}
    end.

-spec keys(memhash()) -> [{term(), term()}] | {error, enomemhash}.
keys({tid, Mem}) ->
    try
        Key = ets:first(Mem),
        keys({tid, Mem}, Key, [])
    catch error:badarg ->
        {error, enomemhash}
    end.

-spec keys(memhash(), term(), [{term(), term()}]) -> [{term(), term()}].
keys(_Mem, '$end_of_table', Acc) ->
    lists:reverse(Acc);
keys({tid, Mem}, CurrentKey, Acc) ->
    NextKey = ets:next(Mem, CurrentKey),
    keys({tid, Mem}, NextKey, [CurrentKey|Acc]).

-spec rget(memhash(), Key :: term()) -> term().
rget(Mem, Key) ->
    case get(Mem, Key) of
        {value, _ID, {tid, _} = Result} ->
            get_all(Result);
        {value, _ID, Result} ->
            Result;
        {error, Error} ->
            throw({error, Error});
        undefined ->
            undefined
    end.

-spec get_all(memhash()) -> [{term(), term()}].
get_all(Mem) ->
    try 
        get_all(Mem, [])
    catch error:badarg ->
        throw({error, enomemhash})
    end.

-spec get_all(memhash(), [memhash()]) -> [{term(), term()}].
%% @private
get_all({tid, Mem}, Mems) ->
    case lists:member(Mem, Mems) of
        true -> throw({error, eloop});
        false -> ok
    end,
    ets:foldr(fun({Key, ID}, Acc) ->
        case memhash_data:get(ID) of
            {tid, _} = Result ->
                [{Key, lists:reverse(get_all(Result, [Mem|Mems]))}|Acc];
            Result ->
                [{Key, Result}|Acc]
        end
    end, [], Mem).

-spec set_val(memhash(), Key :: term(), Value :: term()) -> ok.
%% @doc set a value inside of the memhash with that value. It's added
%%      to the datatable if the key doesn't exist or replace the value
%%      in the table otherwise.
set_val({tid, Mem}, Key, Value) ->
    case ets:lookup(Mem, Key) of
        [] ->
            ID = memhash_data:add(Value),
            ets:insert(Mem, {Key, ID});
        [{Key, ID}] ->
            case memhash_data:get(ID) of
                {tid, _} = Result ->
                    destroy(Result);
                _Result ->
                    ok
            end,
            memhash_data:set(ID, Value)
    end,
    ok.

-spec set_id(memhash(), Key :: term(), memhash_data:table_id()) -> ok.
%% @doc set a reference inside of the memhash. The reference must be an
%%      ID from other part of the memhash (same memhash).
set_id({tid, Mem}, Key, ID) ->
    memhash_data:incr(ID),
    case ets:lookup(Mem, Key) of
        [] ->
            ok;
        [{Key, ID2}] ->
            memhash_data:decr(ID2)
    end,
    ets:insert(Mem, {Key, ID}),
    ok.

-spec remove(memhash(), Key :: term()) -> ok.
remove({tid, Mem}, Key) ->
    case ets:lookup(Mem, Key) of
        [] ->
            ok;
        [{_Key, ID}] ->
            memhash_data:decr(ID),
            ets:delete(Mem, Key),
            ok
    end.

-spec destroy(memhash()) -> ok.
destroy({tid, _} = Mem) ->
    try
        destroy(Mem, [])
    catch error:badarg ->
        ok
    end.

-spec destroy(memhash:memhash(), [memhash:memhash()]) -> ok.
%% @private
destroy({tid, Mem}, Mems) ->
    case lists:member(Mem, Mems) of
        true ->
            ok;
        false ->
            ets:foldl(fun({_Key, ID}, _) ->
                Links = memhash_data:get_links(ID),
                Data = memhash_data:get(ID),
                case {Data, Links} of
                    {{tid, _} = Result, 1} ->
                        destroy(Result, [Mem|Mems]);
                    _Result ->
                        ok
                end,
                memhash_data:decr(ID)
            end, undefined, Mem),
            ets:delete(Mem),
            ok
    end.
