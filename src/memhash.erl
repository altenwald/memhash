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

-opaque memhash() :: reference().

-export_type([memhash/0]).

-spec new() -> memhash().
new() ->
    Ref = make_ref(),
    erlang:put({index, Ref}, []),
    Ref.

-type reason() :: atom().

-spec get(memhash(), Key :: term()) ->
      undefined |
      {value,  memhash_data:table_id(), term()} |
      {error, reason()}.
get(Ref, Key) ->
    case erlang:get({index, Ref}) of
        undefined ->
            {error, enomemhash};
        Mem ->
            case lists:keyfind(Key, 2, Mem) of
                false ->
                    undefined;
                {value, Key, ID} ->
                    {value, ID, memhash_data:get(ID)}
            end
    end.

-spec keys(memhash()) -> [term()].
keys(Ref) ->
    case erlang:get({index, Ref}) of
        undefined -> {error, enomemhash};
        List -> [ element(2, M) || M <- List ]
    end.

-spec rget(memhash(), Key :: term()) -> term().
rget(Ref, Key) ->
    case get(Ref, Key) of
        {value, _ID, Result} when is_reference(Result) ->
            lists:reverse(get_all(Result));
        {value, _ID, Result} ->
            Result;
        {error, Error} ->
            throw({error, Error});
        undefined ->
            undefined
    end.

-spec get_all(memhash()) -> [term()].
get_all(Ref) ->
    get_all(Ref, []).

-spec get_all(memhash(), [memhash()]) -> [term()].
%% @private
get_all(Ref, Refs) ->
    case lists:member(Ref, Refs) of
        true -> throw({error, eloop});
        false -> ok
    end,
    case erlang:get({index, Ref}) of
        undefined ->
            throw({error, enomemhash});
        Indexes ->
            lists:map(fun({value, SubKey, ID}) ->
                case memhash_data:get(ID) of
                    Result when is_reference(Result) ->
                        {SubKey, lists:reverse(get_all(Result, [Ref|Refs]))};
                    Result ->
                        {SubKey, Result}
                end
            end, Indexes)
    end.

-spec set_val(memhash(), Key :: term(), Value :: term()) -> ok.
%% @doc set a value inside of the memhash with that value. It's added
%%      to the datatable if the key doesn't exist or replace the value
%%      in the table otherwise.
set_val(Ref, Key, Value) ->
    Indexes = erlang:get({index, Ref}),
    case lists:keyfind(Key, 2, Indexes) of
        false ->
            ID = memhash_data:add(Value),
            NewIndexes = [{value, Key, ID}|Indexes],
            erlang:put({index, Ref}, NewIndexes);
        {value, Key, ID} ->
            case memhash_data:get(ID) of
                Result when is_reference(Result) ->
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
set_id(Ref, Key, ID) ->
    Indexes = erlang:get({index, Ref}),
    memhash_data:incr(ID),
    NewIndexes = case lists:keyfind(Key, 2, Indexes) of
        false ->
            Indexes;
        {value, Key, ID2} ->
            memhash_data:decr(ID2),
            lists:keydelete(Key, 2, Indexes)
    end,
    erlang:put({index, Ref}, [{value, Key, ID}|NewIndexes]),
    ok.

-spec remove(memhash(), Key :: term()) -> ok.
remove(Ref, Key) ->
    Indexes = erlang:get({index, Ref}),
    case lists:keyfind(Key, 2, Indexes) of
        false ->
            ok;
        {value, _Key, ID} ->
            memhash_data:decr(ID),
            erlang:put({index, Ref}, lists:keydelete(Key, 2, Indexes)),
            ok
    end.

-spec destroy(memhash()) -> ok.
destroy(Ref) when is_reference(Ref) ->
    destroy(Ref, []).

-spec destroy(memhash:memhash(), [memhash:memhash()]) -> ok.
%% @private
destroy(Ref, Refs) ->
    case lists:member(Ref, Refs) of
        true ->
            ok;
        false ->
            case erlang:get({index, Ref}) of
                Indexes when is_list(Indexes) ->
                    lists:foreach(fun({value, _SubKey, ID}) ->
                        case memhash_data:get_with_links(ID) of
                            {Result, 1} when is_reference(Result) ->
                                destroy(Result, [Ref|Refs]);
                            _Result ->
                                ok
                        end,
                        memhash_data:decr(ID)
                    end, Indexes),
                    erlang:erase({index, Ref}),
                    ok;
                undefined ->
                    ok
            end
    end.
