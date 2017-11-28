-module(memhash_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(undefined, memhash:get(M, <<"name">>)),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    ?assertMatch({value, _Number, <<"Manuel">>}, memhash:get(M, <<"name">>)),
    ?assertMatch(ok, memhash:remove(M, <<"name">>)),
    ?assertEqual(undefined, memhash:get(M, <<"name">>)),
    ?assertEqual(ok, memhash:destroy(M)),
    ?assertEqual({error, enomemhash}, memhash:keys(M)),
    ok = memhash:destroy(M),
    ok.

remove_not_found_element_test() ->
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertMatch(ok, memhash:remove(M, <<"name">>)),
    ok = memhash:destroy(M),
    ok.

simple_with_refs_test() ->
    M = memhash:new(),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    {value, ID, <<"Manuel">>} = memhash:get(M, <<"name">>),
    ?assertEqual(ok, memhash:set_id(M, <<"nombre">>, ID)),
    ?assertMatch({value, ID, <<"Manuel">>}, memhash:get(M, <<"name">>)),
    ?assertMatch({value, ID, <<"Manuel">>}, memhash:get(M, <<"nombre">>)),
    ok = memhash:remove(M, <<"name">>),
    ?assertMatch(undefined, memhash:get(M, <<"name">>)),
    ?assertMatch({value, ID, <<"Manuel">>}, memhash:get(M, <<"nombre">>)),
    ok = memhash:remove(M, <<"nombre">>),
    ?assertMatch(undefined, memhash:get(M, <<"name">>)),
    ?assertMatch(undefined, memhash:get(M, <<"nombre">>)),
    ok = memhash:destroy(M),
    ok.

overwriting_entries_test() ->
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    {value, ID, <<"Manuel">>} = memhash:get(M, <<"name">>),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Angel">>)),
    ?assertMatch({value, ID, <<"Angel">>}, memhash:get(M, <<"name">>)),
    ok = memhash:destroy(M),
    ok.

not_found_recursive_key_test() ->
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(undefined, memhash:rget(M, <<"array">>)),
    ok = memhash:destroy(M),
    ok.

single_value_for_recursive_key_test() ->
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    ?assertEqual(<<"Manuel">>, memhash:rget(M, <<"name">>)),
    ok = memhash:destroy(M),
    ok.

circular_error_test() ->
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    {value, ID, _Array} = memhash:get(Base, <<"array">>),
    ?assertEqual(ok, memhash:set_id(Array, <<"loop">>, ID)),
    ?assertException(throw, {error, eloop}, memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok.

circular_memhash_error_test() ->
    Base = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Base)),
    ?assertException(throw, {error, eloop}, memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok.

deep_test() ->
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    ?assertEqual([{0,<<"hi">>},{1,<<"bye">>},{2,<<"hi again">>}],
                 memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok.

submemhash_cleaned_test() ->
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ?assertEqual({error, enomemhash}, memhash:get(Array, 0)),
    ok.

overwriting_reference_test() ->
    Base = memhash:new(),
    Array1 = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array1, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array1, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array1, 2, <<"hi again">>)),
    Array2 = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array2, 0, <<"hola">>)),
    ?assertEqual(ok, memhash:set_val(Array2, 1, <<"adios">>)),
    ?assertEqual(ok, memhash:set_val(Array2, 2, <<"hola de nuevo">>)),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array1)),
    ?assertEqual([{0,<<"hi">>},{1,<<"bye">>},{2,<<"hi again">>}],
                 memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array2)),
    ?assertEqual({error,enomemhash}, memhash:keys(Array1)),
    ?assertEqual([{0,<<"hola">>},{1,<<"adios">>},{2,<<"hola de nuevo">>}],
                 memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ?assertEqual({error,enomemhash}, memhash:keys(Array2)),
    ok.

id_test() ->
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    {value, ID, _Value} = memhash:get(Array, 0),
    ?assertEqual(ok, memhash:set_id(Array, 3, ID)),
    ?assertEqual(ok, memhash:set_id(Array, 4, ID)),
    ?assertEqual({<<"hi">>, 3}, memhash_data:get_with_links(ID)),
    ?assertEqual(ok, memhash:remove(Array, 0)),
    ?assertEqual({<<"hi">>, 2}, memhash_data:get_with_links(ID)),
    ?assertEqual(ok, memhash:remove(Array, 4)),
    ?assertEqual({<<"hi">>, 1}, memhash_data:get_with_links(ID)),
    ?assertEqual([{1,<<"bye">>},{2,<<"hi again">>},{3,<<"hi">>}],
                 lists:reverse(memhash:get_all(Array))),
    ?assertEqual(ok, memhash:destroy(Array)),
    ok.

overwriting_id_test() ->
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    {value, ID0, _Value0} = memhash:get(Array, 0),
    {value, ID1, _Value1} = memhash:get(Array, 1),
    ?assertEqual(ok, memhash:set_id(Array, 3, ID0)),
    ?assertEqual(ok, memhash:set_id(Array, 3, ID1)),
    ?assertEqual([{0,<<"hi">>},{1,<<"bye">>},{2,<<"hi again">>},{3,<<"bye">>}],
                 lists:reverse(memhash:get_all(Array))),
    ?assertEqual({<<"hi">>, 1}, memhash_data:get_with_links(ID0)),
    ?assertEqual({<<"bye">>, 2}, memhash_data:get_with_links(ID1)),
    ?assertEqual(ok, memhash:destroy(Array)),
    ok.

doing_nasty_things_test() ->
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    ?assertEqual(ok, memhash:destroy(Array)),
    ?assertException(throw, {error, enomemhash}, memhash:rget(Base, <<"array">>)),
    ?assertException(throw, {error, enomemhash}, memhash:rget(Array, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok.
