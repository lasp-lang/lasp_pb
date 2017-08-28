-module (state_encoding).
-include("lasp_pb.hrl").

-ifdef(TEST).
-import(lasp_pb_codec, [encode/1, decode/1, encode_decode/2, mult_encode_decode/2]).
-include_lib("eunit/include/eunit.hrl").

counter_positive_val_test() ->
    Val = {response, gcounter, 1234567890987654321},
    {A, _, B} = Val,
    ?assertEqual({A, B}, encode_decode(Val, reqresp)).

counter_negative_val_test() ->
    Val = {response, pncounter, -1234567890987654321},
    {A, _, B} = Val,
    ?assertEqual({A, B}, encode_decode(Val, reqresp)).

set_atom_val_test() ->
    Elems = [one, two, three],
    Val = {response, orset, Elems},
    {A, _, B} = Val,
    Expected = {A, lists:map(fun(E) -> atom_to_list(E) end, B)},
    ?assertEqual(Expected, encode_decode(Val, reqresp)).

set_string_val_test() ->
    Val = {response, orset, ["A", "bbb", "CCCcCCC"]},
    {A, _, B} = Val,
    ?assertEqual({A, B}, encode_decode(Val, reqresp)).

reg_atom_val_test() ->
    Val = {response, lwwregister, some_value},
    {A, _, B} = Val,
    ?assertEqual({A, atom_to_list(B)}, encode_decode(Val, reqresp)).

reg_string_val_test() ->
    Val = {response, lwwregister, "some_value"},
    {A, _, B} = Val,
    ?assertEqual({A, B}, encode_decode(Val, reqresp)).

map_basic_val_test() ->
    Val = {response, mvmap, [
        {"ctr_map_key", gcounter, 1234567890987654321},
        {"ctr_map_key", pncounter, -1234567890987654321},
        {"set_map_key", awset, ["lasp", "is", "cool"]},
        {"reg_map_key", lwwregister, "string value"}
    ]},
    {A, _, B} = Val,
    ?assertEqual({A, B}, encode_decode(Val, reqresp)).

map_nested_val_test() ->
    Val = {response, gmap, [
        {"normal_key", lwwregister, "string value"},
        {"another_normal_key", awset, ["lasp", "is", "cool"]},
        {"map_key", gmap, [
                        {"nested_normal_key", lwwregister, "string value"},
                        {"another_nested_normal_key", awset, ["lasp", "is", "cool"]},
                        {"we need to go deeper", awmap, [
                                                      {"ctr_map_key", pncounter, -1234567890987654321}
                                                      ]}
                    ]}
    ]},
    {A, _, B} = Val,
    ?assertEqual({A, B}, encode_decode(Val, reqresp)).

reqresp_error_test() ->
    Val = {response, error, "something went horribly wrong!"},
    ?assertEqual(Val, encode_decode(Val, reqresp)).

reqresp_success_test() ->
    Ops = [
      {response, success, true},
      {response, success, false}
    ],
    ?assertEqual(Ops, mult_encode_decode(Ops, reqresp)).
-endif.
