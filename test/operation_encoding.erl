-module (operation_encoding).
-include("lasp_pb.hrl").

-ifdef(TEST).
-import(lasp_pb_codec, [encode/1, decode/1, encode_decode/2, mult_encode_decode/2]).
-include_lib("eunit/include/eunit.hrl").

wa_test() ->
    Op = {add_all, ["A", "b", "C"]},
    ?assertEqual(Op, encode_decode(Op,entry)).


simple_op_test() ->
    Op = {add, "A"},
    ?assertEqual(Op, encode_decode(Op,entry)).

entry_basic_test() ->
    Ops = [
        increment,
        {add, "A"},
        {add_all, ["A", "b", "C"]},
        {set, 1010, "value"},
        {apply, "key", gcounter, increment}
    ],
    ?assertEqual(Ops, mult_encode_decode(Ops, entry)).

pair_basic_ops_test() ->
    Ops = [{fst, increment}, {snd, 365}],
    ?assertEqual(Ops, mult_encode_decode(Ops, entry)).

pair_nested_ops_test() ->
    Ops = [{snd, {fst, increment}}, {snd, {fst, {add, "17"}}}],
    ?assertEqual(Ops, mult_encode_decode(Ops, entry)).

triple_basic_op_test() ->
    Op = {set, "key", "value"},
    ?assertEqual(Op, encode_decode(Op, entry)).

quad_basic_op_test() ->
    Op = {apply, "key", gcounter, increment},
    ?assertEqual(Op, encode_decode(Op, entry)).

triple_nested_ops_test() ->
    Ops = [
        {apply, "key", {set, "nested_key", "Value"}},
        {apply, "key2", {add, ["a", "B", "c"]}}
    ],
    ?assertEqual(Ops, mult_encode_decode(Ops, entry)).

map_ops_test() ->
    Ops = [
        {apply, "hey", gcounter, increment},
        {apply, "hi", lwwregister, {set, 12345, "value"}},
        {apply, "hello", awset, {add_all, ["a", "b", "c", "d"]}},
        {apply, "top_level", gmap, {apply, "nested", awset, {add, 3}}}
    ],
    ?assertEqual(Ops, mult_encode_decode(Ops, entry)).

req_opget_test() ->
    Op = {req, {get, {"key", gcounter}}},
    ?assertEqual(Op, encode_decode(Op,req)).

req_opupdate_test() ->
    Op = {req, {put, {{"key", gcounter}, increment}}},
    ?assertEqual(Op, encode_decode(Op,req)).
-endif.
