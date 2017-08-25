-module (operation_encoding).
-include("lasp_pb.hrl").

-ifdef(TEST).
-import(lasp_pb_codec, [encode/1, decode/1, encode_decode/2, mult_encode_decode/2]).
-include_lib("eunit/include/eunit.hrl").

entry_basic_test() ->
    Ops = [
        #entry{u = {int, 365}},
        #entry{u = {str, "something"}},
        #entry{u = {atm, something}}
    ],
    mult_encode_decode(Ops,entry).

pair_basic_ops_test() ->
    Ops = [
        #pair{a = build_entry_rec(fst), b = build_entry_rec(increment)},
        #pair{a = build_entry_rec(snd), b = build_entry_rec(365)}
    ],
    mult_encode_decode(Ops,pair).

pair_nested_ops_test() ->
    Ops = [
        #pair{a = build_entry_rec(snd),
              b = build_entry_rec(#pair{a = build_entry_rec(fst),
                                        b = build_entry_rec(increment)})},
        #pair{a = build_entry_rec(snd),
              b = build_entry_rec(#pair{a = build_entry_rec(fst),
                                        b = build_entry_rec(#pair{a = build_entry_rec(add),
                                                                  b=build_entry_rec(17)})})}
    ],
    mult_encode_decode(Ops,pair).

triple_basic_op_test() ->
    Op = #triple{ a = build_entry_rec(set),
                  b = build_entry_rec(1010),
                  c = build_entry_rec("value")},
    encode_decode(Op, triple).

triple_nested_ops_test() ->
    Ops = [
        #triple{a = build_entry_rec(apply),
                b = build_entry_rec("key"),
                c = build_entry_rec(#triple{a = build_entry_rec(set),
                                            b = build_entry_rec(1010),
                                            c = build_entry_rec("value")})},
        #triple{a = build_entry_rec(apply),
                b = build_entry_rec("key"),
                c = build_entry_rec(#triple{a = build_entry_rec(set),
                                            b = build_entry_rec(add),
                                            c = build_entry_rec(#triple{a = build_entry_rec(a),
                                                                        b = build_entry_rec(b),
                                                                        c = build_entry_rec(c)})})}
    ],
    mult_encode_decode(Ops,triple).

map_ops_test() ->
    Ops = [
        build_entry_rec(#triple{a = build_entry_rec(apply),
                                b = build_entry_rec("hey"),
                                c = build_entry_rec(2)}),
        build_entry_rec(#triple{a = build_entry_rec(apply),
                                b = build_entry_rec("hi"),
                                c = build_entry_rec("random_string")}),
        build_entry_rec(#triple{a = build_entry_rec(apply),
                                b = build_entry_rec("hello"),
                                c = build_entry_rec(can_it_handle_atoms)}),
        build_entry_rec(#triple{a = build_entry_rec(apply),
                                b = build_entry_rec("top_level"),
                                c = build_entry_rec(#triple{a = build_entry_rec(apply),
                                                            b = build_entry_rec("nested"),
                                                            c = build_entry_rec(#pair{a = build_entry_rec(add),
                                                                                      b = build_entry_rec(3)})})})
    ],
    mult_encode_decode(Ops,entry).

opget_test() ->
    Op = #opget{key = "my_lasp_kv_key", type = gcounter},
    encode_decode(Op, opget).

opupdate_test() ->
    Op = #opupdate{
            k=#opget{key = "my_lasp_kv_key", type = gcounter},
            e=[build_entry_rec(increment)],
            actor=node()
    },
    encode_decode(Op, opupdate).

req_opget_test() ->
    Op = #req{u = {get, #opget{key = "my_lasp_kv_key", type = gcounter}}},
    encode_decode(Op, req).

req_opupdate_inc_counter_test() ->
    Op = #req{u = {put,
                #opupdate{
                    k=#opget{key = "my_lasp_kv_key", type = gcounter},
                    e=[build_entry_rec(increment)],
                    actor=node()}}},
    encode_decode(Op, req).

build_entry_rec(Val) when is_atom(Val) ->
    #entry{u = {atm, Val}};
build_entry_rec(Val) when is_integer(Val) ->
    #entry{u = {int, Val}};
build_entry_rec(Val) when is_list(Val) ->
    #entry{u = {str, Val}};
build_entry_rec(#pair{a = _, b = _} = Pair) ->
    #entry{u = {ii, Pair}};
build_entry_rec(#triple{a = _, b = _, c = _} = Triple) ->
    #entry{u = {iii, Triple}}.
-endif.
