-module (state_encoding).
-include("lasp_pb.hrl").

-ifdef(TEST).
-import(lasp_pb_codec, [encode/1, decode/1, encode_decode/2, mult_encode_decode/2]).
-include_lib("eunit/include/eunit.hrl").

counter_val_test() ->
    Op = #valcounter{val=1234567890987654321},
    encode_decode(Op, valcounter).

set_val_test() ->
    Op = #valset{elems=["A", "bbb", "CCCcCCC"]},
    encode_decode(Op, valset).

reg_val_test() ->
    Op = #valreg{val = "Lasp rules"},
    encode_decode(Op, valreg).

map_val_test() ->
    Op =
        #valmap{
          entries = [
            #mapentry{key = "map_key",
                      ktype = gcounter,
                      val = #mapfield{
                        v = {c, #valcounter{val=1234567890987654321}}
                      }}
          ]},
    encode_decode(Op, valmap).

nested_map_val_test() ->
    Op = #valmap{
      entries = [
        #mapentry{key = "obj_key1", ktype = gcounter, val = #mapfield{
          v = {c, #valcounter{val = 1234567890987654321}}
        }},
        #mapentry{key = "obj_key2", ktype = orset, val = #mapfield{
          v = {s, #valset{elems = ["A", "B", "C"]}}
        }},
        #mapentry{key = "obj_key3", ktype = gmap, val = #mapfield{
          v = {m, #valmap{
                entries = [
                    #mapentry{key = "nested_obj_key1", ktype = gcounter,
                    val = #mapfield{v = {c, #valcounter{val = 123}}}},
                    #mapentry{key = "nested_obj_key2", ktype = orset,
                    val = #mapfield{v = {s, #valset{elems = ["ab", "cd"]}}}},
                    #mapentry{key = "nested_obj_key1", ktype = lwwregister,
                    val = #mapfield{v = {r, #valreg{val = "123"}}}}
                    ]}
                  }
        }}
      ]},
    encode_decode(Op, valmap).

reqresp_error_test() ->
    Op = #reqresp{v = {error, "Something went wrong!"}},
    encode_decode(Op, reqresp).

reqresp_counter_test() ->
    Op = #reqresp{v = {ctr, #valcounter{val = 42}}},
    encode_decode(Op, reqresp).

reqresp_set_test() ->
    Op = #reqresp{v = {set, #valset{elems = ["aAa", "BbB", "CCCcccCCC"]}}},
    encode_decode(Op, reqresp).

reqresp_map_test() ->
    Op = #reqresp{v = {map, #valmap{
      entries = [
        #mapentry{key = "map_key",
                  ktype = gcounter,
                  val = #mapfield{
                    v = {c, #valcounter{val=1234567890987654321}}
                  }},
        #mapentry{key = "another_map_key",
                  ktype = orset,
                  val = #mapfield{
                    v = {s, #valset{elems = ["A", "BbB", "ZzZzZzZzZzZzZz..."]}}
                  }},
        #mapentry{key = "yet_another_map_key",
                  ktype = lwwreg,
                  val = #mapfield{
                    v = {r, #valreg{val = "Some register value"}}
                  }},
        #mapentry{key = "is_this_even_possible",
                  ktype = lwwreg,
                  val = #mapfield{
                    v = {m, #valmap{entries = [
                                #mapentry{key = "yes_it_is",
                                          ktype = lwwreg,
                                          val = #mapfield{
                                            v = {r, #valreg{val = "Some nested register value"}}
                                          }}
                    ]}}
                  }}
      ]}}},
    encode_decode(Op, reqresp).

reqresp_reg_test() ->
    Op = #reqresp{v = {reg, #valreg{val = "Lasp Lasp Lasp Lasp Lasp Lasp Lasp Lasp Lasp Lasp"}}},
    encode_decode(Op, reqresp).

reqresp_success_test() ->
    Ops = [
      #reqresp{v = {success, true}},
      #reqresp{v = {success, false}}
    ],
    mult_encode_decode(Ops, reqresp).
-endif.
