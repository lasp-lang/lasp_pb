-module(lasp_pb_codec).

-include ("lasp_pb.hrl").

-export([ encode/1, decode/1 ]).

%% Encoding
encode(#reqresp{v = {Type, Record}}) ->
    #reqresp{v = {Type, encode(Record)}};

encode(#valmap{entries = ListEntries}) ->
    #valmap{entries = lists:map(fun encode/1,ListEntries)};

encode(#mapentry{key = K, ktype = KT, val = Val}) ->
    #mapentry{key = K, ktype = atom_to_list(KT), val = encode(Val)};

encode(#mapfield{v = {Type, Record}}) ->
    #mapfield{v = {Type, encode(Record)}};

encode(#req{u = {put, OpUpdate}}) ->
    #req{u = {put, encode(OpUpdate)}};

encode(#req{u = {get, OpGet}}) ->
    #req{u = {get, encode(OpGet)}};

encode(#opupdate{k = GetOp, e = [], actor = Actor}) ->
    #opupdate{k = encode(GetOp),
              e = [],
              actor = encode(Actor)};

encode(#opupdate{k = GetOp, e = ListUpdates, actor = Actor}) ->
    #opupdate{k = encode(GetOp),
              e = [encode(Entry) || Entry <- ListUpdates],
              actor = encode(Actor)};

encode(#opget{key = K, type = T}) ->
    #opget{key = K,
           type = atom_to_list(T)};

encode(#entry{u = {int, Val}} = Entry) when is_integer(Val) ->
    Entry;

encode(#entry{u = {str, Val}} = Entry) when is_list(Val) ->
    Entry;

encode(#entry{u = {atm, Val}}) when is_atom(Val) ->
    #entry{u = {atm, atom_to_list(Val)}};

encode(#entry{u = {ii, Pair}}) ->
    #entry{u = {ii, encode(Pair)}};

encode(#entry{u = {iii, Triple}}) ->
    #entry{u = {iii, encode(Triple)}};

encode(#pair{a = A, b = B}) ->
    #pair{a = encode(A), b = encode(B)};

encode(#triple{a = A, b = B, c = C}) ->
    #triple{a = encode(A), b = encode(B), c = encode(C)};

encode(X) when is_atom(X) ->
    atom_to_list(X);

%% Catch all clause (valid for integers, lists, binaries, etc.)
encode(X) ->
    X.

%TODO NOTE *IMPORTANT* delete this
% encode({A, B}) ->
%     #entry{u = {ii, #pair{a = encode(A),
%                           b = encode(B)}}};
% encode({A, B, C}) ->
%     #entry{u = {iii, #triple{a = encode(A),
%                             b = encode(B),
%                             c = encode(C)}}};
%
% encode(A) when is_integer(A) ->
%     #entry{u = {int, A}};
% encode(A) when is_atom(A) ->
%     #entry{u = {atm, atom_to_list(A)}};
% % else assume it's string
% encode(A) ->
%     #entry{u = {str, A}}.
%
% encode_atom(A) when is_atom(A) ->
%     atom_to_list(A).
%TODO NOTE END *IMPORTANT* delete this

%% Decode operations.
decode(#reqresp{v = {error, Error}}) ->
    #reqresp{v = {error, binary_to_list(Error)}};

decode(#reqresp{v = {Type, Record}}) ->
    #reqresp{v = {Type, decode(Record)}};

decode(#valmap{entries = ListEntries}) ->
    #valmap{entries = lists:map(fun decode/1,ListEntries)};

decode(#mapentry{key = K, ktype = KT, val = V}) ->
    #mapentry{key = binary_to_list(K),
              ktype = binary_to_atom(KT, utf8),
              val = decode(V)};

decode(#mapfield{v = {r, Reg}}) ->
    #mapfield{v = {r, decode(Reg)}};

decode(#mapfield{v = {m, Map}}) ->
    #mapfield{v = {m, decode(Map)}};

decode(#mapfield{v = {s, Set}}) ->
    #mapfield{v = {s, decode(Set)}};

decode(#mapfield{v = {c, Counter}}) ->
    #mapfield{v = {c, decode(Counter)}};

decode(#valreg{val=Val}) ->
    #valreg{val=binary_to_list(Val)};

decode(#valset{elems=ListElems}) ->
    #valset{elems=decode_string_list(ListElems)};

decode(#valcounter{val=Num}) when is_integer(Num) ->
    #valcounter{val = Num};

decode(#req{u = {get, OpGet}}) ->
    #req{u = {get, decode(OpGet)}};

decode(#req{u = {put, OpUpdate}}) ->
    #req{u = {put, decode(OpUpdate)}};

decode(#opget{key = K, type = T}) ->
    #opget{key = binary_to_list(K),
           type = decode_atom(T)};

decode(#opupdate{k = GetOp, e = [], actor = Actor}) ->
   #opupdate{k = decode(GetOp),
             e = [],
             actor = decode_atom(Actor)};

decode(#opupdate{k = GetOp, e = ListUpdates, actor = Actor}) ->
    #opupdate{k = decode(GetOp),
              e = [decode(Entry) || Entry <- ListUpdates],
              actor = decode_atom(Actor)};

decode(#pair{a = A, b = B}) ->
    #pair{a = decode(A), b = decode(B)};

decode(#triple{a = A, b = B, c = C}) ->
    #triple{a = decode(A), b = decode(B), c = decode(C)};

decode(#entry{u = {int, _Val}} = Entry) ->
    Entry;

decode(#entry{u = {atm, A}}) ->
    #entry{u = {atm, binary_to_atom(A, utf8)}};

decode(#entry{u = {str, A}}) ->
    #entry{u = {str, binary_to_list(A)}};

decode(#entry{u = {ii, #pair{a = A, b = B}}}) ->
    #entry{u = {ii, #pair{a = decode(A),
                          b = decode(B)}}};

decode(#entry{u = {iii, #triple{a = A, b = B, c = C}}}) ->
    #entry{u = {iii, #triple{ a = decode(A),
                              b = decode(B),
                              c = decode(C)}}}.

decode_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, utf8).

decode_string_list(List) ->
    decode_string_list(List, []).

decode_string_list([], Res) ->
    lists:reverse(Res);
decode_string_list([H|T], Res) ->
    H1 = binary_to_list(H),
    decode_string_list(T, [H1|Res]).
