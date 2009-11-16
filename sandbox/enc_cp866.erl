-module(enc_cp866).

-export([decode/1, decode2/1]).


decode(String) ->
    decode(String, "").

decode(<<>>, Result) ->
    lists:reverse(Result);
decode(<<C,Tail/binary>>=Input, Result) ->
    case decode_char(C) of
        badarg ->
            {error, lists:reverse(Result), Input};
        D ->
            decode(Tail, [D | Result])
    end.


decode2(String) when is_binary(String) ->
    [decode_char2(C) || C <- binary_to_list(String)].


decode_char(C) when C >= 0, C =< 16#7f -> C;
decode_char(C) when C >= 16#80, C =< 16#af -> C + 16#390;
decode_char(16#B0) -> 16#2591;
decode_char(16#B1) -> 16#2592;
decode_char(16#B2) -> 16#2593;
decode_char(16#B3) -> 16#2502;
decode_char(16#B4) -> 16#2524;
decode_char(16#B5) -> 16#2561;
decode_char(16#B6) -> 16#2562;
decode_char(16#B7) -> 16#2556;
decode_char(16#B8) -> 16#2555;
decode_char(16#B9) -> 16#2563;
decode_char(16#BA) -> 16#2551;
decode_char(16#BB) -> 16#2557;
decode_char(16#BC) -> 16#255D;
decode_char(16#BD) -> 16#255C;
decode_char(16#BE) -> 16#255B;
decode_char(16#BF) -> 16#2510;
decode_char(16#C0) -> 16#2514;
decode_char(16#C1) -> 16#2534;
decode_char(16#C2) -> 16#252C;
decode_char(16#C3) -> 16#251C;
decode_char(16#C4) -> 16#2500;
decode_char(16#C5) -> 16#253C;
decode_char(16#C6) -> 16#255E;
decode_char(16#C7) -> 16#255F;
decode_char(16#C8) -> 16#255A;
decode_char(16#C9) -> 16#2554;
decode_char(16#CA) -> 16#2569;
decode_char(16#CB) -> 16#2566;
decode_char(16#CC) -> 16#2560;
decode_char(16#CD) -> 16#2550;
decode_char(16#CE) -> 16#256C;
decode_char(16#CF) -> 16#2567;
decode_char(16#D0) -> 16#2568;
decode_char(16#D1) -> 16#2564;
decode_char(16#D2) -> 16#2565;
decode_char(16#D3) -> 16#2559;
decode_char(16#D4) -> 16#2558;
decode_char(16#D5) -> 16#2552;
decode_char(16#D6) -> 16#2553;
decode_char(16#D7) -> 16#256B;
decode_char(16#D8) -> 16#256A;
decode_char(16#D9) -> 16#2518;
decode_char(16#DA) -> 16#250C;
decode_char(16#DB) -> 16#2588;
decode_char(16#DC) -> 16#2584;
decode_char(16#DD) -> 16#258C;
decode_char(16#DE) -> 16#2590;
decode_char(16#DF) -> 16#2580;
decode_char(C) when C >= 16#e0, C =< 16#ef -> C + 16#360;
decode_char(16#F0) -> 16#0401;
decode_char(16#F1) -> 16#0451;
decode_char(16#F2) -> 16#0404;
decode_char(16#F3) -> 16#0454;
decode_char(16#F4) -> 16#0407;
decode_char(16#F5) -> 16#0457;
decode_char(16#F6) -> 16#040E;
decode_char(16#F7) -> 16#045E;
decode_char(16#F8) -> 16#00B0;
decode_char(16#F9) -> 16#2219;
decode_char(16#FA) -> 16#00B7;
decode_char(16#FB) -> 16#221A;
decode_char(16#FC) -> 16#2116;
decode_char(16#FD) -> 16#00A4;
decode_char(16#FE) -> 16#25A0;
decode_char(16#FF) -> 16#00A0;
decode_char(_) -> badarg.


decode_char2(C) ->
    element(C + 1, { 16#0000, 16#0001, 16#0002, 16#0003, 16#0004, 16#0005,
            16#0006, 16#0007, 16#0008, 16#0009, 16#000A, 16#000B, 16#000C,
            16#000D, 16#000E, 16#000F, 16#0010, 16#0011, 16#0012, 16#0013,
            16#0014, 16#0015, 16#0016, 16#0017, 16#0018, 16#0019, 16#001A,
            16#001B, 16#001C, 16#001D, 16#001E, 16#001F, 16#0020, 16#0021,
            16#0022, 16#0023, 16#0024, 16#0025, 16#0026, 16#0027, 16#0028,
            16#0029, 16#002A, 16#002B, 16#002C, 16#002D, 16#002E, 16#002F,
            16#0030, 16#0031, 16#0032, 16#0033, 16#0034, 16#0035, 16#0036,
            16#0037, 16#0038, 16#0039, 16#003A, 16#003B, 16#003C, 16#003D,
            16#003E, 16#003F, 16#0040, 16#0041, 16#0042, 16#0043, 16#0044,
            16#0045, 16#0046, 16#0047, 16#0048, 16#0049, 16#004A, 16#004B,
            16#004C, 16#004D, 16#004E, 16#004F, 16#0050, 16#0051, 16#0052,
            16#0053, 16#0054, 16#0055, 16#0056, 16#0057, 16#0058, 16#0059,
            16#005A, 16#005B, 16#005C, 16#005D, 16#005E, 16#005F, 16#0060,
            16#0061, 16#0062, 16#0063, 16#0064, 16#0065, 16#0066, 16#0067,
            16#0068, 16#0069, 16#006A, 16#006B, 16#006C, 16#006D, 16#006E,
            16#006F, 16#0070, 16#0071, 16#0072, 16#0073, 16#0074, 16#0075,
            16#0076, 16#0077, 16#0078, 16#0079, 16#007A, 16#007B, 16#007C,
            16#007D, 16#007E, 16#007F, 16#0410, 16#0411, 16#0412, 16#0413,
            16#0414, 16#0415, 16#0416, 16#0417, 16#0418, 16#0419, 16#041A,
            16#041B, 16#041C, 16#041D, 16#041E, 16#041F, 16#0420, 16#0421,
            16#0422, 16#0423, 16#0424, 16#0425, 16#0426, 16#0427, 16#0428,
            16#0429, 16#042A, 16#042B, 16#042C, 16#042D, 16#042E, 16#042F,
            16#0430, 16#0431, 16#0432, 16#0433, 16#0434, 16#0435, 16#0436,
            16#0437, 16#0438, 16#0439, 16#043A, 16#043B, 16#043C, 16#043D,
            16#043E, 16#043F, 16#2591, 16#2592, 16#2593, 16#2502, 16#2524,
            16#2561, 16#2562, 16#2556, 16#2555, 16#2563, 16#2551, 16#2557,
            16#255D, 16#255C, 16#255B, 16#2510, 16#2514, 16#2534, 16#252C,
            16#251C, 16#2500, 16#253C, 16#255E, 16#255F, 16#255A, 16#2554,
            16#2569, 16#2566, 16#2560, 16#2550, 16#256C, 16#2567, 16#2568,
            16#2564, 16#2565, 16#2559, 16#2558, 16#2552, 16#2553, 16#256B,
            16#256A, 16#2518, 16#250C, 16#2588, 16#2584, 16#258C, 16#2590,
            16#2580, 16#0440, 16#0441, 16#0442, 16#0443, 16#0444, 16#0445,
            16#0446, 16#0447, 16#0448, 16#0449, 16#044A, 16#044B, 16#044C,
            16#044D, 16#044E, 16#044F, 16#0401, 16#0451, 16#0404, 16#0454,
            16#0407, 16#0457, 16#040E, 16#045E, 16#00B0, 16#2219, 16#00B7,
            16#221A, 16#2116, 16#00A4, 16#25A0, 16#00A0 }).
