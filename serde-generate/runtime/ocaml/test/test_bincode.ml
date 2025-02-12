open Alcotest
open Stdint
open Serde
module Ser = Serialize
module De = Deserialize

let vec a =
  Bytes.init (Array.length a) (fun i -> Char.chr a.(i))

let mk buffer = { De.buffer; offset = 0 }

let check_fail f =
  (check bool) "fail" true (try let _ = f () in false with _ -> true)

let test_bool_ser_false () = (check bytes) "same bytes" (vec [|0|]) (Ser.bool false)
let test_bool_ser_true () = (check bytes) "same bytes" (vec [|1|]) (Ser.bool true)
let test_bool_de_false () = (check bool) "same bool" false (De.bool @@ mk @@ vec [|0|])
let test_bool_de_true () = (check bool) "same bool" true (De.bool @@ mk @@ vec [|1|])
let test_bool_fail_2 () = check_fail (fun () -> De.bool @@ mk @@ vec [|2|])
let test_bool_fail_empty () = check_fail (fun () -> De.bool @@ mk @@ vec [||])

let test_u8_ser () = (check bytes) "same bytes" (vec [|1|]) (Ser.uint8 (Uint8.of_int 1))
let test_u8_de () = (check int) "same int" 255 (Uint8.to_int @@ De.uint8 @@ mk @@ vec [|0xff|])

let test_u16_ser () = (check bytes) "same bytes" (vec [|2; 1|]) (Ser.uint16 (Uint16.of_int 258))
let test_u16_de () = (check int) "same int" 65535 (Uint16.to_int @@ De.uint16 @@ mk @@ vec [|0xff; 0xff|])

let test_u32_ser () = (check bytes) "same bytes" (vec [|4; 3; 2; 1|]) (Ser.uint32 (Uint32.of_int 16909060))
let test_u32_de () = (check int) "same int" 4294967295 (Uint32.to_int @@ De.uint32 @@ mk @@ vec [|0xff; 0xff; 0xff; 0xff|])

let test_u64_ser () = (check bytes) "same bytes" (vec [|8; 7; 6; 5; 4; 3; 2; 1|]) (Ser.uint64 (Uint64.of_int 72623859790382856))
let test_u64_de () = (check bool) "same int" true Uint128.(((shift_left (of_int 1) 64) - (of_int 1)) = Uint128.of_uint64 @@ De.uint64 @@ mk @@ vec @@ Array.make 8 0xff)

let test_u128_ser () = (check bytes) "same bytes" (vec [|16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1|]) (Ser.uint128 (Uint128.of_string "0x0102030405060708090A0B0C0D0E0F10" ))
let test_u128_de () = (check bool) "same int" true Uint128.(of_string "0xffffffffffffffffffffffffffffffff" = De.uint128 @@ mk @@ vec @@ Array.make 16 0xff)

let test_i8_ser_pos () = (check bytes) "same bytes" (vec [|4|]) (Ser.int8 (Int8.of_int 4))
let test_i8_ser_neg () = (check bytes) "same bytes" (vec [|0xfe|]) (Ser.int8 (Int8.of_int (-2)))
let test_i8_de () = (check int) "same int" (-1) (Int8.to_int @@ De.int8 @@ mk @@ vec [|0xff|])

let test_i16_ser () = (check bytes) "same bytes" (vec [|2; 1|]) (Ser.int16 (Int16.of_int 258))
let test_i16_de () = (check int) "same int" (-1) (Int16.to_int @@ De.int16 @@ mk @@ vec [|0xff; 0xff|])

let test_i32_ser () = (check bytes) "same bytes" (vec [|4; 3; 2; 1|]) (Ser.int32 (Int32.of_int 16909060))
let test_i32_de () = (check int) "same int" (-1) (Int32.to_int @@ De.int32 @@ mk @@ vec [|0xff; 0xff; 0xff; 0xff|])

let test_i64_ser () = (check bytes) "same bytes" (vec [|8; 7; 6; 5; 4; 3; 2; 1|]) (Ser.int64 (Int64.of_int 72623859790382856))
let test_i64_de () = (check bool) "same int" true (Stdlib.Int64.minus_one = De.int64 @@ mk @@ vec @@ Array.make 8 0xff)

let test_i128_ser () = (check bytes) "same bytes" (vec [|16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1|]) (Ser.uint128 (Uint128.of_string "0x0102030405060708090A0B0C0D0E0F10" ))
let test_i128_de () = (check bool) "same int" true Uint128.(of_int (-1) = De.uint128 @@ mk @@ vec @@ Array.make 16 0xff)

let test_f64_ser () = (check bytes) "same bytes" (vec [|0x1a; 0xdf; 0xc4; 0x41; 0x66; 0x63; 0x8a; 0x3d|]) (Ser.float64 0.000000000003)
let test_f64_de () = (check bool) "same float" true (0.000000000003 = De.float64 @@ mk @@ vec [|0x1a; 0xdf; 0xc4; 0x41; 0x66; 0x63; 0x8a; 0x3d|])

let test_bytes_ser_empty () = (check bytes) "same bytes" (vec [|0; 0; 0; 0; 0; 0; 0; 0|]) (Ser.bytes Bytes.empty)
let test_bytes_ser () = (check bytes) "same bytes" (vec [|2; 0; 0; 0; 0; 0; 0; 0; 0; 0|]) (Ser.bytes (Bytes.make 2 '\000'))
let test_bytes_de () = (check bytes) "same bytes" (vec [|0; 0; 0; 0; 0; 0; 0; 0|]) (Ser.bytes Bytes.empty)

type tuple = (uint8 * uint16) [@@deriving serde]
let test_tuple_ser () = (check bytes) "same bytes" (vec [|0; 1; 0|]) (tuple_ser ((Uint8.of_int 0), Uint16.of_int 1))
let test_tuple_de () = (check bool) "same tuple" true ((Uint8.of_int 2, Uint16.of_int 1) = tuple_de @@ mk @@ vec [|2; 1; 0|])

let test_option_ser_none () = (check bytes) "same bytes" (vec [|0|]) (Ser.option Ser.uint16 None)
let test_option_ser_some () = (check bytes) "same bytes" (vec [|1; 6; 0|]) (Ser.option Ser.uint16 (Some (Uint16.of_int 6)))
let test_option_de_none () = (check bool) "same" true (None = De.option De.uint16 (mk @@ vec [|0|]))
let test_option_de_some () = (check bool) "same" true (Some (Uint16.of_int 2) = De.option De.uint16 (mk @@ vec [|1; 2; 0|]))
let test_option_de_fail () = check_fail (fun () -> De.option De.uint16 (mk @@ vec [|2; 6; 0|]))

let test_seq_ser_empty () = (check bytes) "same bytes" (vec [|0; 0; 0; 0; 0; 0; 0; 0|]) (Ser.variable Ser.uint16 [])
let test_seq_ser_small () = (check bytes) "same bytes" (vec [|2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0|]) (Ser.variable Ser.uint16 [Uint16.of_int 0; Uint16.of_int 1])
let test_seq_ser_big () = (check bytes) "same bytes"
    (vec (Array.concat ([|0; 1; 0; 0; 0; 0; 0; 0|] :: List.init 256 (fun _ -> [|0; 1|]))))
    (Ser.variable Ser.uint16 @@ List.init 256 (fun _ -> Uint16.of_int 256))
let test_seq_de () = (check bool) "same" true ([Uint16.of_int 3] = De.variable De.uint16 @@ mk @@ vec [|1; 0; 0; 0; 0; 0; 0; 0; 3; 0|])

let test_string_ser () =
  (check bytes) "same bytes" (vec [|5; 0; 0; 0; 0; 0; 0; 0; 65; 66; 67; 0xce; 0x94|]) (Ser.string "ABC\u{0394}")
let test_string_de () =
  (check string) "same" "ABC\u{0394}" (De.string @@ mk @@ vec [|5; 0; 0; 0; 0; 0; 0; 0; 65; 66; 67; 0xce; 0x94|])
let test_string_fail_length () =
  check_fail (fun () -> De.string @@ mk @@ vec [|3; 0; 0; 0; 0; 0; 0; 0; 65; 66|])
let test_string_fail_utf8 () =
  check_fail (fun () -> De.string @@ mk @@ vec [|3; 0; 0; 0; 0; 0; 0; 0; 0x80; 97; 98|])

let test_long_seq () =
  let i = Uint16.of_int 5 in
  let l = List.init 1000000 (fun _ -> i) in
  let b = Ser.(variable uint16) l in
  (check bool) "same" true (l = De.(variable uint16) @@ mk b)

let test_map () =
  let compare k1 k2 = Bytes.compare (Ser.uint16 k1) (Ser.uint16 k2) in
  let m = Map.empty in
  let m = Map.add ~compare (Uint16.of_int 1) (Uint8.of_int 5) m in
  let m = Map.add ~compare (Uint16.of_int 256) (Uint8.of_int 3) m in
  let b = Ser.(map uint16 uint8) m in
  (check bytes) "same bytes" (vec [|2; 0; 0; 0; 0; 0; 0; 0; 0; 1; 3; 1; 0; 5|]) b;
  (check bool) "same" true ((Map.bindings @@ De.map Ser.uint16 De.uint16 De.uint8 @@ mk b) = Map.bindings m);
  let m2 = Map.empty in
  let m2 = Map.add ~compare (Uint16.of_int 256) (Uint8.of_int 3) m2 in
  let m2 = Map.add ~compare (Uint16.of_int 1) (Uint8.of_int 5) m2 in
  let b2 = Ser.(map uint16 uint8) m2 in
  (check bytes) "same bytes" b b2;
  check_fail (fun () -> De.map Ser.uint16 De.uint16 De.uint8 @@ mk @@ vec [|2; 1; 0; 5; 0; 1; 3|])

type foo = {
  x: uint8;
  y: uint16;
} [@@deriving serde]

let test_struct_ser () =
  (check bytes) "same bytes" (vec [|0; 1; 0|]) @@ foo_ser {x = Uint8.zero; y = Uint16.one}

let test_struct_de () =
  (check bool) "same" true ({x = Uint8.of_int 2; y = Uint16.one} = foo_de @@ mk @@ vec [|2; 1; 0|])

type bar =
  | A
  | B of foo
  | C
[@@deriving serde]

let test_variant_ser () =
  (check bytes) "same bytes" (vec [|1; 0; 0; 0; 0; 1; 0|]) @@ bar_ser (B {x = Uint8.zero; y = Uint16.one})

let test_variant_de () =
  (check bool) "same" true (B {x = Uint8.of_int 2; y = Uint16.one} = bar_de @@ mk @@ vec [|1; 0; 0; 0; 2; 1; 0|])

let () =
  run "bincode" [
    "bool", [
      test_case "serialize false" `Quick test_bool_ser_false;
      test_case "serialize true" `Quick test_bool_ser_true;
      test_case "deserialize false" `Quick test_bool_de_false;
      test_case "deserialize true" `Quick test_bool_de_true;
      test_case "deserialize fail 2" `Quick test_bool_fail_2;
      test_case "deserialize fail empty" `Quick test_bool_fail_empty;
    ];
    "uint8", [
      test_case "serialize 1u8" `Quick test_u8_ser;
      test_case "deserialize ff" `Quick test_u8_de;
    ];
    "uint16", [
      test_case "serialize 258u16" `Quick test_u16_ser;
      test_case "deserialize ffff" `Quick test_u16_de;
    ];
    "uint32", [
      test_case "serialize 16909060u32" `Quick test_u32_ser;
      test_case "deserialize ffffffff" `Quick test_u32_de;
    ];
    "uint64", [
      test_case "serialize 72623859790382856" `Quick test_u64_ser;
      test_case "deserialize ffffffffffffffff" `Quick test_u64_de;
    ];
    "uint128", [
      test_case "serialize 1339673755198158349044581307228491536" `Quick test_u128_ser;
      test_case "deserialize ffffffffffffffffffffffffffffffff" `Quick test_u128_de;
    ];
    "int8", [
      test_case "serialize 4i8" `Quick test_i8_ser_pos;
      test_case "serialize -2i8" `Quick test_i8_ser_neg;
      test_case "deserialize ff" `Quick test_i8_de;
    ];
    "int16", [
      test_case "serialize 258i16" `Quick test_i16_ser;
      test_case "deserialize ffff" `Quick test_i16_de;
    ];
    "int32", [
      test_case "serialize 16909060" `Quick test_i32_ser;
      test_case "deserialize ffffffff" `Quick test_i32_de;
    ];
    "int64", [
      test_case "serialize 72623859790382856" `Quick test_i64_ser;
      test_case "deserialize ffffffffffffffff" `Quick test_i64_de;
    ];
    "int128", [
      test_case "serialize 72623859790382856" `Quick test_i128_ser;
      test_case "deserialize ffffffffffffffffffffffffffffffff" `Quick test_i128_de;
    ];
    "float64", [
      test_case "serialize" `Quick test_f64_ser;
      test_case "deserialize" `Quick test_f64_de;
    ];
    "bytes", [
      test_case "serialize empty" `Quick test_bytes_ser_empty;
      test_case "serialize 0x00,0x00" `Quick test_bytes_ser;
      test_case "deserialize" `Quick test_bytes_de;
    ];
    "tuple", [
      test_case "serialize" `Quick test_tuple_ser;
      test_case "deserialize" `Quick test_tuple_de;
    ];
    "option", [
      test_case "serialize none" `Quick test_option_ser_none;
      test_case "serialize some" `Quick test_option_ser_some;
      test_case "deserialize none" `Quick test_option_de_none;
      test_case "deserialize some" `Quick test_option_de_some;
      test_case "deserialize fail" `Quick test_option_de_fail;
    ];
    "sequence", [
      test_case "serialize []" `Quick test_seq_ser_empty;
      test_case "serialize [0, 1]" `Quick test_seq_ser_small;
      test_case "serialize [...]" `Quick test_seq_ser_big;
      test_case "deserialize" `Quick test_seq_de;
    ];
    "string", [
      test_case "serialize" `Quick test_string_ser;
      test_case "deserialize" `Quick test_string_de;
      test_case "deserialize fail length" `Quick test_string_fail_length;
      test_case "deserialize fail utf8" `Quick test_string_fail_utf8;
    ];
    "long seq", [
      test_case "long seq" `Quick test_long_seq;
    ];
    "map", [
      test_case "map" `Quick test_map;
    ];
    "struct", [
      test_case "serialize" `Quick test_struct_ser;
      test_case "deserialize" `Quick test_struct_de;
    ];
    "variant", [
      test_case "serialize" `Quick test_variant_ser;
      test_case "deserialize" `Quick test_variant_de;
    ];

  ]
