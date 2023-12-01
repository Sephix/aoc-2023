open Base

let read_lines filename =
  let ic = Stdio.In_channel.create filename in
  let try_read () =
    try Some (Stdio.In_channel.input_line_exn ic) with
    | End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some line -> loop (line :: acc)
    | None ->
      Stdio.In_channel.close ic;
      List.rev acc
  in
  loop []
;;

let filename = "../input/day1.txt"
let lines = read_lines filename

let string_to_number = function
  | s when Char.is_digit s.[0] -> Char.escaped s.[0]
  | s when String.is_prefix ~prefix:"one" s -> "1"
  | s when String.is_prefix ~prefix:"two" s -> "2"
  | s when String.is_prefix ~prefix:"three" s -> "3"
  | s when String.is_prefix ~prefix:"four" s -> "4"
  | s when String.is_prefix ~prefix:"five" s -> "5"
  | s when String.is_prefix ~prefix:"six" s -> "6"
  | s when String.is_prefix ~prefix:"seven" s -> "7"
  | s when String.is_prefix ~prefix:"eight" s -> "8"
  | s when String.is_prefix ~prefix:"nine" s -> "9"
  | _ -> ""
;;

let rec line_to_number = function
  | "" -> ""
  | s ->
    let next = String.sub s ~pos:1 ~len:(String.length s - 1) in
    string_to_number s ^ line_to_number next
;;

let sanatized_lines = List.map lines ~f:line_to_number

let line_digit line =
  let list_char = String.to_list line in
  let filtered = List.filter list_char ~f:Char.is_digit in
  String.of_char_list filtered
;;

let input_digit input = List.map input ~f:line_digit

let rec get_line_value = function
  | [] -> []
  | current :: rest ->
    ((String.get current 0 |> Char.escaped)
     ^ (String.get current (String.length current - 1) |> Char.escaped))
    :: get_line_value rest
;;

let rec sum_values (values : string list) =
  match values with
  | [] -> 0
  | current :: rest -> Int.of_string current + sum_values rest
;;

let result1 = input_digit lines |> get_line_value |> sum_values |> Int.to_string
let result2 = input_digit sanatized_lines |> get_line_value |> sum_values |> Int.to_string
