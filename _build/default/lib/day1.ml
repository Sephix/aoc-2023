let read_lines filename =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some line -> loop (line :: acc)
    | None -> close_in ic; List.rev acc
  in
  loop []

let filename = "../input/day1.txt"
let lines = read_lines filename
(* let lines = [ *)
(*   "two1nine"; *)
(*   "eightwothree"; *)
(*   "abcone2threexyz"; *)
(*   "xtwone3four"; *)
(*   "4nineeightseven2"; *)
(*   "zoneight234"; *)
(*   "7pqrstsixteen"; *)
(* ] *)
let string_to_number (str: string) =
  match str with
  | "" -> ""
  | s when s.[0] >= '0' && s.[0] <= '9' -> Char.escaped s.[0]
  | s when String.length s >= 3 ->
      (match String.sub s 0 3 with
        | "one" -> "1"
        | "two" -> "2"
        | "six" -> "6"
        | _ when String.length str >= 4 ->
            (match String.sub str 0 4 with
              | "four" -> "4"
              | "five" -> "5"
              | "nine" -> "9"
                | _ when String.length str >= 5 ->
                  (match String.sub s 0 5 with
                  | "three" -> "3"
                  | "seven" -> "7"
                  | "eight" -> "8" 
                  | _ -> "")
              | _ -> "")
        | _ -> "")
  | _ -> ""

let rec line_to_number = function
  | "" -> ""
  | s -> string_to_number s ^ line_to_number (String.sub s 1 (String.length s-1))

let sanatized_lines = List.map line_to_number lines

let is_digit = function '0' .. '9' -> true | _ -> false

let rec line_digit x acc = match x with
  | [] -> acc |> List.to_seq |> String.of_seq
  | current :: rest -> 
      if (is_digit current) then
        (line_digit rest (acc @ [current]))
      else (line_digit rest acc)

let rec input_digit input = match input with
  | [] -> []
  | current :: rest -> (line_digit (current |> String.to_seq |> List.of_seq)[]) :: (input_digit rest)
let rec get_line_value = function
  | [] -> []
  | current :: rest -> 
      ((String.get current 0 |> Char.escaped) ^ (String.get current (String.length current - 1) |> Char.escaped))
      :: (get_line_value rest)

let rec sum_values (values: string list) = match values with
  | [] -> 0
  | current :: rest -> int_of_string current + sum_values rest


let result1 = input_digit lines |> get_line_value |> sum_values |> string_of_int
let result2 = input_digit sanatized_lines |> get_line_value |> sum_values |> string_of_int 

