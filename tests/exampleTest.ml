open Unix

let get_input input =
  match input with Some s -> s | None -> ""

let print_error test_name input expected actual =
  print_endline ("- " ^ test_name ^ ": FAILED " ^ (get_input input));
  print_endline ("  Expected result to = " ^ expected);
  print_endline ("  But actual result  = " ^ actual)

let print_success test_name input =
  print_endline ("- " ^ test_name ^ ": PASSED " ^ (get_input input))

let run_example folder test_name input = try
  match input with
  | None -> input_line (Unix.open_process_in ("./main.native examples/" ^ folder ^ "/" ^ test_name))
  | Some s -> input_line (Unix.open_process_in ("echo '" ^ s ^ "' | ./main.native examples/" ^ folder ^ "/" ^ test_name))
  with End_of_file -> ""

let run_test folder test_name expected input =
  let actual = run_example folder test_name input in
  match actual = expected with
  | true -> print_success test_name input
  | false -> print_error test_name input expected actual

let compile_example path =
  let _ = Sys.command ("./main.native examples/" ^ path) in ()

let compile_program () =
  let _ = Sys.command "make" in ()

let test folder name expected input =
  run_test folder name expected input

let () =
  compile_program ();
  print_endline "Testing Examples\n";
  print_endline "\nConditionals";
  test "conditionals" "ifFalse" "14 : Int" None;
  test "conditionals" "ifTrue" "True : Bool" None;
  print_endline "\nLambdas";
  test "lambdas" "let" "15 : Int" None;
  test "lambdas" "letNested" "True" None;
  test "lambdas" "semi" "(λx.x)" None;
  print_endline "\nPairs";
  test "pairs" "fst" "1 : Int" None;
  test "pairs" "snd" "(True, False) : (Bool × Bool)" None;
  test "pairs" "pair" "(3, (True, 5)) : (Int × (Bool × Int))" None;
  test "pairs" "swap" "(2, 1) : (Int × Int)" None;
  test "pairs" "unit" "() : ()" None;
  print_endline "\nPattern Matching";
  test "pattern-matching" "matchInt" "second : String" None;
  test "pattern-matching" "matchList" "2 : Int" None;
  test "pattern-matching" "matchPair" "3 : Int" None;
  test "pattern-matching" "matchStr" "1 : Int" None;
  test "pattern-matching" "matchVar" "True : Bool" None;
  print_endline "\nRecursion";
  test "recursion" "factorial" "120 : Int" None;
  test "recursion" "fib" "55 : Int" None;
  print_endline "\nStrings";
  test "strings" "string" "Hello, world! : String" None;
  print_endline "\nSum";
  test "sum" "cons" "[3, 4, 5] : [Int]" None;
  test "sum" "list" "[1, 2, 3] : [Int]" None;
  test "sum" "mkList" "[1, 2] : [Int]" None;
  test "sum" "nil" "[] : ['a]" None;
  print_endline "\nVariants";
  test "variants" "inl" "inl 4 : (Int + 'b1)" None;
  test "variants" "inr" "inr (4, 5) : ('a0 + (Int × Int))" None;
