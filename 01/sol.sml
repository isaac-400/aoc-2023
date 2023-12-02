(* Day 1 solution attempt *)

type filepath = string
val stringNums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
		 "1", "2", "3", "4", "5", "6", "7", "8", "9"]

(* filepath -> string *)
fun readFile filePath =
    let val fd = TextIO.openIn filePath
        val s = TextIO.inputAll fd
        val _ = TextIO.closeIn fd
    in s end;

(* string -> string list *)
fun split s =
    String.tokens Char.isSpace s;

(* string -> string *)
fun translateNum s =
    case s of
	"one" => 1
      | "two" => 2
      | "three" => 3
      | "four" => 4
      | "five" => 5
      | "six" => 6
      | "seven" => 7
      | "eight" => 8
      | "nine" => 9
      | s => Option.getOpt(Int.fromString(s), 0) ;


fun forwardSearch string terms =
    case string of
	"" => NONE
      | string => case (List.find (fn x => String.isPrefix x string) terms) of
		 NONE => forwardSearch (String.extract(string, 1, NONE)) terms
		   |  x => x ;

fun backwardSearch string terms =
    case string of
	"" => NONE
      | string => case (List.find (fn x => String.isSuffix x string) terms) of
		      NONE => backwardSearch (String.extract(string, 0, SOME ((String.size string) - 1))) terms
		   |  x => x ;

fun charToInt c =
    Option.getOpt(Int.fromString (str c), ~1);

fun filterInts s =
    let val chars = String.explode s
    in List.map charToInt (List.filter Char.isDigit chars) end;

fun combineFirstAndLast l =
    ((List.hd l) * 10) + (List.last l);

fun part1 filepath =
    List.foldl op+ 0
	       (List.map
		    (combineFirstAndLast o filterInts)
		    ((split o readFile) filepath));

fun part2 filepath =
   List.foldl op+ 0
    ((List.map
	 (fn x =>
	      (combineFirstAndLast o (List.map translateNum)) [Option.getOpt((forwardSearch x stringNums), ""),
				      Option.getOpt((backwardSearch x stringNums), "")])
	 ((split o readFile) filepath)));
