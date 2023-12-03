(* Day 2 *)
type filepath = string
(* filepath -> string *)
fun readFile filePath =
    let val fd = TextIO.openIn filePath
        val s = TextIO.inputAll fd
        val _ = TextIO.closeIn fd
    in s end;

(* char -> string -> string list *)
fun splitBy char string =
    String.tokens (fn c => c = char) string;

fun parseId s =
    let val chars = String.explode s
        val ints =  List.filter Char.isDigit chars
        val intString = String.implode ints
    in
        Option.getOpt((Int.fromString intString), 0) (* Ids are positive integers *)
    end;

fun parseCube s =
    let val l = String.tokens Char.isSpace s
        val num = Option.getOpt(Int.fromString (List.hd l), 0) (* the number of cubes is at least zero *)
        val color = List.last l
    in
        (num, color)
    end;

fun getPower rounds =
    let fun max (a, b) = if a > b then a else b;
        fun maxColor (r1, r2) = let val (r:int, g:int, b:int) = r1
                                  val (x:int, y:int, z:int) = r2
                              in
                                  (max(r, x), max(g, y), max(b, z))
                              end;
        val (r, g, b) = (List.foldl maxColor (0, 0, 0) rounds)                        
    in
        r * g * b
    end;

type game = { id : int,
              rounds:(int * int * int) list,
              power: int}

fun gameFromString string =
    let val (id::game) =  (splitBy #":" string)
        val rounds = splitBy #";" (List.hd game)
        val cubes = List.map (splitBy #",") rounds
        fun packCubes (y, acc) = let val (r:int, g:int, b:int) = acc
                                     val (num:int, color: string) = y
                                 in
                                     case color of
                                         "red" => ((r+num), g, b)
                                       | "green" => (r, (g+num), b)
                                       | "blue" => (r, g, (b+num))
                                       | _ => (r, g, b)
                                 end;
        val parsed = List.map (List.map parseCube) cubes
        val packed = (List.map (List.foldr packCubes (0,0,0)) parsed)
        val result: game = {
            id = parseId(id),
            rounds = packed,
            power = getPower packed
        };
    in
        result
    end;


fun isValidGame game =
    let val {rounds=rounds, id=_, power=_} = game
    in
        List.all (fn (r, g, b) => r <= 12 andalso g <= 13 andalso b <=14) rounds
    end;

fun id {id=x, rounds=_, power=_} = x;
fun power {id=_, rounds=_, power=x} = x;

fun main _ =
    let val input = ((splitBy #"\n") o readFile) "./input.txt"
        fun part1 _ =
            List.foldr op+ 0
                       (List.map id
                                 (List.filter isValidGame
                                              (List.map gameFromString input)));
        fun part2 _ =
            List.foldr op+ 0
                       (List.map power
                                 (List.map gameFromString input));
    in
        print ("Part 1: " ^ (Int.toString (part1())) ^ "\n");
        print ("Part 2: " ^ (Int.toString (part2())) ^ "\n")
    end;
