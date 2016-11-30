(*
 * Sharpstone: a tiny card game simulator
 *
 * Written as project exam template for Computer Science, Laboratorio di Programmazione
 * Freely adapted from Heartstone (TM) by Blizzard Entertainment, Inc.
 *
 * (C) 2016 Alvise Spanò @ DAIS, Università Ca' Foscari, Venezia
 *)

module LabProg2016.Sharpstone

#if INTERACTIVE
#r "System.Runtime.Serialization.dll"
#endif

open System
open System.IO
open System.Runtime.Serialization
open System.Text

// globals
//

let rand = new Random (23)    // remove seed argument for making randomness indeterministic

/// Generate a random integer within the interval (a, b) inclusively.
let rnd_int a b = rand.Next (a, b + 1) 


// type definitions
//

/// Defines the card type.
[< DataContract; StructuralEquality; NoComparison >]
type card = {
    [< field: DataMember(Name = "id") >] id : string
    [< field: DataMember(Name = "name") >] name : string
    [< field: DataMember(Name = "cost") >] cost : int
    [< field: DataMember(Name = "type") >] typee : string
    [< field: DataMember(Name = "attack") >] attack : int
    [< field: DataMember(Name = "health") >] mutable health : int
}
with
    override c.ToString () = sprintf "%s [Id:%s  Atk:%d  HP:%d]" c.name c.id c.attack c.health

/// Deck type alias.
type deck = card list

/// Defined the player type.
[< StructuralEquality; NoComparison >]
type player = {
    name : string
    mutable life : int
    mutable deck : deck
}
with
    override p.ToString () = sprintf "%s [Life:%d  Deck:%d]" p.name p.life p.deck.Length


// JSON stuff
//

/// Convert a JSON string into a typed value.
let unjson<'t> (input : string) : 't =  
    use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(input)) 
    let obj = (new Json.DataContractJsonSerializer(typeof<'t>)).ReadObject(ms) 
    obj :?> 't

/// Parse a JSON deck given the filename.
let parse_deck (filename : string) = 
    use fstr = File.OpenRead filename
    use rd = new StreamReader (fstr)
    printfn "Parsing JSON file \"%s\"..." fstr.Name
    rd.ReadToEnd () |> unjson<card[]> |> Array.toList


// printers
//

/// Prints the turn number header. Call this function at the beginning of each turn.
let print_turn_begin (turn : int) = printfn "\nTurn %d:" turn

/// Prints the status of the 2 players. Call this function at the end of each turn.
let print_turn_end (p1 : player, p2 : player) = printfn "\t%O\n\t%O" p1 p2

/// Prints the information of 2 cards fighting. Call this function at each turn when both players have a card.
let print_turn_2cards (c1 : card, c2 : card) = printfn "%O VS %O" c1 c2

/// Prints the information of 1 card fighting against a player with no cards. Call this function at each turn when only 1 players have a card.
let print_turn_1card (p : player, c : card) = printfn "* %O VS player %O" c p

/// Prints the information of 2 players when both have no cards. Call this function at each turn no cards have been drawn.
let print_turn_no_cards (p1 : player, p2 : player) = printfn "* Both %O and %O have no cards" p1 p2

/// Prints the information of a dead cards. Call this function when a card dies.
let print_card_death (c : card) = printfn "+ %O died (%d overkill)" { c with health = 0 } -c.health

// combat mechanics
//

// Inserction sort
let rec insert_sort (i : 'a)  (l : 'a list) f : 'a list =
    match l with
    | [] -> [i]
    | h::t -> if (f h) > (f i) then i::l else h::(insert_sort i t f) // TODO - Auto swap with random if equals

// Remove useless cards
let clean_deck (deck1 : deck) : deck =
    let validate_card (card : card) : bool = 
        (card.typee = "MINION") && (card.attack > 0) && (card.health > 0)
    let rec clean_deck_ric ( deck1 : deck) : deck = 
        match deck1 with
        | []->[]
        | [x]->[]
        | x::y::xs -> if validate_card x then x::clean_deck_ric (y::xs) else clean_deck_ric (y::xs)
    clean_deck_ric deck1

let draw_card (mana : int) (player : player) : deck =   
    let normalize_mana (x : int) (max : int) =
      if x > max then max
      else x

    let calc_points (card : card) : float = float(card.attack) / float(card.health) // Calculate points for each card
    // Filter cards based on mana
    let rec filter_mana (deck : deck) (mana : int) (deckOut : deck): deck = 
        match deck with
        | []->[]
        | [x]->[]
        | x::y::xs -> if x.cost <= mana then filter_mana (y::xs) mana (insert_sort x deckOut calc_points) else filter_mana (y::xs) mana deckOut
    filter_mana player.deck (normalize_mana mana 10) []

// !!! YOU MUST IMPLEMENT THIS !!!
let fight (deck1 : deck) (deck2 : deck) : player * player * int =
    // Clean the decks
    let cleanDeck1 = clean_deck deck1
    let cleanDeck2 = clean_deck deck2

    // Define players status
    let p1 = { name ="P1"; life = 30; deck = cleanDeck1 }   // Player 1
    let p2 = { name ="P2"; life = 30; deck = cleanDeck2 }   // Player 2

    let mutable turn = 1           // Turn counter also defined as mana
    let mutable quit = false       // Cycle flag
    while not quit && p1.life > 0 && p2.life > 0 do
        print_turn_begin turn       // Begin turn
        
        // Extract cards
        let c1 = draw_card turn p1                                 
        let c2 = draw_card turn p2
        
        // TODO - Impelemt logic here :)      
        
        print_turn_end (p1, p2) // Print Turn results
        turn <- turn + 1
        quit <- true // TODO - Remove after done logic

    // Print game results
    if p1.life = p2.life then printfn "Tie"
    elif p1.life > p2.life then printfn "P1 wins"
    else printfn "P2 wins"
    p1, p2, turn - 1

// main code
//

[< EntryPoint >]
let main argv =
    let code =
        try
            if argv.Length <> 2 then
                printfn "Usage: Sharpstone <DECK1> <DECK2>"
                0
            else
                let p filename = parse_deck filename    // function for parsing a JSON file defining a deck as a list of cards
                let d1 = p argv.[0]                     // parse the first argument of the executable (DECK1)
                let d2 = p argv.[1]                     // parse the second argument of the executable (DECK2)
                let p1, p2, turn as r = fight d1 d2
                // print final result
                printfn "\nResult:\n\t%d Turns\n\t%O\n\t%O\n\tHash: %X" turn p1 p2 (r.GetHashCode ())
                0
                

        with e -> printfn "Uncaught exception: %O" e; 1

    #if DEBUG
    printfn "\n\nPress any key to exit..."
    Console.ReadKey () |> ignore
    #endif
    code