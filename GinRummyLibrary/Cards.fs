module Cards
open System

type Suit = Spades | Clubs | Hearts | Diamonds
type Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Card = { suit: Suit; rank: Rank}

type Hand = Card seq
type Deck = Card seq

let AllSuits = [ Spades; Clubs; Hearts; Diamonds ]
let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let rnd = new Random()

let allCards = 
    seq { 
        for s in AllSuits do
            for r in AllRanks do
                yield {suit=s; rank=r}
    }

let FullDeck = 
    allCards

let Shuffle (deck:Deck) = 
    Seq.sortBy (fun x -> rnd.Next()) deck

// Add other functions here related to Card Games ...
let CheckDuplicates cards = 
    let Duplicates = cards |> Seq.groupBy id |> Seq.map snd |> Seq.exists (fun s -> (Seq.length s) > 1)
    if Duplicates then 
        raise (new System.Exception "Duplicates Found!")

let rec flatten l =
    match l with 
    | [] -> []
    | head::tail -> head @ flatten tail

let combine a b = 
    a @ b

let rec reverse l = 
    match l with 
    | [] -> []
    | head::tail -> (reverse tail) @ [head]