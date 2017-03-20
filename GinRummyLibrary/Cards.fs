module Cards
open System

type Suit = Spades | Clubs | Hearts | Diamonds
type Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Card = { suit: Suit; rank: Rank}

type Hand = Card seq
type Deck = Card seq

let AllSuits = [ Spades; Clubs; Hearts; Diamonds ]
let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let allCards = 
    seq { 
        for s in AllSuits do
            for r in AllRanks do
                yield {suit=s; rank=r}
    }

let FullDeck = 
    allCards

let rec recShuffle arr (rand:System.Random) =
    match arr with
    | [] -> []
    | head::tail ->
        let nxt = rand.Next(0,tail.Length)
        if nxt = 0 then head::(recShuffle tail rand)
        else 
            let shuffle = List.map(fun index -> if index = tail.[nxt] then head else index) tail
            tail.[nxt]::(recShuffle shuffle rand)

let Shuffle (deck:Deck) = 
    let rnd = new Random()
    let deckList =List.ofSeq deck
    let result = recShuffle deckList rnd
    List.toSeq result 

// Add other functions here related to Card Games ...
