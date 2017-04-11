module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

//Helper AI Processes
let GetRemovedDeadwood cardList removed =   
    cardList |> List.filter (fun x -> not(x.Equals(removed))) |> List.toSeq |> GinRummy.Deadwood

let rec PossibleDiscardScores currentPosition (cardList:Card list) =
    if(currentPosition < cardList.Length) then 
        [GetRemovedDeadwood cardList (cardList.[currentPosition])] @ (PossibleDiscardScores (currentPosition+1) cardList)
    else []

let FindDiscardCard (cards:Card list) = 
    let discardScores = PossibleDiscardScores 0 cards
    let discardPos = discardScores |> List.mapi (fun i v -> i, v) |> List.minBy snd |> fst
    cards.[discardPos]

let GetBestHandScore cardList = 
    cardList |> FindDiscardCard |> GetRemovedDeadwood cardList

let rec GetDeckScores computerHand possibleDeck = 
    match possibleDeck with
    | [] -> []
    | head::tail -> (GetBestHandScore (computerHand @ [head])) :: (GetDeckScores computerHand tail)

//Main AI Processes
let ComputerPickupDiscard (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) =
    let DiscardScore = GetBestHandScore ((List.ofSeq computerHand) @ [topDiscard])                                 
    let DeckScore = (List.ofSeq possibleDeck) |> GetDeckScores (List.ofSeq computerHand) |> List.averageBy (fun x -> double x)
    if(double DiscardScore < DeckScore) then true else false

let ComputerMove newHand =
    if ((GinRummy.Deadwood newHand) = 0 ) then (Gin, None)
    else  
        let cards = List.ofSeq newHand
        let Discard = FindDiscardCard cards
        let score = GetRemovedDeadwood cards Discard

        if(score = 0) then (Gin, Some Discard)
        else if(score <= 10) then (Knock, Some Discard)
        else (Continue, Some Discard)
                    
let CalculatePossibleDeck (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) = 
    let removeDiscard = Seq.filter (fun x -> not (x.Equals topDiscard)) possibleDeck
    let hand = Set.ofSeq computerHand
    Seq.filter (fun x -> not (hand.Contains x)) removeDiscard
   
    