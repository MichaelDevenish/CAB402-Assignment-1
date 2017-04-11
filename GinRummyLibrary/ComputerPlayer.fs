module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue
    
let GetRemovedDeadwood cardList removed =   
    cardList 
    |> List.filter (fun x -> not(x.Equals(removed))) 
    |> List.toSeq 
    |> GinRummy.Deadwood

let rec getDiscardScores currentPos (cardList:Card list) =
    if(currentPos < cardList.Length) then 
        [GetRemovedDeadwood cardList (cardList.[currentPos])] @ (getDiscardScores (currentPos+1) cardList)
    else []

let getDiscard (cards:Card list) discardScores = 
    let discardPos = 
        discardScores
        |> List.mapi (fun i v -> i, v) 
        |> List.minBy snd
        |> fst

    cards.[discardPos]

let GetBestHandScore cardList = 
    let set =  (GinRummy.checkSet cardList 1) @ (GinRummy.checkRun cardList 0) 
    let test = getDiscardScores 0 cardList
    let discard = getDiscard cardList test
    GetRemovedDeadwood cardList discard

let rec GetDeckScores computerHand possibleDeck = 
    match possibleDeck with
    | [] -> []
    | head::tail -> 
        GetBestHandScore (computerHand @ [head]) :: (GetDeckScores computerHand tail)

let ComputerPickupDiscard (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) =
    let DiscardScore = GetBestHandScore (List.ofSeq computerHand @ [topDiscard])                                 
    let DeckScore = 
        (List.ofSeq possibleDeck)
        |> GetDeckScores (List.ofSeq computerHand) 
        |> List.averageBy (fun x -> double x)

    if(double DiscardScore < DeckScore) then true else false

let ComputerMove newHand =
    let score = (GinRummy.Deadwood newHand);
    if (score = 0 ) then 
        (Gin, None)
    else  
        let cards = List.ofSeq newHand
        let test = getDiscardScores 0 cards
        let Discard = getDiscard cards test
        let score = GetRemovedDeadwood cards Discard

        if(score = 0) then (Gin, Some Discard)
        else if(score <= 10) then (Knock, Some Discard)
        else (Continue, Some Discard)
            
let calculatePossibleDeck (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) = 
    let removeDiscard = Seq.filter (fun x -> not (x.Equals topDiscard)) possibleDeck
    let hand = Set.ofSeq computerHand
    Seq.filter (fun x -> not (hand.Contains x)) removeDiscard
   
    