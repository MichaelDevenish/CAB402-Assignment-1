module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue
    
let GetBestHandScore cardList = 
    let rec calculateRemove (setsRunsList:Card list list) lengthCheck = 
        let filteredList = List.filter (fun (x:Card list) -> x.Length = lengthCheck ) setsRunsList
        if (filteredList.Length > 0 ) then filteredList.[0].[0] 
        else calculateRemove setsRunsList (lengthCheck+1)

    let mostValuable =  (GinRummy.checkSet cardList 1) @ (GinRummy.checkRun cardList 0) 
    let mostValuableDiscard =  GinRummy.getMostValuable 1 0 mostValuable

    let RemovedDiscardCard = calculateRemove mostValuableDiscard 1

    let flattenedRuns = mostValuableDiscard |> List.filter (fun (x:Card list) -> x.Length > 2) |> GinRummy.flatten

    cardList
    |> List.filter(fun x -> not((Set.ofList flattenedRuns).Contains x)) 
    |> List.filter (fun x -> not(x.Equals(RemovedDiscardCard)))
    |> GinRummy.GetCardScore

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
    //reduce the ammount of deadwood possible
    let card = Seq.head newHand
    (Continue, Some card)
    // Fixme: change function so that it computes which action the Computer should take: Continue, Knock or Gin 
    //        and which card would be best to discard

let calculatePossibleDeck (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) = 
    let removeDiscard = Seq.filter (fun x -> not (x.Equals topDiscard)) possibleDeck
    let hand = Set.ofSeq computerHand
    Seq.filter (fun x -> not (hand.Contains x)) removeDiscard
   
    