module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

let rec calculateDiscard lengthCheck (setsRunsList:Card list list)  = 

    let greater = 
        setsRunsList
        |> List.filter (fun x -> (x.Length > (lengthCheck))) 
        |> GinRummy.flatten
        |> Set.ofList
    
    let filteredList = 
        setsRunsList    
        |> List.filter (fun x -> not(List.exists (fun e -> greater.Contains e)x))
     
    if (filteredList.Length > 0 ) then
        filteredList.[0].[0] 
    else (calculateDiscard (lengthCheck+1) setsRunsList)
    
let GetRemovedDeadwood cardList removed =   
    cardList 
    |> List.filter (fun x -> not(x.Equals(removed))) 
    |> List.toSeq 
    |> GinRummy.Deadwood

let GetBestHandScore cardList = 
    let set =  (GinRummy.checkSet cardList 1) @ (GinRummy.checkRun cardList 0) 
    
    let discard = calculateDiscard 1 set
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
        let Discard = 
            (GinRummy.checkSet cards 1) @ (GinRummy.checkRun cards 0) 
            |> calculateDiscard 1
        let score = GetRemovedDeadwood cards Discard

        if(score = 0) then (Gin, Some Discard)
        else if(score <= 10) then (Knock, Some Discard)
        else (Continue, Some Discard)
            
let calculatePossibleDeck (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) = 
    let removeDiscard = Seq.filter (fun x -> not (x.Equals topDiscard)) possibleDeck
    let hand = Set.ofSeq computerHand
    Seq.filter (fun x -> not (hand.Contains x)) removeDiscard
   
    