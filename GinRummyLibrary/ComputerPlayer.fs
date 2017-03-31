module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

let rec calculateDiscard (setsRunsList:Card list list) lengthCheck = 
    let greater = GinRummy.flatten(List.filter (fun (x:Card list) -> (x.Length > (lengthCheck+1))) setsRunsList)
    let twoSectional = List.filter (fun x -> not (List.exists  (fun e -> (Set.ofList (greater)).Contains e) x) ) setsRunsList
    let filteredList = List.filter (fun (x:Card list) -> x.Length = lengthCheck ) twoSectional
    if (filteredList.Length > 0 ) then filteredList.[0].[0] 
    else calculateDiscard setsRunsList (lengthCheck+1)
    

let GetBestHandScore cardList = 
    
    let mostValuable =  (GinRummy.checkSet cardList 1) @ (GinRummy.checkRun cardList 0) 
    let RemovedDiscardCard = calculateDiscard mostValuable 1
    let r2 =  List.filter (fun x -> not(x.Equals(RemovedDiscardCard))) cardList
    let sequence = List.toSeq r2
    GinRummy.Deadwood sequence

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
        let mostValuable = (GinRummy.checkSet cards 1) @ (GinRummy.checkRun cards 0)  
        let Discard = calculateDiscard mostValuable 1
        //if score - discard = 0 then gin
        //if score < 10 run
        //else
        (Continue, Some Discard)//todo, knock
    // Fixme: change function so that it computes which action the Computer should take: Continue, Knock or Gin 
    //        and which card would be best to discard

let calculatePossibleDeck (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) = 
    let removeDiscard = Seq.filter (fun x -> not (x.Equals topDiscard)) possibleDeck
    let hand = Set.ofSeq computerHand
    Seq.filter (fun x -> not (hand.Contains x)) removeDiscard
   
    