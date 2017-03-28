module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue
    


let GetBestHandScore cardList = 
    let rec calculateRemove (setsRunsList:Card list list) lengthCheck = 
        let filteredList = List.filter (fun (x:Card list) -> x.Length = lengthCheck ) setsRunsList
        if (filteredList.Length > 0 ) then filteredList.[0].[0] 
        else calculateRemove setsRunsList (lengthCheck+1)

    let mostValuableDiscard = 
        (GinRummy.checkSet cardList) @ (GinRummy.checkRun cardList 0) 
        |> GinRummy.getMostValuable 1 0

    let RemovedDiscardCard = calculateRemove mostValuableDiscard 1

    let flattenedRuns = mostValuableDiscard |> List.filter (fun (x:Card list) -> x.Length > 2) |> GinRummy.flatten

    cardList
    |> List.filter(fun x -> not((Set.ofList flattenedRuns).Contains x)) 
    |> List.filter (fun x -> not(x.Equals(RemovedDiscardCard)))
    |> GinRummy.GetCardScore

let ComputerPickupDiscard (computerHand:Hand) (topDiscard:Card) (possibleDeck:Deck) =

    //calculate the deadwood for picking up the discard card and removing a card that isnt near a run
    //and the possible deadwood for taking a run card  doing so and do the lower

    //GET THE DISCARD SCORE//    
    let DiscardScore = GetBestHandScore (List.ofSeq computerHand @ [topDiscard])
                                   
    //GET THE POSSIBLE RUN SCORE//
    //do above for each possible card in the possibleDeck and get average (do using recursive function that returns list of each best hand)


    true
    // Fixme: change function so that it computes if Computer should pickup from Discard pile 
    //        or draw a fresh card from the deck

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
   
    