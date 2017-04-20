module GinRummy

open Cards

//get score of supplied list of cards
let rec GetCardScore (cardList:Card list) = 
    if(cardList.Length > 0) then
        let index = List.findIndex (fun x -> x = cardList.Head.rank) AllRanks
        if index < 9 then (index+1)+(GetCardScore cardList.Tail)
        else 10 + (GetCardScore cardList.Tail)
    else 0

//sets
let rec GetSets (cardList:Card list) index =
    if(index < cardList.Length ) then 
        let filtered = List.filter (fun x -> x.rank = cardList.[index].rank ) cardList
        if filtered.Length > 2 then [filtered] @ (GetSets cardList (index+1))
        else (GetSets cardList (index+1)) 
    else []


//runs
let GetNextInRun index (cardList:Card list) currentCard = 
    if (index < AllRanks.Length-1) && (List.exists (fun x -> x = {suit=currentCard.suit; rank=AllRanks.[index+1]}) cardList) then
        Some ({suit=currentCard.suit; rank=AllRanks.[index+1]}) 
    else None

let rec GetRun (cardList:Card list) currentCard = 
    match GetNextInRun (List.findIndex (fun x -> x = currentCard.rank) AllRanks) cardList currentCard with
    | None -> [currentCard]
    | Some card -> [currentCard]@(GetRun cardList card)

let rec GetRuns (cardList:Card list) index  =    
    if((cardList.Length-1) < index) then [] else
        if (List.filter(fun x -> x.suit = cardList.[index].suit) cardList).Length > 1 
            then [(GetRun cardList cardList.[index])]@(GetRuns cardList (index+1))
        else (GetRuns cardList (index+1))

let rec ExpandRuns (cardList:Card list list) = 
    match cardList with 
    | [] -> []
    | head::tail -> 
        let shrunken =List.filter (fun x -> not (x = head.[head.Length-1])) head   
        if (head.Length > 2) then [shrunken] @ [head] @ ExpandRuns tail
        else [head] @ ExpandRuns tail

//getting most valuable sets/runs
let rec GetMostValuable comparedIndex comparitorIndex (setsAndRuns:Card list list)= 
    let RecursionChoice addToTrue addToFalse (setsAndRuns:Card list list) =  
        let GetMostValuableCall = (fun x y -> GetMostValuable x (comparitorIndex+y) setsAndRuns)         
        if((comparedIndex+1) > (setsAndRuns.Length-1)) then GetMostValuableCall (comparitorIndex+addToTrue+1) addToTrue    
        else GetMostValuableCall (comparedIndex+addToFalse) 0          

    if (comparitorIndex >= (setsAndRuns.Length-1)) then setsAndRuns  
    else if(List.exists (fun x -> (Set.ofList (setsAndRuns.[comparitorIndex])).Contains x) (setsAndRuns.[comparedIndex])) then         
            let InnerRecursion index = 
                (fun y -> RecursionChoice y -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(setsAndRuns.[index]))) setsAndRuns))
            if(((GetCardScore (setsAndRuns.[comparitorIndex])) > (GetCardScore (setsAndRuns.[comparedIndex])))) 
                then InnerRecursion comparedIndex 1
            else InnerRecursion comparedIndex 0
        else RecursionChoice 1 1 setsAndRuns           

//helper functions
let rec RemoveDuplicates list = 
    match list with 
        | [] -> []
        | head::tail -> [head] @  RemoveDuplicates(List.filter (fun x -> not (x.Equals(head))) tail)

//Main Functions
let Deadwood (hand:Hand) =    
    let handList = List.ofSeq hand
    let bestSetsAndRuns = 
        GetRuns handList 0 |> ExpandRuns |> List.filter (fun (x:Card list) -> x.Length > 2) 
        |> combine (GetSets handList 0) |> List.sortBy (fun x -> GetCardScore x ) |> reverse |> RemoveDuplicates    
              
    let scoreRemovedCards = bestSetsAndRuns |> GetMostValuable 1 0 |> flatten |> Set.ofList
    handList |> List.filter(fun x -> not(scoreRemovedCards.Contains x)) |> GetCardScore
 
let Score (firstOut:Hand) (secondOut:Hand) =
    let firstScore = Deadwood firstOut
    let secondScore = Deadwood secondOut
    let score = (secondScore - firstScore)

    if firstScore = 0  then (secondScore + 25)
    else if(score >= 0 && not (firstScore = secondScore) ) then score
        else (score-25)

// Add other functions related to Gin Rummy here ...