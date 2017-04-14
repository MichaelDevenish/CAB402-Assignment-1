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
let rec GetRun (cardList:Card list) startingPoint = 
    let index = List.findIndex (fun x -> x = startingPoint.rank) AllRanks
    let next =
        if (index < AllRanks.Length-1) 
            && (List.exists (fun x -> x = {suit=startingPoint.suit; rank=AllRanks.[index+1]}) cardList) then
            ({suit=startingPoint.suit; rank=AllRanks.[index+1]}) 
        else startingPoint

    if (next.Equals(startingPoint)) then [next]
    else [startingPoint]@(GetRun cardList next)

let rec GetRuns (cardList:Card list) index  =    
    if((cardList.Length-1) < index) then [] else
        let filtered = List.filter(fun x -> x.suit = cardList.[index].suit ) cardList
        if filtered.Length > 1 then [(GetRun cardList cardList.[index])]@(GetRuns cardList (index+1))
        else (GetRuns cardList (index+1))

let rec ExpandRuns (cardList:Card list list) = 
    match cardList with 
    | [] -> []
    | head::tail -> 
        let shrunken =List.filter (fun x -> not (x = head.[head.Length-1])) head   
        if (head.Length > 2) then [shrunken] @ [head] @ ExpandRuns tail
        else [head] @ ExpandRuns tail

//getting most valuable sets/runs
let rec getMostValuable comparedIndex comparitorIndex (setsAndRuns:Card list list)= 
    let ValuableProcess addToTrue addToFalse (setsAndRuns:Card list list) =  
         if((comparedIndex+1) > (setsAndRuns.Length-1)) then 
            (getMostValuable (comparitorIndex+addToTrue+1) (comparitorIndex+addToTrue) setsAndRuns)
         else (getMostValuable (comparedIndex+addToFalse) comparitorIndex setsAndRuns)

    if (comparitorIndex >= (setsAndRuns.Length-1)) then setsAndRuns  
    else if(List.exists (fun x -> (Set.ofList (setsAndRuns.[comparitorIndex])).Contains x) (setsAndRuns.[comparedIndex])) then         
             if(((GetCardScore (setsAndRuns.[comparitorIndex])) > (GetCardScore (setsAndRuns.[comparedIndex]))) ) then
                ValuableProcess 1 -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(setsAndRuns.[comparedIndex]))) setsAndRuns)
             else ValuableProcess 0 -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(setsAndRuns.[comparitorIndex]))) setsAndRuns)
          else ValuableProcess 1 1 setsAndRuns           

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
              
    let scoreRemovedCards = bestSetsAndRuns |> getMostValuable 1 0 |> flatten |> Set.ofList
    handList |> List.filter(fun x -> not(scoreRemovedCards.Contains x)) |> GetCardScore
 
let Score (firstOut:Hand) (secondOut:Hand) =
    let firstScore = Deadwood (List.ofSeq firstOut)
    let secondScore = Deadwood (List.ofSeq secondOut)
    let score = (secondScore - firstScore)

    if  firstScore = 0  then (secondScore + 25)
    else if(score >= 0 && not (firstScore = secondScore) ) then score
        else (score-25)

// Add other functions related to Gin Rummy here ...