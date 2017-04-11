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
let rec checkSet (cardList:Card list) index =
    if(index < cardList.Length ) then 
        let filtered = List.filter (fun x -> x.rank = cardList.[index].rank ) cardList

        if filtered.Length > 2 then [filtered] @ (checkSet cardList (index+1))
        else (checkSet cardList (index+1)) 
    else []

//runs
let nextExists (cardList:Card list) (current:Card)  =     
    let index = List.findIndex (fun x -> x = current.rank) AllRanks

    if (index < AllRanks.Length-1) && (List.exists (fun x -> x = {suit=current.suit; rank=AllRanks.[index+1]}) cardList) then
        {suit=current.suit; rank=AllRanks.[index+1]} 
    else current

let rec checkRun (cardList:Card list) index  = 
    let rec checkRunLength (arr:Card list) first = 
        let next = nextExists arr first
        if (next.Equals(first)) then [next]
        else [first]@(checkRunLength arr next)
    
    if((cardList.Length-1) < index) then [] else
        let filtered = List.filter(fun x -> x.suit = cardList.[index].suit ) cardList
        if filtered.Length > 1 then [(checkRunLength cardList cardList.[index])]@(checkRun cardList (index+1))
        else (checkRun cardList (index+1))

let rec expandRuns (cardList:Card list list) = 
    match cardList with 
    | [] -> []
    | head::tail -> 
        let shrunken =List.filter (fun x -> not (x = head.[head.Length-1])) head   
        if (head.Length > 2) then [shrunken] @ [head] @ expandRuns tail
        else [head] @ expandRuns tail

//getting most valuable sets/runs
let rec getMostValuable comparedIndex comparitorIndex (setsAndRuns:Card list list)= 
    let cleanRecursive addToTrue addToFalse (arr:Card list list) =  
         if((comparedIndex+1) > (arr.Length-1)) then 
            (getMostValuable (comparitorIndex+addToTrue+1) (comparitorIndex+addToTrue) arr)
         else (getMostValuable (comparedIndex+addToFalse) comparitorIndex arr)

    if (comparitorIndex >= (setsAndRuns.Length-1)) then setsAndRuns  
    else if(List.exists (fun x -> (Set.ofList (setsAndRuns.[comparitorIndex])).Contains x) (setsAndRuns.[comparedIndex])) then         
             if(((GetCardScore (setsAndRuns.[comparitorIndex])) > (GetCardScore (setsAndRuns.[comparedIndex]))) ) then
                cleanRecursive 1 -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(setsAndRuns.[comparedIndex]))) setsAndRuns)
             else cleanRecursive 0 -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(setsAndRuns.[comparitorIndex]))) setsAndRuns)
          else cleanRecursive 1 1 setsAndRuns           

//helper functions
let rec RemoveDuplicates list = 
    match list with 
        | [] -> []
        | head::tail -> [head] @  RemoveDuplicates(List.filter (fun x -> not (x.Equals(head))) tail)

//Main Functions
let Deadwood (hand:Hand) =    
    let handList = List.ofSeq hand
    let bestSetsAndRuns = 
        checkRun handList 0 |> expandRuns |> List.filter (fun (x:Card list) -> x.Length > 2) 
        |> combine (checkSet handList 0) |> List.sortBy (fun x -> GetCardScore x ) |> reverse |> RemoveDuplicates    
              
    let result = bestSetsAndRuns |> getMostValuable 1 0 |> flatten |> Set.ofList
    handList |> List.filter(fun x -> not(result.Contains x)) |> GetCardScore
 
let Score (firstOut:Hand) (secondOut:Hand) =
    let firstScore = Deadwood (List.ofSeq firstOut)
    let secondScore = Deadwood (List.ofSeq secondOut)
    let score = (secondScore - firstScore)

    if  firstScore = 0  then (secondScore + 25)
    else if(score >= 0 && not (firstScore = secondScore) ) then score
        else (score-25)

// Add other functions related to Gin Rummy here ...