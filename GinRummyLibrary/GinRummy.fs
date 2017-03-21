module GinRummy

open Cards

//get score of supplied list of cards
let rec GetCardScore (arr:Card list) = 
     match arr with
     | [] -> 0
     | head::tail ->
        match head.rank with
        |Ace -> 1+(GetCardScore tail)
        |Two -> 2+(GetCardScore tail)
        |Three -> 3+(GetCardScore tail)
        |Four -> 4+(GetCardScore tail)
        |Five -> 5+(GetCardScore tail)
        |Six -> 6+(GetCardScore tail)
        |Seven -> 7+(GetCardScore tail)
        |Eight -> 8+(GetCardScore tail)
        |Nine -> 9+(GetCardScore tail)
        |Ten |Jack |Queen |King -> 10+(GetCardScore tail)

// Add helper functions to help compute Deadwood function

//sets
let rec checkSet (arr:Card list)=
    match arr with
    | [] -> [[]]
    | head::tail ->
        let filtered = List.filter (fun x -> x.rank = head.rank ) tail
        if filtered.Length > 1 then [[head]@filtered]@(checkSet tail)
        else (checkSet tail) 
    
//runs
let existsCheck current checkArray nextRank= 
    if (List.exists (fun x -> x = {suit=current.suit; rank=nextRank}) checkArray) then {suit=current.suit; rank=nextRank} else current

let nextExists (arr:Card list) (current:Card)  = 
        let next = existsCheck current arr
        match current.rank with
        |Ace -> next Two 
        |Two -> next Three 
        |Three -> next Four 
        |Four -> next Five
        |Five -> next Six
        |Six -> next Seven
        |Seven -> next Eight 
        |Eight -> next Nine
        |Nine -> next Ten
        |Ten -> next Jack
        |Jack -> next Queen 
        |Queen -> next King
        |King -> current

let rec checkRunLength (arr:Card list) first = 
    let next = nextExists arr first
    if (next.Equals(first)) then [next]
    else [first]@(checkRunLength arr next )

let rec checkRun (arr:Card list) index  = 
    if((arr.Length-1) < index) then [] else
        let filtered = List.filter(fun x -> x.suit = arr.[index].suit ) arr
        if filtered.Length > 1 then [(checkRunLength arr arr.[index])]@(checkRun arr (index+1))
        else (checkRun arr (index+1))

let Deadwood (hand:Hand) = 
    let handList = List.ofSeq hand
    let runs = List.filter (fun (x:Card list) -> x.Length > 2) (checkRun handList 0)
    let setsAndRuns = (checkSet handList) @ runs//dosent matter if repeats of runs since it will remove them since they are a lower score
    
    //foreach in each subarray see if that value is in another subarray if it is compare the score of those subarrays and remove the smallest
    //AKA find out if value exists twice in setsAndRuns if so find out which has higher value and remove other
    // remove result from handList
    GetCardScore handList

    // Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    0//if there is any cards that the ai has that could make runs when the player knocks (add this to the GinRummy.score algorithim)
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// Add other functions related to Gin Rummy here ...