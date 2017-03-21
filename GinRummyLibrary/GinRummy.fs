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

//getting most efficient
let share (a:Card list) (b:Card list) = 
    List.exists (fun x ->(Set.ofList a).Contains x) b

let rec contains (arr:Card list list) comparedIndex comparitorIndex = 
    if (comparitorIndex >= (arr.Length-1)) then  arr  
    else if(share (arr.[comparitorIndex]) (arr.[comparedIndex])) then 
             if((GetCardScore (arr.[comparitorIndex])) > (GetCardScore (arr.[comparedIndex]))) then
                  let shrunkenList = (List.filter (fun x -> not (x.Equals(arr.[comparedIndex]))) arr)
                  if((comparedIndex+1) > (shrunkenList.Length-1)) then 
                      (contains shrunkenList (comparitorIndex+2) (comparitorIndex+1))
                  else (contains shrunkenList comparedIndex comparitorIndex)
             else let shrunkenList = (List.filter (fun x -> not (x.Equals(arr.[comparitorIndex]))) arr)
                  if((comparedIndex+1) > (shrunkenList.Length-1)) then 
                      (contains shrunkenList (comparitorIndex+1) (comparitorIndex))
                  else (contains shrunkenList comparedIndex comparitorIndex)
        else if((comparedIndex+1) > (arr.Length-1)) then (contains arr (comparitorIndex+2) (comparitorIndex+1))
        else (contains arr (comparedIndex+1) comparitorIndex)
           
let rec flatten l =
    match l with 
    | [] -> []
    | head::tail -> head @ flatten tail

let forgetPickup (hand:Hand) = 
    if( Seq.length hand > 10) then 
        List.ofSeq (Seq.take 10 hand)
        else List.ofSeq hand

let Deadwood (hand:Hand) = 
    let handList = forgetPickup hand
    let runs = List.filter (fun (x:Card list) -> x.Length > 2) (checkRun handList 0)
    let setsAndRuns = (checkSet handList) @ runs
    let bestSetsAndRuns = flatten(contains setsAndRuns 1 0)
    let filtered = List.filter(fun x -> not((Set.ofList bestSetsAndRuns).Contains x)) handList
    GetCardScore filtered

let Score (firstOut:Hand) (secondOut:Hand) =
    0//if there is any cards that the ai has that could make runs when the player knocks (add this to the GinRummy.score algorithim)
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// Add other functions related to Gin Rummy here ...