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
let rec checkSet (arr:Card list) index =
    if(index < arr.Length ) then 
        let filtered = List.filter (fun x -> x.rank = arr.[index].rank ) arr
        if filtered.Length > 2 then [filtered] @ (checkSet arr (index+1))
        else (checkSet arr (index+1)) 
    else 
        []

//runs
let nextExists (arr:Card list) (current:Card)  = 
    let existsCheck current checkArray nextRank= 
        if (List.exists (fun x -> x = {suit=current.suit; rank=nextRank}) checkArray) then
            {suit=current.suit; rank=nextRank} 
            else current

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

let rec checkRun (arr:Card list) index  = 
    let rec checkRunLength (arr:Card list) first = 
        let next = nextExists arr first
        if (next.Equals(first)) then [next]
        else [first]@(checkRunLength arr next)
    
    if((arr.Length-1) < index) then [] else
        let filtered = List.filter(fun x -> x.suit = arr.[index].suit ) arr
        if filtered.Length > 1 then [(checkRunLength  arr arr.[index])]@(checkRun arr (index+1))
        else (checkRun arr (index+1))

let rec expandRuns (arr:Card list list) = 
    match arr with 
    | [] -> []
    | head::tail -> 
        let shrunken =List.filter (fun x -> not (x = head.[head.Length-1])) head   
        if (head.Length > 2) then [shrunken] @ [head] @ expandRuns tail
        else [head] @ expandRuns tail

//getting most valuable sets/runs
let rec getMostValuable comparedIndex comparitorIndex (arr:Card list list)= 
    let cleanRecursive addToTrue addToFalse (arr:Card list list) =  
         if((comparedIndex+1) > (arr.Length-1)) then 
            (getMostValuable (comparitorIndex+addToTrue+1) (comparitorIndex+addToTrue) arr)
         else (getMostValuable (comparedIndex+addToFalse) comparitorIndex arr)

    if (comparitorIndex >= (arr.Length-1)) then arr  
    else if(List.exists (fun x -> (Set.ofList (arr.[comparitorIndex])).Contains x) (arr.[comparedIndex])) then         
             if(((GetCardScore (arr.[comparitorIndex])) > (GetCardScore (arr.[comparedIndex]))) ) then
                    cleanRecursive 1 -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(arr.[comparedIndex]))) arr)
             else 
                cleanRecursive 0 -(comparedIndex-1) (List.filter (fun x -> not (x.Equals(arr.[comparitorIndex]))) arr)
          else cleanRecursive 1 1 arr           

let rec RemoveDuplicates list = 
    match list with 
        | [] -> []
        | head::tail -> [head] @  RemoveDuplicates(List.filter (fun x -> not (x.Equals(head))) tail)

let rec flatten l =
    match l with 
    | [] -> []
    | head::tail -> head @ flatten tail

let Deadwood (hand:Hand) = 
    let combine a b = 
        a @ b

    let rec reverse l = 
        match l with 
        | [] -> []
        | head::tail -> (reverse tail) @ [head]
    
    let handList = List.ofSeq hand
    let bestSetsAndRuns = 
        checkRun handList 0
        |> expandRuns
        |> List.filter (fun (x:Card list) -> x.Length > 2)
        |> combine (checkSet handList 0)
        |> List.sortBy (fun x -> GetCardScore x )
        |> reverse
        |> RemoveDuplicates          
    let result = 
        bestSetsAndRuns
        |> getMostValuable 1 0
        |> flatten 
        |> Set.ofList

    (List.filter(fun x -> not(result.Contains x)) handList)
    |> GetCardScore
 

let Score (firstOut:Hand) (secondOut:Hand) =
    //if there is any cards that the ai has that could make runs when the player knocks (add this to the GinRummy.score algorithim)
    let firstScore = Deadwood (List.ofSeq firstOut)
    let secondScore = Deadwood (List.ofSeq secondOut)
    let score = (secondScore - firstScore)

    if  firstScore = 0  then (secondScore + 25)
    else if(score >= 0 && not (firstScore = secondScore) ) then score
        else (score-25)

// Add other functions related to Gin Rummy here ...