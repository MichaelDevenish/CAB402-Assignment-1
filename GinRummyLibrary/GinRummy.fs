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

let Deadwood (hand:Hand) = 
    let handList = List.ofSeq hand
    
    //reduce handlist to only the  cards that arent in runs
    GetCardScore handList

    // Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    0//if there is any cards that the ai has that could make runs when the player knocks (add this to the GinRummy.score algorithim)
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// Add other functions related to Gin Rummy here ...