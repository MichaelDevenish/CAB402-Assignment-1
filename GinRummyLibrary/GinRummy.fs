module GinRummy

open Cards

// Add helper functions to help compute Deadwood function

let Deadwood (hand:Hand) = 
    Seq.length hand
    // Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    0
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// Add other functions related to Gin Rummy here ...