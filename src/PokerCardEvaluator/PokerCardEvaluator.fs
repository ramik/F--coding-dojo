module PokerCardEvaluator

open Cards
open System

let ShuffleCards (random : Random) = Seq.map (fun c -> (random.Next(), c)) >>
                                     Seq.sortBy (fun (a, _) -> a) >>
                                     Seq.map (fun (_, b) -> b)

