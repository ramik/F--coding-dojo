module PokerCardEvaluator

open Cards
open System

let ShuffleCards (random : Random) = Seq.map (fun c -> (random.Next(), c)) >>
                                     Seq.sortBy (fun (a, _) -> a) >>
                                     Seq.map (fun (_, b) -> b)

let GetShuffledDeck deck = ShuffleCards (new System.Random()) deck
let GetHand = GetShuffledDeck >> Seq.take 5

type Shark = int
type Duplicates = { value : int; shark : Shark }
type Hand = | OnlyShark of Shark 
            | FourOfKind of Duplicates
            | Pair of Duplicates
            | TwoPairs of Hand * Hand
            | ThreeOfKind of Duplicates
            | FullHouse of Hand * Hand

let EvaluatePairs (hand : seq<Card>) =
       let sortedpairs = hand |> Seq.countBy (fun c -> c.FaceValue) |> Seq.sortBy (fun (b, a) -> -a * 20 - b) |> Seq.toList
       match sortedpairs with 
             | (a, b) :: (c, d) :: _ when b = 2 -> Some(Pair({ value = a; shark = c }))
             | _ -> None

let EvaluateHand (hand : seq<Card>) = 
    let eval = EvaluatePairs hand
    match eval with | Some(x) -> x
                    | None -> raise (NotImplementedException("only ponies"))                                  

GetHand Deck |> EvaluateHand |> ignore