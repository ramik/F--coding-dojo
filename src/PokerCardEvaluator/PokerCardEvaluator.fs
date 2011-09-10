module PokerCardEvaluator

open Cards
open System

let ShuffleCards (random : Random) = Seq.map (fun c -> (random.Next(), c)) >>
                                     Seq.sortBy (fun (a, _) -> a) >>
                                     Seq.map (fun (_, b) -> b)

let GetShuffledDeck deck = ShuffleCards (new System.Random()) deck
let GetHand = GetShuffledDeck >> Seq.take 5

type Shark = int option
type Duplicates = { value : int; shark : Shark }
type Hand = | OnlyShark of Shark 
            | FourOfKind of Duplicates
            | Pair of Duplicates
            | TwoPairs of Hand * Hand * Shark
            | ThreeOfKind of Duplicates
            | FullHouse of Hand * Hand

let EvaluatePairs (hand : seq<Card>) =
       let sortedpairs = hand |> Seq.countBy (fun c -> c.FaceValue) |> Seq.sortBy (fun (b, a) -> -a * 20 - b) |> Seq.toList
       match sortedpairs with 
             | (firstValue, countA) :: (secondValue, countB) :: (thirdValue, _) :: _ when countA = 2 && countB = 2 ->
                           Some(TwoPairs(Pair({ value = firstValue; shark = None }), 
                                         Pair({ value = secondValue; shark = None }), Some(thirdValue)))
             | (firstValue, count) :: (secondValue, _) :: _ when count = 2 -> Some(Pair({ value = firstValue; shark = Some(secondValue) }))
             | (firstValue, countA) :: (secondValue, countB) :: _ when countA = 3 && countB = 2 ->
                           Some(FullHouse(ThreeOfKind({ value = firstValue; shark = None }), 
                                          Pair({ value = secondValue; shark = None })))
             | (firstValue, count) :: (secondValue, _) :: _ when count = 3 -> Some(ThreeOfKind({ value = firstValue; shark = Some(secondValue) }))
             | (firstValue, count) :: (secondValue, _) :: _ when count = 4 -> Some(FourOfKind({ value = firstValue; shark = Some(secondValue) }))
             | _ -> None

let EvaluateHand (hand : seq<Card>) = 
    let eval = EvaluatePairs hand
    match eval with | Some(x) -> x
                    | None -> raise (NotImplementedException("only ponies"))                                  

GetHand Deck |> EvaluateHand |> ignore