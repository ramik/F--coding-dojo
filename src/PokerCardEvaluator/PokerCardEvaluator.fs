module PokerCardEvaluator

open Cards
open System

type Shark = int option
type Duplicates = { value : int; shark : Shark }
type Hand = | OnlyShark of Shark 
            | FourOfKind of Duplicates
            | Pair of Duplicates
            | TwoPairs of Hand * Hand * Shark
            | ThreeOfKind of Duplicates
            | FullHouse of Hand * Hand
            | Straight of Shark
            | Flush of Shark
            | StraightFlush of Shark

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


let GetHighest (hand : seq<Card>) = hand |> Seq.sortBy (fun c -> (-1) * c.FaceValue) |> Seq.head |> (fun c -> c.FaceValue)

let EvaluateStraightAndflushes (hand : Card list) =
    let isFlush = hand |> Seq.windowed 2 |> Seq.forall (fun c -> c.[0].Suit = c.[1].Suit)
    let isStraight = 
          let ordered = hand |> List.sortBy (fun c -> c.FaceValue)
          let zipped = ordered |> Seq.skip 1 |> Seq.zip ordered |> Seq.forall (fun c -> (fst c).FaceValue + 1 = (snd c).FaceValue)
          match zipped, ordered with 
                    | true, _ -> (true, GetHighest hand)
                    | _, ValueCard(2, _) :: ValueCard(3, _) :: ValueCard(4, _) :: ValueCard(5, _) :: Ace(_) :: _ -> (true, 5)
                    | _ -> (false, 0)
    match isFlush, isStraight with | true, (true, x) -> Some(StraightFlush (Some(x)))
                                   | true, _ -> Some(Flush (Some(GetHighest hand)))
                                   | _, (true, x) -> Some(Straight(Some(x)))
                                   | _, _ -> None

let EvaluateHand hand = 
    let duplicates = EvaluatePairs hand
    let combinations = EvaluateStraightAndflushes hand
    match duplicates, combinations with | Some(x), _ -> x
                                        | _, Some(x) -> x
                                        | None, None -> OnlyShark (Some(GetHighest hand))


[<EntryPoint>]
let main args =
    let hand = (new Deck()).DrawHand
    printfn "Evaluating hand:"
    List.iter (printfn "%A") hand
    hand |> EvaluateHand |> printfn "%A"
    // Return 0. This indicates success.
    0