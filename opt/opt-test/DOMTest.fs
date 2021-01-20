module opt_test

open NUnit.Framework

let buildCFG (paths: (int * int list) list) : CFG.CFG =
    let entry = paths |> List.head |> fst |> uint
    let f (a, b) = uint a, b |> Seq.ofList |> Seq.map uint
    let ps = paths |> Seq.ofList |> Seq.map f
    CFG.ofPaths entry ps

[<Test>]
let DominanceFrontierTest () =
    // test case stolen from Hal Perkins
    let dom =
        [
            1, [2; 5; 9]
            2, [3]
            3, [3; 4]
            4, [13]
            5, [6; 7]
            6, [4; 8]
            7, [8; 12]
            8, [5; 13]
            9, [10; 11]
            10, [12]
            11, [12]
            12, [13]
            13, []
        ] |> buildCFG |> DOM.compute
    let f = fun n -> DOM.frontier (uint n) dom
    Assert.That(f 1, Is.EqualTo(Seq.empty))
    Assert.That(f 2, Is.EqualTo(Seq.ofList [4]))
    Assert.That(f 3, Is.EqualTo(Seq.ofList [3; 4]))
    Assert.That(f 4, Is.EqualTo(Seq.ofList [13]))
    Assert.That(f 5, Is.EqualTo(Seq.ofList [4; 5; 12; 13]))
    Assert.That(f 7, Is.EqualTo(Seq.ofList [8; 12]))
    Assert.That(f 8, Is.EqualTo(Seq.ofList [5; 13]))
    Assert.That(f 9, Is.EqualTo(Seq.ofList [12]))
    Assert.That(f 12, Is.EqualTo(Seq.ofList [13]))
    Assert.That(f 13, Is.EqualTo(Seq.empty))
    Assert.Pass()