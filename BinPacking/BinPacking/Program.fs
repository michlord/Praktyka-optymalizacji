// (0,0)-------------->x
//  | (x,y)__________
//  | |              |
//  | |______________| height
//  |  width
//  V y
type Size = {width : int; height : int}
type Rect = {x : int; y : int; width : int; height: int}


module List =
    let randomItem list = 
        let r = System.Random()
        let size = List.length list
        list.[r.Next(size)]

module Data =

    let private r = System.Random()

    let randomTypes amount =
        let randomSize minw minh maxw maxh =
            {Size.width=r.Next(minw, maxw); height=r.Next(minh, maxh)}
        [for x in 1..amount -> randomSize 1 1 20 20]

    let randomDisplacements amount =
        Array2D.init amount amount (fun _ _ -> r.Next(15))

    let randomData amount typesAmount =
        let types = randomTypes typesAmount
        let displacements = randomDisplacements amount
        let sizes = [for x in 1..amount -> types.[r.Next(typesAmount)]]
        (sizes, displacements, types)

module Util =
    let neighbourhood alpha =
        let size = List.length alpha
        let swappedAt i j =
            let copy = Array.ofList alpha
            let tmp = copy.[i]
            copy.[i] <- copy.[j]
            copy.[j] <- tmp
            List.ofArray copy
        seq {
            for i in 0..(size - 1) do
                for j in (i + 1)..(size - 1) ->
                    swappedAt i j
        }

    let dimensions (list : Rect list) =
        if List.isEmpty list then {Size.width=0; height=0}
        else
            let r = List.maxBy (fun {Rect.x=x; width=w} -> x + w) list
            let width = r.x + r.width
            let r = List.maxBy (fun {Rect.y=y; height=h} -> y + h) list
            let height = r.y + r.height
            {Size.width=width; height=height}

    let area (list : Rect list) =
        let {Size.width=w; height=h} = dimensions list
        w * h

    let isOverlaping (r1 : Rect) (r2 : Rect) =
        if r1.x + r1.width <= r2.x || r2.x + r2.width <= r1.x then false
        elif r1.y + r1.height <= r2.y || r2.y + r2.height <= r1.y then false
        else true

module Strategy =
    
    open Util

    let minArea (list : Rect list list) =
        List.minBy area list
        
    let minRatio (list : Rect list list) =
        let coef x =
            let {Size.width=w; height=h} = dimensions x
            abs (w-h)
        List.minBy coef list

    let private r = System.Random()

    let random (list : Rect list list) =
        let size = List.length list
        list.[r.Next(size)]

module Packing =
    
    

    let pack (alpha : Size list) (disp : int [,]) strategy (maxHeight : int) =
        
        let rec pack list (alpha : Size list) index =
            match alpha with
            | hd :: tl ->
                if hd.height > maxHeight then failwith "Too high input rectangle"
                else


                // For the currently inserted rectangle we have a temporary list of rectangles
                // that are wider by the amount specified by the displacement matrix.

                //let tmpList = list
                let tmpList =
                    List.mapi (fun i (r: Rect) -> 
                            let x = r.x-disp.[index,i]
                            if x < 0 then
                                {r with width=r.width+disp.[i,index]+disp.[index,i]+x; x=0}
                            else 
                                {r with width=r.width+disp.[i,index]+disp.[index,i]; x=x}
                        ) list

                // Try to find positions where you could insert a rectangle so that it would not overlap with
                // other rectangles and would not violate the displacement requirement.

                let positions =
                    let res = List.fold (fun acc r -> (r.x+r.width, r.y) :: (r.x, r.y+r.height) :: acc) [] tmpList                
                    if List.isEmpty res then [(0, 0)] else res


                // for each possition try to insert a rectangle and check if it isn't overlapping
                let possiblePositions =
                    List.filter (fun (x,y) -> 
                        List.forall (fun r -> 
                            let rect = {Rect.x=x; y=y; width=hd.width; height=hd.height}

                            rect.y + rect.height <= maxHeight &&
                            not (Util.isOverlaping rect  r)
                        ) tmpList
                    ) positions

                // What if this is empty? You can always make the area wider.
                // WHEN YOU HAVE SOMETHING LIKE THIS:
                //
                //  [] ------------------------> you won't have a 'position' to put a rect here
                //     []
                //    [   ]
                //  [            ]
                //                     [   ]
                //                [         ] --> only here but it will be too high
                //
                // Just force the rect furthest to the top right (implemented in let pick...)
                

                // Put the rect 'hd' in all possible positions in the 'list' (NOT tmpList).
                let candidates = 
                    List.map (fun (x,y) ->
                        {Rect.x=x; y=y; width=hd.width; height=hd.height} :: list
                    ) possiblePositions

                let pick =
                    if List.isEmpty candidates then
                        let {Size.width=w; height=h} = Util.dimensions list
                        {Rect.x=w; y=0; width=hd.width; height=hd.height} :: list
                    else 
                        strategy candidates

                pack pick tl (index+1)

            | [] -> list

        pack [] alpha 0
        
module Simulation =

    let private rnd = System.Random()

    let simmulatedAnnealing (alpha : Size list) (disp : int [,]) maxHeight strategy
                            (t0 : float) (tchange : float) titerchange titerret maxiter =
        
        let area (a : Size list) =
            Packing.pack a disp strategy maxHeight 
            |> Util.area

        let mutable iterCount = 0
        let mutable alpha = alpha
        let mutable best = alpha
        let mutable bestArea = area best
        let mutable t = t0

        while iterCount < maxiter do
            let beta = List.randomItem (Util.neighbourhood alpha |> List.ofSeq)
            let areaAlpha = area alpha
            let areaBeta = area beta
            
            if areaBeta  < areaAlpha then alpha <- beta
            else if System.Math.Pow(System.Math.E, (float (areaAlpha-areaBeta))/(float t)) < (float (rnd.Next(0,99))) / 100.0 then alpha <- beta
            if areaBeta < bestArea then
                best <- beta
                bestArea <- area best

            if iterCount % titerchange = 0 then t <- t * tchange
            if iterCount % titerret = 0 then t <- t0

            iterCount <- iterCount + 1

        (best, area best ,Packing.pack best disp strategy maxHeight)

            

module Test =
    

//    open System.Drawing 
//    open System.Windows.Forms
//
//    let draw res =
//        let frm = new Form()
//        frm.Paint.Add(fun e -> 
//            let p = new Pen(Color.Black, 1.0f)
//            List.iter (fun {x=x; y=y; width=w; height=h} -> 
//                let scale = 5
//                e.Graphics.DrawRectangle(p, x*scale,y*scale,w*scale,h*scale)
//                ) res
//            )
//
//        System.Windows.Forms.Application.Run(frm)
//
//    let testPack =
//        let (sizes, displacements, types) = Data.randomData 50 4
//        try
//            let x = Packing.pack sizes displacements Strategy.minRatio 150
//            draw x
//            ()
//        with
//            | e -> System.Console.WriteLine("exception: {0} ", e)
//        ()

    let testProgram amount s strategy =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()

        let (sizes, displacements, types) = Data.randomData amount s

        let (alpha, area, g) = Simulation.simmulatedAnnealing sizes displacements 160 strategy 125.0 0.98 100 500 5000
    
        let areaSum r =
            List.sumBy (fun {Size.width=w; height=h} -> w*h) r

        let precision =
            (float (areaSum sizes)) / (float area)

        let sides = 
            let {Size.width=w; height=h} = Util.dimensions g
            (min w h, max w h)

        let (shorter, longer) = sides
        let ratio = (float longer) / (float shorter)
    

        stopWatch.Stop()

        let elapsed = stopWatch.Elapsed.TotalSeconds

        System.Console.WriteLine("Typy: {0}, Dokładność: {1}, Pole obszaru: {2}, Krótszy bok: {3}, Dłuższy bok: {4}, Ratio: {5}, Czas: {6}",
            s, precision, area, shorter, longer, ratio, elapsed)

         


[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let run i s =
        Test.testProgram 50 i s
    let types = [3; 5; 8; 10; 12; 15; 20]
    System.Console.WriteLine("Strategia min. obszaru")
    for i in types do
        run i Strategy.minArea
    System.Console.WriteLine("Strategia współczynnika")
    for i in types do
        run i Strategy.minRatio
    System.Console.WriteLine("Strategia losowa")
    for i in types do
        run i Strategy.random
    
    0

