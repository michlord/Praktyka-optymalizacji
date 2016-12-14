
//rect is represented as (width, height)
let inputData typesAmount rectsAmount maxHeight maxWidth maxDisplacement =
    let r = System.Random()
    let types = [|for x in 1..typesAmount -> (r.Next(1, maxWidth+1), r.Next(1, maxHeight+1))|]
    let rectangles = [|for x in 1..rectsAmount -> types.[r.Next(0,typesAmount)]|]
    let displacements = Array2D.init rectsAmount rectsAmount (fun x y -> r.Next(1, maxDisplacement))
    (rectangles, displacements)




// g - area of rectangles (x,y,w,h,i)
// this is how we interpret the rectangles and coordinates
// (0,0)-------------->
//  | (left, top)____
//  | |              |
//  | |______________| height
//  |  width
//  V

type Rect = {left : int; top : int; width : int; height : int; index : int}
// or if you want to make your life easier...
// let rect l t w h i = {left = l; top = t; width = w; height = h; index = i}






let insert (g : Rect list) (rect : Rect) d m =
    if m < rect.top+rect.height then None
    else
        // find rectangles that come before 'rect'.
        let pred (g : Rect list) (r : Rect) =

            let rec pred g high low =
                match g with
                | hd :: tl ->
                    let t = max hd.top low
                    let b = min (hd.top+hd.height) high
                    if b <= low || t >= high 
                        then pred tl high low
                    elif t = low && b = high
                        then [hd]
                    elif t = low 
                        then hd :: (pred tl high b)
                    elif b = high
                        then hd :: (pred tl t low)
                    else
                        [hd] @ (pred tl high b) @ (pred tl t low)
            
                | [] -> []
    
            let l = List.sort g
                    |> List.takeWhile (fun {left=l} -> l < r.left + r.width)
                    |> List.rev
   
            pred l (r.top+r.height) r.top

        // find min left coordinate where 'rect' could be placed based on d (displacement) values.
        let minLeft = List.fold (fun acc {left=l; width=w; index=i} -> max acc (l+w+(d i rect.index))) 0 (pred g rect)
        // check if the displaced 'rect' doesn't intersect with some other rectangle
        let rect = {rect with left=minLeft}
        // when checking if two rects overlap take d into consideration
        let isOverlaping (r1 : Rect) (r2 : Rect) =
            if r1.left + r1.width + (d r1.index r2.index) <= r2.left ||
               r2.left + r2.width + (d r2.index r1.index) <= r1.left then false
            else if r1.top + r1.height <= r2.top || r2.top + r2.height <= r1.top then false
            else true
            

        if List.exists (isOverlaping rect) g
            then None
            else Some (rect::g, rect)

let getR (r : (int*int) []) i =
    r.[i]

let getD (d : int [,]) i j =
    d.[i,j]

// calculate area from g
let area g =
    let maxh = List.maxBy (fun {top=t; height=h} -> t+h) g
    let maxw = List.maxBy (fun {left=l; width=w} -> l+w) g
    (maxh.top+maxh.height)*(maxw.left+maxw.width)
          
let minAreaStrategy xss =
    List.minBy (fun (g, _) -> area g) xss

let randomStrategy (xss : (Rect list * 'a) list) =
    let r = System.Random()
    let shuffleR (r : System.Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())
    xss |> Seq.ofList |> shuffleR r |> Seq.head

let ratioStrategy xss =
    let coeff g =
        let maxh = List.maxBy (fun {top=t; height=h} -> t+h) g
        let maxw = List.maxBy (fun {left=l; width=w} -> l+w) g    
        abs (maxh.top+maxh.height - maxw.left+maxw.width)
    List.minBy (fun (g, _) -> (area g)*(coeff g)) xss

let pack r alpha d m strategy =
    let rect left top i =
        let (w, h) = getR r i
        {left=left; top=top; width=w; height=h; index=i}
    let disp i j =
        getD d i j
    let rec pack g p alpha =
        match alpha with
        | (x::xs) -> 
            let o = List.choose (fun (l,t) -> insert g (rect l t x) disp m ) p
            
            if o = [] then []
            else
                let (g, r) = strategy o

                let p = List.filter (fun (l,t) -> l <> r.left || t <> r.top) p
                let p = (r.left+r.width,r.top) :: (r.left,r.top+r.height) :: p

                pack g p xs
        | [] -> g
    pack [] [(0,0)] alpha


// neighbourhood contains n^2 permuations
// of alpha where in each permutation
// a single pair of elements is swapped
let neighbourhood alpha  =
    let last = List.length alpha - 1
    seq {for i in 0..last do 
            for j in (i+1)..last -> 
                let copy = Array.ofList alpha
                let tmp = copy.[i]
                copy.[i] <- copy.[j]
                copy.[j] <- tmp
                List.ofArray copy
        }


let simmulatedAnnealing r d m s a (t0 : float) (tchange : float) titerchange titerret maxiter =
    let ar x = area (pack r x d m s)
    let rnd = System.Random()
    let shuffleR (r : System.Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())

    let mutable iterCount = 0

    let mutable alpha = a
    let mutable best = a
    let mutable n = shuffleR rnd (neighbourhood alpha)
    let mutable t = t0

    while iterCount < maxiter do
        let beta = Seq.tryHead n
        match beta with
        | Some beta ->
            n <- Seq.tail n // It could fail here...
            if (ar beta) < (ar alpha) then alpha <- beta
            else if System.Math.Pow(System.Math.E, (float ((ar alpha)-(ar beta)))/(float t)) < (float (rnd.Next(0,99))) / 100.0
            then alpha <- beta
            if (ar beta) < (ar best) then best <- beta
        | None -> () // error, this could happen but I don't know what to do with it yet

        if iterCount % titerchange = 0 then t <- t * tchange
        if iterCount % titerret = 0 then t <- t0

        iterCount <- iterCount + 1

    (best, ar best, pack r best d m s)




let test : Rect list = 
    let (r, d) = inputData 4 10 10 20 4
    let a = [for x in 0..9 -> x]
    let s = minAreaStrategy
    let (alpha, area, g) = simmulatedAnnealing r d 18 s a 125.0 0.98 100 500 5000
    g 


let testSuite types size strategy =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let (r, d) = inputData types size 10 20 4
    let alpha = [for x in 0..(size-1) -> x]
    let (alpha, area, g) = simmulatedAnnealing r d 18 strategy alpha 125.0 0.98 100 500 5000
    
    let areaSum r =
        Array.sumBy (fun (w, h) -> w*h) r

    let precision =
        (float (areaSum r)) / (float area)

    let sides = 
        let maxh = List.maxBy (fun {top=t; height=h} -> t+h) g
        let maxw = List.maxBy (fun {left=l; width=w} -> l+w) g
        let height = maxh.top+maxh.height
        let width = maxw.left+maxw.width
        (min width height, max width height)

    let (shorter, longer) = sides
    let ratio = (float longer) / (float shorter)
    

    stopWatch.Stop()

    let elapsed = stopWatch.Elapsed.TotalSeconds

    System.Console.WriteLine("Typy: {0}, Dokładność: {1}, Pole obszaru: {2}, Krótszy bok: {3}, Dłuższy bok: {4}, Ratio: {5}, Czas: {6}",
        types, precision, area, shorter, longer, ratio, elapsed)





//let test = 
//    let (rectangles, displacements) = inputData 4 10 10 20 4
//    let alpha = [for x in 0..9 -> x]
//    let strategy = minAreaStrategy
//    pack rectangles alpha displacements 18 strategy




open System.Drawing 
open System.Windows.Forms

[<EntryPoint>]
let main argv =
    let run i s =
        try
            testSuite i 20 s
        with
            | _ -> System.Console.WriteLine("fail")

    let types = [3; 5; 8; 10; 12; 15; 20]
    System.Console.WriteLine("Strategia min. obszaru")
    for i in types do
        run i minAreaStrategy
    System.Console.WriteLine("Strategia współczynnika")
    for i in types do
        run i ratioStrategy
    System.Console.WriteLine("Strategia losowa")
    for i in types do
        run i randomStrategy
    
    0


//[<EntryPoint>]
//let main argv = 
//    printfn "%A" argv
//    let res = test
//    let frm = new Form()
//    frm.Paint.Add(fun e -> 
//        let p = new Pen(Color.Black, 1.0f)
//        List.iter (fun {left=l; top=t; width=w; height=h} -> 
//            let scale = 5
//            e.Graphics.DrawRectangle(p, l*scale,t*scale,w*scale,h*scale)
//            ) res
//    )
//
//    System.Windows.Forms.Application.Run(frm)
//    0 // return an integer exit code
