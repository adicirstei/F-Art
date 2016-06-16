
let private swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

// shuffle an array (in-place)
let shuffle (rand:System.Random) (a:'T []) : ('T [])  =
    Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a
    a

let floatRand (rand:System.Random) min max =
    let r = rand.NextDouble () * (max - min)
    min +  r

let randomSphere (rand:System.Random) radius = 
    let a = 2.0 * System.Math.PI * rand.NextDouble()
    (radius * System.Math.Cos a, radius * System.Math.Sin a)