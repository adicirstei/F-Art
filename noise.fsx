#load "random.fsx"

let private f2 : float = 0.5 * ((sqrt 3.0) - 1.0)
let private g2 : float = (3.0 - sqrt 3.0) / 6.0
let private f3 : float = 1.0 / 3.0
let private g3 : float = 1.0 / 6.0
let private f4 : float = ((sqrt 5.0) - 1.0) / 4.0
let private g4 : float = (5.0 - (sqrt 5.0)) / 20.0
let private get = Array.get
//let private reverseArray = Array.rev


let private generatePermMod12 =  Array.map (fun i -> i % 12) 


let private grad3 = [|
  1.0; 1.0; 0.0; -1.0; 1.0; 0.0;0.0; 1.0; -1.0; 0.0;
  -1.0; -1.0; 0.0; 1.0; 0.0; 1.0; -1.0; 0.0; 1.0;
  1.0; 0.0; -1.0; -1.0; 0.0; -1.0; 0.0; 1.0; 1.0;
  0.0; -1.0; 1.0; 0.0; 1.0; -1.0; 0.0; -1.0; -1.0|]

let private grad4 = [|
    0.0; 1.0; 1.0; 1.0; 0.0; 1.0; 1.0; -1.0; 0.0; 1.0; -1.0; 1.0; 0.0; 1.0; -1.0; -1.0;
    0.0; -1.0; 1.0; 1.0; 0.0; -1.0; 1.0; -1.0; 0.0; -1.0; -1.0; 1.0; 0.0; -1.0; -1.0; -1.0;
    1.0; 0.0; 1.0; 1.0; 1.0; 0.0; 1.0; -1.0; 1.0; 0.0; -1.0; 1.0; 1.0; 0.0; -1.0; -1.0;
    -1.0; 0.0; 1.0; 1.0; -1.0; 0.0; 1.0; -1.0; -1.0; 0.0; -1.0; 1.0; -1.0; 0.0; -1.0; -1.0;
    1.0; 1.0; 0.0; 1.0; 1.0; 1.0; 0.0; -1.0; 1.0; -1.0; 0.0; 1.0; 1.0; -1.0; 0.0; -1.0;
    -1.0; 1.0; 0.0; 1.0; -1.0; 1.0; 0.0; -1.0; -1.0; -1.0; 0.0; 1.0; -1.0; -1.0; 0.0; -1.0;
    1.0; 1.0; 1.0; 0.0; 1.0; 1.0; -1.0; 0.0; 1.0; -1.0; 1.0; 0.0; 1.0; -1.0; -1.0; 0.0;
    -1.0; 1.0; 1.0; 0.0; -1.0; 1.0; -1.0; 0.0; -1.0; -1.0; 1.0; 0.0; -1.0; -1.0; -1.0; 0.0|]


//addUp : List Bool -> Int
let private addUp (bs:bool list) : int = bs |> List.filter id |> List.length

let private getCornerOffset2d x y : int*int=
  if (x > y) then
    (1, 0)
  else
    (0, 1)


let private getCornerOffset3d x y z : int*int*int*int*int*int =
  if (x >= y) then
    if (y >= z) then
      (1, 0, 0, 1, 1, 0)
    else if (x >= z) then
      (1, 0, 0, 1, 0, 1)
    else
      (0, 0, 1, 1, 0, 1)
  else
    if (y < z) then
      (0, 0, 1, 0, 1, 1)
    else if (x < z) then
      (0, 1, 0, 0, 1, 1)
    else
      (0, 1, 0, 1, 1, 0)




let private getN2d (x:float) (y:float) (i:int) (j:int) (perm:int array) (permMod12:int array) : float =
  let t = 0.5 - x * x - y * y
  if (t < 0.0) then
    0.0
  else
    let gi = (get permMod12 (i + get perm j)) * 3
    let t' = t * t
    t' * t' * ((get grad3 gi ) * x + (get grad3 (gi + 1)) * y)


let private getN3d (x:float) (y:float) (z:float) (i:int) (j:int) (k:int)  (perm:int array) (permMod12:int array) : float =
  let t = 0.6 - x * x - y * y - z * z
  if (t < 0.0) then
      0.0
  else
      let gi = (get permMod12 (i + get perm (j + get perm k))) * 3
      let t'= t * t
      t' * t' * ((get grad3 gi) * x + (get grad3 (gi + 1)) * y + (get grad3 (gi + 2)) * z)


let private getN4d (x:float) (y:float) (z:float) (w:float) (i:int) (j:int) (k:int) (l:int)  (perm:int array) (permMod12:int array) : float =
  let t = 0.6 - x * x - y * y - z * z - w * w
  if (t < 0.0) then
      0.0
  else
      let gi = ((get perm i + (get perm j + (get perm k + (get perm l)))) % 32) * 4
      let t' = t * t
      t' * t' * ((get grad4 gi) * x + (get grad4 (gi + 1)) * y + (get grad4 (gi + 2)) * z + (get grad4 (gi + 3)) * w)


(***
Permutation table that is needed to generate the noise value.
***)
type PermutationTable = {
    perm: int array 
    permMod12: int array}




(***
Genrate the permutation tables that are needed to calculate the noise value.
The function takes a seed and returns the table and  a new seed.
***)

let permutationTable (rand:System.Random) : PermutationTable =
  let perm = [|0..255|] |> Random.shuffle rand |> (fun list -> Array.append list  (Array.rev list))
  {perm = perm; permMod12 = generatePermMod12 perm}


(***
Generates a noise value between `-1` and `1` based on the given x and y value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
***)


let noise2d ({perm=perm; permMod12=permMod12}:PermutationTable) (xin:float) (yin:float) : float =
  let s = (xin + yin) * f2
  let i = int <| floor (xin + s)
  let j = int <| floor (yin + s)
  let t = float (i + j) * g2
  let x0' = (float i) - t
  let y0' = (float j) - t
  let x0 = xin - x0'
  let y0 = yin - y0'
  let (i1, j1) = getCornerOffset2d x0 y0
  let x1 = x0 - (float i1) + g2
  let y1 = y0 - (float j1) + g2
  let x2 = x0 - 1.0 + 2.0 * g2
  let y2 = y0 - 1.0 + 2.0 * g2

  let ii =  i &&& 255
  let jj =  j &&& 255

  let n0 = getN2d x0 y0 ii jj perm permMod12
  let n1 = getN2d x1 y1 (ii + i1)  (jj + j1) perm permMod12
  let n2 = getN2d x2 y2 (ii + 1)  (jj + 1) perm permMod12
  70.0 * (n0 + n1 + n2)


(***
Generates a noise value between `-1` and `1` based on the given x, y and z value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
***)


let noise3d ({perm=perm; permMod12=permMod12}:PermutationTable) (xin:float) (yin:float) (zin:float) : float =
  let s = (xin + yin + zin) * f3
  let i = int <| floor (xin + s)
  let j = int <| floor (yin + s)
  let k = int <| floor (zin + s)
  let t =  float (i + j + k) * g3
  let x0' = (float i) - t
  let y0' = (float j) - t
  let z0' = (float k) - t
  let x0 = xin - x0'
  let y0 = yin - y0'
  let z0 = zin - z0'
  let (i1, j1, k1, i2, j2,  k2) = getCornerOffset3d x0 y0 z0

  let x1 = x0 - (float i1) + g3
  let y1 = y0 - (float j1) + g3
  let z1 = z0 - (float k1) + g3
  let x2 = x0 - (float i2) + 2.0 * g3
  let y2 = y0 - (float j2) + 2.0 * g3
  let z2 = z0 - (float k2) + 2.0 * g3
  let x3 = x0 - 1.0 + 3.0 * g3
  let y3 = y0 - 1.0 + 3.0 * g3
  let z3 = z0 - 1.0 + 3.0 * g3

  let ii = i &&& 255
  let jj = j &&& 255
  let kk = k &&& 255

  let n0 = getN3d x0 y0 z0 ii jj kk perm permMod12
  let n1 = getN3d x1 y1 z1 (ii + i1) (jj + j1) (kk + k1) perm permMod12
  let n2 = getN3d x2 y2 z2 (ii + i2) (jj + j2) (kk + k2)  perm permMod12
  let n3 = getN3d x3 y3 z3 (ii + 1) (jj + 1) (kk + 1)  perm permMod12
  32.0 * (n0 + n1 + n2 + n3)


(***
Generates a noise value between `-1` and `1` based on the given x, y, z and w value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
***)

let noise4d ({perm=perm; permMod12=permMod12}:PermutationTable) x y z w : float =
  let s = (x + y + z + w) * f4
  let i = int <| floor (x + s)
  let j = int <| floor (y + s)
  let k = int <| floor (z + s)
  let l = int <| floor (w + s)
  let t = float (i + j + k + l) * g4
  let x0' = (float i) - t
  let y0' = (float j) - t
  let z0' = (float k) - t
  let w0' = (float l) - t

  let x0 =  (x - x0')
  let y0 =  (y - y0')
  let z0 =  (z - z0')
  let w0 =  (w - w0')

  let rankx = (addUp [x0 > y0; x0 > z0; x0 > w0])
  let ranky = (addUp [x0 <= y0; y0 > z0; y0 > z0])
  let rankz = (addUp [x0 <= z0; y0 <= z0; z0 > w0])
  let rankw = (addUp [x0 <= w0; y0 <= w0; z0 <= w0])

  let i1 = if rankx >= 3 then 1 else 0
  let j1 = if ranky >= 3 then 1 else 0
  let k1 = if rankz >= 3 then 1 else 0
  let l1 = if rankw >= 3 then 1 else 0

  let i2 = if rankx >= 2 then 1 else 0
  let j2 = if ranky >= 2 then 1 else 0
  let k2 = if rankz >= 2 then 1 else 0
  let l2 = if rankw >= 2 then 1 else 0

  let i3 = if rankx >= 1 then 1 else 0
  let j3 = if ranky >= 1 then 1 else 0
  let k3 = if rankz >= 1 then 1 else 0
  let l3 = if rankw >= 1 then 1 else 0

  let x1 = x0 - (float i1) + g4
  let y1 = y0 - (float j1) + g4
  let z1 = z0 - (float k1) + g4
  let w1 = w0 - (float l1) + g4
  let x2 = x0 - (float i2) + 2.0 * g4
  let y2 = y0 - (float j2) + 2.0 * g4
  let z2 = z0 - (float k2) + 2.0 * g4
  let w2 = w0 - (float l2) + 2.0 * g4
  let x3 = x0 - (float i3) + 3.0 * g4
  let y3 = y0 - (float j3) + 3.0 * g4
  let z3 = z0 - (float k3) + 3.0 * g4
  let w3 = w0 - (float l3) + 3.0 * g4
  let x4 = x0 - 1.0 + 4.0 * g4
  let y4 = y0 - 1.0 + 4.0 * g4
  let z4 = z0 - 1.0 + 4.0 * g4
  let w4 = w0 - 1.0 + 4.0 * g4

  let ii = i &&& 255
  let jj = j &&& 255
  let kk = k &&& 255
  let ll = l &&& 255

  let n0 = getN4d x0 y0 z0 w0 ii jj kk ll perm permMod12
  let n1 = getN4d x1 y1 z1 w1 (ii + i1) (jj + j1) (kk + k1) (ll + l1) perm permMod12
  let n2 = getN4d x2 y2 z2 w2 (ii + i2) (jj + j2) (kk + k2) (ll + l2) perm permMod12
  let n3 = getN4d x3 y3 z3 w3 (ii + i3) (jj + j3) (kk + k3) (ll + l3) perm permMod12
  let n4 = getN4d x4 y4 z4 w4 (ii + 1) (jj + 1) (kk + 1) (ll + 1) perm permMod12
  27.0 * (n0 + n1 + n2 + n3 + n4)