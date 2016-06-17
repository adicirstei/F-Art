#r "packages/Fsharp.Data/lib/net40/Fsharp.Data.dll"

open FSharp.Data
open System.Drawing

type Palette = Color array
type Particle = {
  position : float*float
  radius : float
  duration : float
  time : float
  velocity : float*float
  speed : float
  color : Color
//  prev : (Float,Float)
}
type Config = {
  pointilism: float
  noiseScalar: float*float
  startArea: float
  maxRadius: int
  lineStyle: System.Drawing.Drawing2D.LineCap
  interval: float
  count: int
  steps: int
  palette : Palette
}

type Palettes = FSharp.Data.JsonProvider<"data/palettes.json">
type JsonPalette = Palettes.Root


let (width,height) = 1200,700

#load "random.fsx"

let clamp v x y =
  if v < x 
  then x 
  else if v > y then y else v

let lerp x y a =
  x * (1.0 - a) + y * a

let tAdd (i , j) (p,q) = (i + p, j + q)


let normalize (x,y) =
  let len = x*x + y*y
  if len > 0.0 then (x*(1.0 /(sqrt len)), y*(1.0 /(sqrt len))) else (0.0,0.0)

let scale s (x,y) = (s * x, s * y)

let maps = [| 
  "architecture.jpg"
  "church2.jpg"
  "city2.jpg"
  "city5.jpg"
  "eye.jpg"
  "fractal1.jpg"
  "fractal2.jpg"
  "geo1.jpg"
  "geo3.jpg"
  "geo4.jpg"
  "geo5.jpg"
  "map7.jpg"
  "nature1.jpg"
  "pat1.jpg"
  "scifi.jpg"
  "sym3.jpg"
  "sym6.jpg"
  "pattern_dots_black_white.png" |] |> Array.map (fun f -> "maps/" + f )


let luminosity (c:Color) : float =
  (float c.R) * 0.299 + (float c.G) * 0.587 + (float c.B) * 0.114


let strToColor (str:string) : Color =
  ColorTranslator.FromHtml ("#" + str)

let toPalette (v:Palettes.NumbersOrStrings) : Palette =
  v.Strings
  |> Array.map strToColor 

let palettes =
  Palettes.GetSamples()
  |> Array.map (fun (i:JsonPalette) -> i.Colors |> toPalette)   

let createConfig (rand:System.Random) : Config =
  { 
    pointilism = 0.1 * rand.NextDouble () 
    noiseScalar = (0.000001, Random.floatRand rand 0.0002 0.004)
    startArea = Random.floatRand rand 0.0 1.5
    maxRadius = 10 // rand.Next (5, 100)
    lineStyle = 
      if rand.NextDouble () < 0.5 
      then System.Drawing.Drawing2D.LineCap.Square 
      else System.Drawing.Drawing2D.LineCap.Round
    interval = Random.floatRand rand 0.001 0.01
    count = rand.Next (50, 2000)
    steps = rand.Next (100, 1000)
    palette = palettes.[rand.Next palettes.Length]
  }


let createParticle (rand:System.Random) (config:Config) (i:int) : Particle =
  let scale = (System.Math.Min (width, height)) / 2
  let (x,y) = Random.randomSphere rand ((float scale) * config.startArea)
  let pal = config.palette
  let dur = Random.floatRand rand 1.0 500.0
  {
    
    position = (x + (float width) / 2.0, y + (float height) / 2.0)
   
    radius = Random.floatRand rand 0.01 (float config.maxRadius)
    duration = dur
    time = Random.floatRand rand 1.0 dur
    velocity = (Random.floatRand rand -1.0 1.0, Random.floatRand rand -1.0 1.0)
    speed = (Random.floatRand rand 0.5 2.0)
    
    color = pal.[rand.Next pal.Length]
    
  }

#load "noise.fsx"


let draw (rand:System.Random) =
  let mutable time = 0.0
  let mutable stepCount = 0
  let config = createConfig rand
  let pal = config.palette

  let i = new Bitmap(width, height)
  let g = Graphics.FromImage i
  let lumMap:Bitmap = new Bitmap (maps.[rand.Next maps.Length])  


  g.DrawImage(lumMap, 0, 0, width, height)
  //g.Flush()
  let lumi = Array2D.init width height (fun x y -> luminosity <| i.GetPixel(x,y))

  let particles = Array.init config.count (createParticle rand config)

  g.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.AntiAlias


  let simplex = Noise.permutationTable rand 
  let pointilism = lerp 0.000001 0.5 config.pointilism

  let stepPart (i:int) (p:Particle) : unit =
    let (x,y) = p.position
    let fx = float <| clamp (int <| round x) 0 (width - 1)
    let fy = float <| clamp (int <| round y) 0 (height - 1)
    let heightValue = lumi.[int fx, int fy] / 255.0
    let pS = lerp (fst config.noiseScalar) (snd config.noiseScalar) heightValue
    let n = Noise.noise3d simplex (fx * pS) (fy * pS) (p.duration + time)
    let angle = n * System.Math.PI * 2.0
    let speed = p.speed + (lerp 0.0 2.0 (1.0 - heightValue))
    let velo = normalize <| tAdd p.velocity (System.Math.Cos angle, System.Math.Sin angle)
    let move = scale speed p.velocity
    let newPos = tAdd p.position move
    let r = (lerp 0.01 1.0 heightValue) * p.radius * (Noise.noise3d simplex (x*pointilism) (y*pointilism) (p.duration + time))

    let pen = new Pen(p.color, float32 <| (r * (p.time / p.duration)))
    pen.StartCap <- config.lineStyle
    pen.EndCap <- config.lineStyle

    g.DrawLine (pen, int <| x, int <| y, int <| fst newPos, int <| snd newPos)
 
    particles.[i] <- 
      if p.time + config.interval > p.duration then createParticle rand config i
      else {p with velocity = velo; position = newPos; time = p.time + config.interval }
    ()

  g.FillRectangle(new SolidBrush(pal.[0]), 0, 0, width, height )

  while stepCount < config.steps do
    time <- time + config.interval
    stepCount <- stepCount + 1
    Array.iteri stepPart particles 



  g.DrawImage(lumMap, 0, 0, 60, 40)
  //g.Flush()
  i


