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


let (width,height) = 600,400

#load "random.fsx"

let clamp v x y =
  if v < x 
  then x 
  else if v > y then y else v

let lerp x y a =
  x * (1.0 - a) + y * a

let tAdd (i , j) (p,q) = (i + p, j + q)



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
    maxRadius = rand.Next (5, 100)
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
  let time = 0.0
  let config = createConfig rand
  let pal = config.palette
  let text = rand.Next() |> sprintf "~# %d #~"
  let redBr = new SolidBrush(Color.Red)
  let whiteBr = new SolidBrush(Color.White)
  let i = new Bitmap(600,400)
  let g = Graphics.FromImage i
  let f = new Font("Courier", float32 <| 24.0)

  let particles = Array.init 22 (createParticle rand config)

  let simplex = Noise.permutationTable rand 

  let stepPart (i:int) (p:Particle) : unit =
    let (x,y) = p.position
    let fx = float <| clamp (int <| round x) 0 (width - 1)
    let fy = float <| clamp (int <| round y) 0 (height - 1)
    let heightValue = 0.98
    let pS = lerp (fst config.noiseScalar) (snd config.noiseScalar) heightValue
    let n = Noise.noise3d simplex (fx * pS) (fy * pS) (p.duration + time)
    let angle = n * System.Math.PI * 2.0
	  let speed = p.speed + (lerp 0.0 2.0 (1.0 - heightValue))
    let velo = tAdd p.velocity (System.Math.Cos angle, System.Math.Sin angle)

(***
      vec2.add(p.velocity, p.velocity, [ Math.cos(angle), Math.sin(angle) ]);
      vec2.normalize(p.velocity, p.velocity);
      var move = vec2.scale([], p.velocity, speed);
      vec2.add(p.position, p.position, move);

      var s2 = pointilism;
      var r = p.radius * simplex.noise3D(x * s2, y * s2, p.duration + time);
      r *= lerp(0.01, 1.0, heightValue);
      ctx.beginPath();
      ctx.lineTo(x, y);
      ctx.lineTo(p.position[0], p.position[1]);
      ctx.lineWidth = r * (p.time / p.duration);
      ctx.lineCap = opt.lineStyle || 'square';
      ctx.lineJoin = opt.lineStyle || 'square';
      ctx.strokeStyle = p.color;

      // ctx.strokeStyle = colorStyle(rgb.map(x => x * 255));
      ctx.globalAlpha = globalAlpha;
      ctx.stroke();

      p.time += dt;
      if (p.time > p.duration) {
        resetParticle(p);
      }

***)

    ()



  g.FillRectangle(whiteBr, float32 <| 0.0, float32 <| 0.0, float32 <| 600.0, float32 <| 400.0 )


  
  pal |> Array.iteri (fun i c -> g.FillRectangle((new SolidBrush(c) ), float32 <| 30*i, float32 <| 0.0, float32 <| 30*i + 30, float32 <| 64.0 ))

  //g.DrawString(text, f, redBr, float32 <| 10.0, float32 <| 40.0)
  g.Flush()
  i


