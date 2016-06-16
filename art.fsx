#r "packages/Fsharp.Data/lib/net40/Fsharp.Data.dll"

open FSharp.Data
open System.Drawing

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

type Palettes = FSharp.Data.JsonProvider<"data/palettes.json">
type JsonPalette = Palettes.Root
type Palette = Color array


let strToColor (str:string) : Color =
  ColorTranslator.FromHtml ("#" + str)

let toPalette (v:Palettes.NumbersOrStrings) : Palette =
  v.Strings
  |> Array.map strToColor 


let palettes =
  Palettes.GetSamples()
  |> Array.map (fun (i:JsonPalette) -> i.Colors |> toPalette)   


let draw (rand:System.Random) =
  let pal = palettes.[rand.Next palettes.Length]
  let text = rand.Next() |> sprintf "~# %d #~"
  let redBr = new SolidBrush(Color.Red)
  let whiteBr = new SolidBrush(Color.White)
  let i = new Bitmap(600,400)
  let g = Graphics.FromImage i
  let f = new Font("Courier", float32 <| 24.0)
  g.FillRectangle(whiteBr, float32 <| 0.0, float32 <| 0.0, float32 <| 600.0, float32 <| 400.0 )
  pal |> Array.iteri (fun i c -> g.FillRectangle((new SolidBrush(c) ), float32 <| 30*i, float32 <| 0.0, float32 <| 30*i + 30, float32 <| 64.0 ))

  //g.DrawString(text, f, redBr, float32 <| 10.0, float32 <| 40.0)
  g.Flush()
  i


