#r "packages/Suave/lib/net40/Suave.dll"

#load "art.fsx"

open Suave
open Suave.Operators
open Suave.Filters

open System
open System.Drawing
open System.Drawing.Imaging

let imageWebPart (img:Image) : WebPart =
  let ms = new System.IO.MemoryStream()
  img.Save(ms, ImageFormat.Png)
  ms.ToArray()
  |> Successful.ok >=> Writers.setMimeType "image/png"

let randomImage () =
  let seed = Environment.TickCount
  
  printfn "Random seed : %d" seed
  Art.draw (new System.Random(seed))
  |> imageWebPart

let seededImage seed =
  printfn "Chosen seed : %d" seed
  Art.draw (System.Random(seed))
  |> imageWebPart


let app : WebPart = 
  choose [
    GET >=> pathScan "/image/%d" seededImage
    GET >=> path "/image" >=> warbler (fun _ -> randomImage())
  ]


startWebServer defaultConfig app
