#r "packages/Suave/lib/net40/Suave.dll"

#load "art.fsx"

open Suave
open Suave.Operators

open System
open System.Drawing
open System.Drawing.Imaging

let imageWebPart (img:Image) : WebPart =
  let ms = new System.IO.MemoryStream()
  img.Save(ms, ImageFormat.Png)
  ms.ToArray()
  |> Successful.ok >=> Writers.setMimeType "image/png"



let app : WebPart = imageWebPart <| Art.draw "Putirinta"

startWebServer defaultConfig app
