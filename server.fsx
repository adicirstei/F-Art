#r "packages/Suave/lib/net40/Suave.dll"

open Suave
open System
open System.Drawing
open System.Drawing.Imaging


let draw (text:string) =

  let redBr = new SolidBrush(Color.Red)
  let whiteBr = new SolidBrush(Color.Red)
  let i = new Bitmap(600,400)
  let g = Graphics.FromImage i
  let f = new Font("Courier", float32 <| 24.0)
  g.FillRectangle(whiteBr, float32 <| 0.0, float32 <| 0.0, float32 <| 600.0, float32 <| 400.0 )
  g.DrawString(text, f, redBr, float32 <| 10.0, float32 <| 40.0)
  g.Flush()
  let ms = new System.IO.MemoryStream()
  i.Save(ms, ImageFormat.Png)
  ms.ToArray()



startWebServer defaultConfig (Successful.ok <| draw "bibi")
