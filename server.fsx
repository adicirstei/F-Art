#r "packages/Suave/lib/net40/Suave.dll"

open Suave
open System
open System.Drawing
open System.Drawing.Imaging





startWebServer defaultConfig (Successful.OK "Hello World!")
