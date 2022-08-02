module Lmhm.Weather.ImageGen.Drawing

open System.Reflection
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing

[<Measure>] type px // Pixels

type Point = float<px> * float<px>

let font =
    use fontStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Lmhm.Weather.ImageGen.consola.ttf")
    FontCollection().Add(fontStream).CreateFont(14.0f, FontStyle.Regular)

let loadImage resourceName =
    use imageStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Lmhm.Weather.ImageGen." + resourceName)
    Image.Load(imageStream)

let saveImage (filePath: string) (image: Image) =
    image.SaveAsPng(filePath)
    image.Dispose()

let drawText (text: string) (color: Color) (point: Point) (image: Image) =
    image.Mutate(fun ctx -> ctx.DrawText(text, font, color, PointF(fst point |> float32, snd point |> float32)) |> ignore)
    image

let drawLine (pen: Pen) (start: Point) (stop: Point) (image: Image) =
    let points = [|
        PointF(fst start |> float32 |> round, snd start |> float32 |> round)
        PointF(fst stop |> float32 |> round, snd stop |> float32 |> round) |]
    image.Mutate(fun ctx -> ctx.DrawLines(pen, points) |> ignore)
    image

let drawPoint (pen : Pen) (point : Point) (image : Image) =
    drawLine pen point point image