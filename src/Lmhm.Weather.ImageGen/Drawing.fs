module Lmhm.Weather.ImageGen.Drawing

open System.Reflection
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing

let font =
    use fontStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Lmhm.Weather.ImageGen.consola.ttf")
    FontCollection().Add(fontStream).CreateFont(14.0f, FontStyle.Regular)

let loadImage resourceName =
    use imageStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Lmhm.Weather.ImageGen." + resourceName)
    Image.Load(imageStream)

let saveImage (filePath: string) (image: Image) =
    image.SaveAsPng(filePath)
    image.Dispose()

let drawText (text: string) (color: Color) (x: int, y: int) (image: Image) =
    image.Mutate(fun ctx -> ctx.DrawText(text, font, color, PointF(float32 x, float32 y)) |> ignore)
    image

let drawLine (pen: Pen) (x: int, y: int) (x1: int, y1: int) (image: Image) =
    let points = [| PointF(float32 x, float32 y); PointF(float32 x1, float32 y1) |]
    image.Mutate(fun ctx -> ctx.DrawLines(pen, points) |> ignore)
    image
