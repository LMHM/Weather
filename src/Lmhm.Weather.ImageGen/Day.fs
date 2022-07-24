module Lmhm.Weather.ImageGen.Day

open System
open System.IO
open SixLabors.ImageSharp
open Lmhm.Weather.ImageGen.Drawing
open SixLabors.ImageSharp.Drawing.Processing

module private Process =

    let margin = 8
    let pixelsPerHour = 14
    let pixelsPerMinute = (float pixelsPerHour) / 60.0

    let drawTimeDigits offsetY (hourOffsetX, image) =
        let hourStr hour = hour % 24 |> sprintf "%02i"
        let thirdHour (hour, _) = hour % 3 = 0
        let draw image (hour, offsetX) = drawText (hourStr hour) Color.Cyan (offsetX, offsetY) image
        (hourOffsetX, 
            hourOffsetX
            |> List.filter thirdHour
            |> List.fold draw image)

    let drawTimeTicks offsetY label (hourOffsetX, image) =
        let pen hour = 
            if hour % 3 = 0
            then Pens.Solid(Color.DarkCyan, 1.0f)
            else Pens.Dot(Color.DarkCyan, 1.0f)
        let draw image (hour, offsetX) =
            let x = offsetX + margin
            drawLine (pen hour) (x, offsetY) (x, offsetY + 28) image
        (hourOffsetX,
            hourOffsetX
            |> List.fold draw image
            |> drawText label Color.Green (12, offsetY))

    let last24hourOffsets() =
        let hourNow = DateTime.Now.Hour
        let minuteOffset = (float DateTime.Now.Minute) * pixelsPerMinute |> int
        let toHourOffset hour = (hour, (hour - hourNow) * pixelsPerHour - minuteOffset)
        let inDrawArea (_, offsetX) = offsetX >= 0 && offsetX <= 24 * pixelsPerHour
        [ hourNow .. hourNow + 25 ]
        |> List.map toHourOffset
        |> List.filter inDrawArea
 
    let drawTimeScales image =
        (last24hourOffsets(), image)
        |> drawTimeDigits 2
        |> drawTimeTicks 16 "Riktn"
        |> drawTimeTicks 48 "Hast" 
        |> drawTimeTicks 80 "Tryck"
        |> drawTimeTicks 112 "Temp"
        |> drawTimeTicks 144 "RH"
        |> drawTimeTicks 176 "Nbd"
        |> drawTimeTicks 208 "Sikt"
        |> drawTimeTicks 240 "KVLV"
        |> drawTimeDigits 272
        |> snd

    let generateImage data =
        loadImage "weather_template.png"
        |> drawTimeScales

let readSensorData path =
    let today = DateOnly.FromDateTime(DateTime.Today)
    let last24hrs = DateTime.Now.AddDays(-1)
    Input.read path (today.AddDays(-1)) today
    |> List.filter (fun data -> data.Timestamp >= last24hrs)

let run inputPath outputPath =
    readSensorData inputPath
    |> Process.generateImage
    |> saveImage (Path.Combine(outputPath, "weather.png"))