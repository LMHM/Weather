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

    let drawTimeAxis image =
        let hourNow = DateTime.Now.Hour
        let minuteOffset = (float DateTime.Now.Minute) * pixelsPerMinute |> int
        let pen hour = if hour % 3 = 0 then Pens.Solid(Color.DarkCyan, 1.0f) else Pens.Dot(Color.DarkCyan, 1.0f)
        let hourStr hour = hour % 24 |> sprintf "%02i"
        for hour in hourNow .. hourNow + 25 do
            let offset = (hour - hourNow) * pixelsPerHour - minuteOffset
            if offset >= 0 && offset <= 24 * pixelsPerHour then
                let x = offset + margin
                drawLine (pen hour) (x, 16) (x, 44) image |> ignore
                if hour % 3 = 0 then 
                    drawText (hourStr hour) Color.Cyan (offset, 2) image |> ignore
        image

    let generateImage data =
        loadImage "weather_template.png"
        |> drawTimeAxis

let readSensorData path =
    let today = DateOnly.FromDateTime(DateTime.Today)
    let last24hrs = DateTime.Now.AddDays(-1)
    Input.read path (today.AddDays(-1)) today
    |> List.filter (fun data -> data.Timestamp >= last24hrs)

let run inputPath outputPath =
    readSensorData inputPath
    |> Process.generateImage
    |> saveImage (Path.Combine(outputPath, "weather.png"))