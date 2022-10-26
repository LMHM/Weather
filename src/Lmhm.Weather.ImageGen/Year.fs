module Lmhm.Weather.ImageGen.Year

open System
open Lmhm.Weather.ImageGen.Drawing
open Lmhm.Weather.ImageGen.Input
open SixLabors.ImageSharp

module Process =

    let formatMeasuring<[<Measure>]'u> format empty (measuring : Measuring<'u> option) =
        match measuring with
        | Some m -> sprintf format m.Value
        | None -> empty

    let formatTime<[<Measure>]'u> (measuring : Measuring<'u> option) =
        match measuring with
        | Some m -> m.Time.ToString("HH:mm")
        | None -> "--:--"

    let formatTemperature = formatMeasuring "%5.1f" "  ---"
    let formatWind = formatMeasuring "%4.1f" " ---"
    let formatPressure = formatMeasuring "%6.1f" "  ----"
    let formatRain value = 
        match value with 
        | Some v -> sprintf "%5.1f" v
        | None -> "  ---"

    let drawToday (summary : DaySummary) image =
        image
        |> drawText (formatTemperature summary.TempMin) Color.White (72.0<px>, 240.0<px>)
        |> drawText (formatTime summary.TempMin) Color.Grey (72.0<px>, 252.0<px>)
        |> drawText (formatTemperature summary.TempMax) Color.White (144.0<px>, 240.0<px>)
        |> drawText (formatTime summary.TempMax) Color.Grey (144.0<px>, 252.0<px>)
        |> drawText (formatWind summary.WindMax) Color.White (232.0<px>, 240.0<px>)
        |> drawText (formatTime summary.WindMax) Color.Grey (224.0<px>, 252.0<px>)
        |> drawText (formatPressure summary.PressureMin) Color.White (312.0<px>, 240.0<px>)
        |> drawText (formatTime summary.PressureMin) Color.Grey (320.0<px>, 252.0<px>)
        |> drawText (formatPressure summary.PressureMax) Color.White (400.0<px>, 240.0<px>)
        |> drawText (formatTime summary.PressureMax) Color.Grey (408.0<px>, 252.0<px>)
        |> drawText (formatRain summary.Rain) Color.White (480.0<px>, 240.0<px>)

    let generateImage data =
        loadImage "summary_template.png"
        |> drawToday data.Today

let run inputPath outputPath =
    DateOnly.FromDateTime(DateTime.Today)
    |> readYearSummary inputPath
    |> Process.generateImage
    |> saveImage (outputPath + "summering.png")
