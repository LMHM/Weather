module Lmhm.Weather.ImageGen.Year

open System
open Lmhm.Weather.ImageGen.Drawing
open Lmhm.Weather.ImageGen.Input
open SixLabors.ImageSharp

module Process =

    let formatMeasuring<[<Measure>] 'u> format empty (measuring: Measuring<'u> option) =
        match measuring with
        | Some m -> sprintf format m.Value
        | None -> empty

    let formatTime<[<Measure>] 'u> (measuring: Measuring<'u> option) =
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

    let drawDay color offset (summary: DaySummary) image =
        image
        |> drawText (formatTemperature summary.TempMin) color (72.0<px>, 216.0<px> + offset)
        |> drawText (formatTime summary.TempMin) Color.Grey (72.0<px>, 228.0<px> + offset)
        |> drawText (formatTemperature summary.TempMax) color (144.0<px>, 216.0<px> + offset)
        |> drawText (formatTime summary.TempMax) Color.Grey (144.0<px>, 228.0<px> + offset)
        |> drawText (formatWind summary.WindMax) color (232.0<px>, 216.0<px> + offset)
        |> drawText (formatTime summary.WindMax) Color.Grey (224.0<px>, 228.0<px> + offset)
        |> drawText (formatPressure summary.PressureMin) color (312.0<px>, 216.0<px> + offset)
        |> drawText (formatTime summary.PressureMin) Color.Grey (320.0<px>, 228.0<px> + offset)
        |> drawText (formatPressure summary.PressureMax) color (400.0<px>, 216.0<px> + offset)
        |> drawText (formatTime summary.PressureMax) Color.Grey (408.0<px>, 228.0<px> + offset)
        |> drawText (formatRain summary.Rain) color (480.0<px>, 216.0<px> + offset)

    let generateImage data =
        loadImage "summary_template.png"
        |> drawDay Color.Yellow 0.0<px> data.Yesterday
        |> drawDay Color.White 24.0<px> data.Today

let run inputPath outputPath =
    DateOnly.FromDateTime(DateTime.Today)
    |> readYearSummary inputPath
    |> Process.generateImage
    |> saveImage (outputPath + "summering.png")
