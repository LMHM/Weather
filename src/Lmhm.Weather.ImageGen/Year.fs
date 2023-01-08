module Lmhm.Weather.ImageGen.Year

open System
open Lmhm.Weather.ImageGen.Drawing
open Lmhm.Weather.ImageGen.Input
open SixLabors.ImageSharp
open System.IO

module private Process =

    let formatMeasuring<[<Measure>] 'u> format empty (measuring: Measuring<'u> option) =
        match measuring with
        | Some m -> sprintf format m.Value
        | None -> empty

    let formatTime<[<Measure>] 'u> (measuring: Measuring<'u> option) =
        match measuring with
        | Some m -> m.Time.ToString("HH:mm")
        | None -> "--:--"

    let formatDay<[<Measure>] 'u> (measuring: Measuring<'u> option) =
        match measuring with
        | Some m -> m.Time.ToString("dd")
        | None -> "--"

    let formatMonth (date: DateOnly) = date.ToString("MMM yy")

    let formatTemperature = formatMeasuring "%5.1f" "  ---"
    let formatWind = formatMeasuring "%4.1f" " ---"
    let formatPressure = formatMeasuring "%6.1f" "  ----"
    let formatAccRain = sprintf "%4.0f"

    let formatRain value =
        match value with
        | Some v -> sprintf "%5.1f" v
        | None -> "  ---"

    let yearColor (date: DateOnly) =
        if date.Year = DateTime.UtcNow.Year then
            Color.Yellow
        else
            Color.LightGreen

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

    let drawMonth data rowNbr image =
        let y = 1.0<px> * (rowNbr * 12 + 48 |> float)
        let color = yearColor data.Date

        image
        |> drawText (formatMonth data.Date) color (8.0<px>, y)
        |> drawText (formatMonth data.Date) color (584.0<px>, y)
        |> drawText (formatTemperature data.TempMin) color (72.0<px>, y)
        |> drawText (formatDay data.TempMin) Color.Grey (120.0<px>, y)
        |> drawText (formatTemperature data.TempMax) color (144.0<px>, y)
        |> drawText (formatDay data.TempMax) Color.Grey (192.0<px>, y)
        |> drawText (formatWind data.WindMax) color (232.0<px>, y)
        |> drawText (formatDay data.WindMax) Color.Grey (272.0<px>, y)
        |> drawText (formatPressure data.PressureMin) color (312.0<px>, y)
        |> drawText (formatDay data.PressureMin) Color.Grey (368.0<px>, y)
        |> drawText (formatPressure data.PressureMax) color (400.0<px>, y)
        |> drawText (formatDay data.PressureMax) Color.Grey (456.0<px>, y)
        |> drawText (formatRain data.Precipitation) color (480.0<px>, y)
        |> drawText (formatAccRain data.AccumulatedPrecipitation) color (528.0<px>, y)

    let generateImage data =
        let image = loadImage "summary_template.png"

        let processRow (image, rowNbr) data =
            (drawMonth data rowNbr image, rowNbr + 1)

        data.Months
        |> List.sortBy (fun x -> (-x.Date.Year, x.Date.Month))
        |> List.fold processRow (image, 0)
        |> fst
        |> drawDay Color.Yellow 0.0<px> data.Yesterday
        |> drawDay Color.White 24.0<px> data.Today
        |> drawText (DateTime.Now.ToString("yyyy-MM-dd HH:mm")) Color.Orange (508.0<px>, 276.0<px>)

let run inputPath outputPath =
    DateOnly.FromDateTime(DateTime.Today)
    |> readYearSummary inputPath
    |> Process.generateImage
    |> saveImage (Path.Combine(outputPath, "summering.png"))
