module Lmhm.Weather.ImageGen.Month

open System
open SixLabors.ImageSharp
open Lmhm.Weather.ImageGen.Input
open Lmhm.Weather.ImageGen.Drawing

module private Process =

    let formatDate (data: DaySummary) = data.Date.ToString("MM-dd")

    let dateColor (data: DaySummary) =
        match data.TempAvg with
        | None -> Color.White
        | Some x when x > 10.0<degC> -> Color.Red
        | Some x when x < 0.0<degC> -> Color.Cyan
        | _ -> Color.Yellow

    let formatSmall measuring =
        match measuring with
        | Some x -> sprintf "%5.1f" x.Value
        | None -> "    -"

    let formatLarge measuring =
        match measuring with
        | Some x -> sprintf "%6.1f" x.Value
        | None -> "     -"

    let formatTime measuring =
        match measuring with
        | Some x -> x.Time.ToString("HH:mm")
        | None -> "  -  "

    let formatRain value =
        match value with
        | Some x when x = 0.0<mm> -> "  -"
        | Some x when x < 1.0<mm> -> " <1"
        | Some x -> sprintf "%3.0f" x
        | None -> "  -"

    let drawRow (data: DaySummary) rowNbr image =
        let y = 1.0<px> * (rowNbr * 13 + 26 |> float)

        image
        |> drawText (formatDate data) (dateColor data) (8.0<px>, y)
        |> drawText (formatSmall data.TempMin) Color.LightGreen (58.0<px>, y)
        |> drawText (formatTime data.TempMin) Color.Grey (104.0<px>, y)
        |> drawText (formatSmall data.TempMax) Color.LightGreen (154.0<px>, y)
        |> drawText (formatTime data.TempMax) Color.Grey (200.0<px>, y)
        |> drawText (formatSmall data.WindMax) Color.LightGreen (248.0<px>, y)
        |> drawText (formatTime data.WindMax) Color.Grey (296.0<px>, y)
        |> drawText (formatLarge data.PressureMin) Color.LightGreen (352.0<px>, y)
        |> drawText (formatTime data.PressureMin) Color.Grey (408.0<px>, y)
        |> drawText (formatLarge data.PressureMax) Color.LightGreen (456.0<px>, y)
        |> drawText (formatTime data.PressureMax) Color.Grey (512.0<px>, y)
        |> drawText (formatRain data.Rain) Color.LightGreen (560.0<px>, y)
        |> drawText (formatDate data) (dateColor data) (592.0<px>, y)

    let generateImage data =
        let image = loadImage "20dag_template.png"
        let processRow (image, rowNbr) data = (drawRow data rowNbr image, rowNbr + 1)
        List.fold processRow (image, 0) data |> fst

let run inputPath outputPath =
    let today = DateOnly.FromDateTime(DateTime.Today)

    today :: [ for i in -1..-1..-20 -> today.AddDays i ]
    |> readDaySummary inputPath
    |> Process.generateImage
    |> saveImage (outputPath + "20dag.png")
