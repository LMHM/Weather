module Lmhm.Weather.ImageGen.Month

open System
open SixLabors.ImageSharp
open Lmhm.Weather.ImageGen.Physics
open Lmhm.Weather.ImageGen.Drawing

type Data = 
  { Date: DateOnly
    TempAvg: float<degC> option
    TempMin: Measuring<degC> option
    TempMax: Measuring<degC> option
    WindMax: Measuring<m/s> option
    PressureMin: Measuring<hPa> option
    PressureMax: Measuring<hPa> option
    Rain: float<mm> option }
    
    static member Default =
      { Date = DateOnly.FromDateTime(DateTime.Today)
        TempAvg = None
        TempMin = None
        TempMax = None
        WindMax = None
        PressureMin = None
        PressureMax = None
        Rain = None }

module Input =

    open System.IO
    open System.Globalization

    let filePath path pattern (date: DateOnly) =
        let filename = date.ToString("yyMMdd") |> sprintf pattern
        Path.Combine(path, filename)

    let readTempAvg path date =
        let filePath = filePath path "MT%s.TXT" date
        if not(File.Exists(filePath)) then
            None
        else
            File.ReadAllLines(filePath)[0] |> float |> (*) 1.0<degC> |> Some

    let readMeasuring<[<Measure>]'u> (line: string) : Measuring<'u> option =
        {
            Value = Double.Parse(line[..10], CultureInfo.InvariantCulture) |> LanguagePrimitives.FloatWithMeasure
            Time = line[14..] |> DateTime.Parse
        } |> Some

    let readRain path date =
        let fileName = filePath path "DN%s.TXT" date
        if not(File.Exists(fileName)) then
            None
        else
            File.ReadAllLines(fileName)[0] |> float |> (*) 1.0<mm> |> Some

    let readDay path (date: DateOnly) =
        let filePath = filePath path "DS%s.TXT" date
        if not(File.Exists(filePath)) then
            { Data.Default with Date = date }
        else
            let lines = File.ReadAllLines(filePath)
            {
                Date = date
                TempAvg = readTempAvg path date
                TempMin = readMeasuring<degC> lines[3]
                TempMax = readMeasuring<degC> lines[4]
                WindMax = readMeasuring<m/s> lines[0]
                PressureMin = readMeasuring<hPa> lines[1]
                PressureMax = readMeasuring<hPa> lines[2]
                Rain = readRain path date
            }

    let readData path =
        List.map (readDay path)

module Process =
    
    let formatDate data =
        data.Date.ToString("MM-dd")

    let dateColor data =
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

    let drawRow data rowNbr image =
        let y = rowNbr * 13 + 26
        image 
        |> drawText (formatDate data) (dateColor data) 8 y
        |> drawText (formatSmall data.TempMin) Color.LightGreen 58 y
        |> drawText (formatTime data.TempMin) Color.Grey 104 y
        |> drawText (formatSmall data.TempMax) Color.LightGreen 154 y
        |> drawText (formatTime data.TempMax) Color.Grey 200 y
        |> drawText (formatSmall data.WindMax) Color.LightGreen 248 y
        |> drawText (formatTime data.WindMax) Color.Grey 296 y
        |> drawText (formatLarge data.PressureMin) Color.LightGreen 352 y
        |> drawText (formatTime data.PressureMin) Color.Grey 408 y
        |> drawText (formatLarge data.PressureMax) Color.LightGreen 456 y
        |> drawText (formatTime data.PressureMax) Color.Grey 512 y
        |> drawText (formatRain data.Rain) Color.LightGreen 560 y
        |> drawText (formatDate data) (dateColor data) 592 y

    let generateImage data =
        let image = loadImage "20dag_template.png"
        let processRow (image, rowNbr) data = (drawRow data rowNbr image, rowNbr + 1)
        List.fold processRow (image, 0) data |> fst
    
let run inputPath outputPath =
    let today = DateOnly.FromDateTime(DateTime.Today)
    today :: [for i in -1..-1..-20 -> today.AddDays i]
    |> Input.readData inputPath 
    |> Process.generateImage
    |> saveImage (outputPath + "20dag.png")
