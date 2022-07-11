module Lmhm.Weather.ImageGen.AverageTemp

open System
open Lmhm.Weather.ImageGen.Physics

type Data =
  { Sum: float<degC>
    Count: int
    Date: DateOnly }
    
    static member Default () = 
      { Sum = 0.0<degC>
        Count = 0 
        Date = DateOnly.FromDateTime(DateTime.Today) }

module Input =

    open System.IO

    let readCurrent path =
        let filePath = Path.Combine(path, "mtsum.txt")
        if File.Exists(filePath) then
            let line = File.ReadAllText(filePath)
            { 
                Sum = line[..10] |> float |> (*) 1.0<degC>
                Count = line[11..21] |> int
                Date = DateOnly.ParseExact(line[22..].Trim(), "yyMMdd")
            }
        else
            Data.Default ()

    let readUpdate path =
        let filePath = Path.Combine(path, "VDATA.TXT")
        if File.Exists(filePath) then
            File.ReadAllText(filePath)[130..146] |> float |> (*) 1.0<degC>
        else
            0.0<degC>

module Output =

    open System.IO

    let filePath path pattern (date: DateOnly) =
        let filename = date.ToString("yyMMdd") |> sprintf pattern
        Path.Combine(path, filename)

    let writeAverage path data =
        let filePath = filePath path "MT%s.TXT" data.Date
        let content = sprintf "%5.1f" (data.Sum / (float data.Count))
        File.WriteAllText(filePath, content)

    let writeCurrent path data =
        let filePath = Path.Combine(path, "mtsum.txt")
        let content = sprintf " %10.1f %10i %s" data.Sum data.Count (data.Date.ToString("yyMMdd"))
        File.WriteAllText(filePath, content)

let nextDay path data =
    let today = DateOnly.FromDateTime(DateTime.Today)
    if data.Date <> today then
        Output.writeAverage path data
        Data.Default ()
    else
        data

let update newValue data =
    { data with Sum = data.Sum + newValue; Count = data.Count + 1 }

let run inputPath outputPath =
    Input.readCurrent inputPath
    |> nextDay outputPath
    |> update (Input.readUpdate inputPath)
    |> Output.writeCurrent outputPath