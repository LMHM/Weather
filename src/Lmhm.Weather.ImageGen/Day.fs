module Lmhm.Weather.ImageGen.Day

open System
open System.IO
open SixLabors.ImageSharp
open Lmhm.Weather.ImageGen.Drawing
open SixLabors.ImageSharp.Drawing.Processing

module private Process =

    open Input

    [<Measure>]type h       // hour
    [<Measure>]type minute  // minute

    let margin = 8.0<px>
    let pixelsPerHour = 14.0<px/h>
    let pixelsPerMinute = pixelsPerHour / 60.0<minute/h>
    let pixelsPerDegree = 28.0<px> / 360.0<deg>

    let penNormalGraph = Pens.Solid(Color.White, 1.0f)
    let penMaxGraph = Pens.Solid(Color.Cyan, 1.0f)
    let penSolidTick = Pens.Solid(Color.DarkCyan, 1.0f)
    let penDotTick = Pens.Dot(Color.DarkCyan, 1.0f)

    let modUp<[<Measure>]'u> (denominator: float<'u>) (value : float<'u>) =
        if value % denominator = 0.0<_> then value else value + denominator - value % denominator

    let modDown<[<Measure>]'u> (denominator : float<'u>) (value : float<'u>) =
        if value % denominator = 0.0<_> then value else value - value % denominator

    let minMaxBy<[<Measure>]'u> (projection : Input.SensorData -> float<'u> option) modulo data =
        let folder (curMin, curMax) value = (min curMin value, max curMax value)
        let (minimum, maximum) =
            data 
            |> List.filter (fun x -> projection x |> Option.isSome)
            |> List.map (fun x -> (projection x).Value)
            |> List.fold folder (LanguagePrimitives.FloatWithMeasure<'u> 10000000.0, 0.0<_>)
        (modDown modulo minimum, modUp modulo maximum)

    let drawTimeDigits offsetY (hourOffsetX, image) =
        let hourStr hour = hour % 24.0<h> |> sprintf "%02.0f"
        let thirdHour (hour, _) = hour % 3.0<h> = 0.0<h>
        let draw image (hour, offsetX) = drawText (hourStr hour) Color.Cyan (offsetX, offsetY) image
        (hourOffsetX, 
            hourOffsetX
            |> List.filter thirdHour
            |> List.fold draw image)

    let drawTimeTicks offsetY label (hourOffsetX, image) =
        let pen hour = if hour % 3.0<h> = 0.0<h> then penSolidTick else penDotTick
        let draw image (hour, offsetX) =
            let x = offsetX + margin
            drawLine (pen hour) (x, offsetY) (x, offsetY + 28.0<px>) image
        (hourOffsetX,
            hourOffsetX
            |> List.fold draw image
            |> drawText label Color.Green (12.0<px>, offsetY))

    let last24hourOffsets() =
        let hourNow = float DateTime.Now.Hour * 1.0<h>
        let minuteOffset = (float DateTime.Now.Minute * 1.0<minute>) * pixelsPerMinute
        let toHourOffset hour = (hour, (hour - hourNow) * pixelsPerHour - minuteOffset + margin)
        let inDrawArea (_, offsetX) = offsetX >= 0.0<px> && offsetX <= 336.0<px>
        [ hourNow .. 1.0<h> .. hourNow + 25.0<h> ]
        |> List.map toHourOffset
        |> List.filter inDrawArea
 
    let drawTimeScales image =
        (last24hourOffsets(), image)
        |> drawTimeDigits 2.0<px>
        |> drawTimeTicks 16.0<px> "Riktn"
        |> drawTimeTicks 48.0<px> "Hast" 
        |> drawTimeTicks 80.0<px> "Tryck"
        |> drawTimeTicks 112.0<px> "Temp"
        |> drawTimeTicks 144.0<px> "RH"
        |> drawTimeTicks 176.0<px> "Nbd"
        |> drawTimeTicks 208.0<px> "Sikt"
        |> drawTimeTicks 240.0<px> "KVLV"
        |> drawTimeDigits 272.0<px>
        |> snd

    let timeOffset timestamp =
        344.0<px> + ((timestamp - DateTime.Now).TotalMinutes * 1.0<minute> * pixelsPerMinute)

    let plot pen timestamp y image =
        (timeOffset timestamp, y) |> drawPoint pen <| image

    let drawWindDirectionGraph data image =
        let current = 
            match (List.last data).WindDirection with
            | Some v -> v |> float |> sprintf "%3.0f"
            | None -> "---"
        let plotDirection image value =
            match value.WindDirection with
            | Some direction -> plot penNormalGraph value.Timestamp (44.0<px> - (direction * pixelsPerDegree)) image
            | None -> image
        List.fold plotDirection image data
        |> drawText current Color.White (466.0<px>, 18.0<px>)

    let drawWindSpeedGraph data image =
        let (minAvg, _) = data |> minMaxBy (fun x -> x.WindSpeed) 5.0<m/s>
        let (_, maxMax) = data |> minMaxBy (fun x -> x.WindSpeedMax) 5.0<m/s>
        let pixelsPerMeterPerSecond = 28.0<px> / (maxMax - minAvg)
        let currentAvg = 
            match (List.last data).WindSpeed with
            | Some v -> v |> float |> sprintf "%4.1f"
            | None -> " ---"
        let currentMax =
            match (List.last data).WindSpeedMax with
            | Some v -> v |> float |> sprintf "%4.1f"
            | None -> " ---"
        let range = maxMax |> sprintf "[0-%2.0f]" 
        let plotSpeed pen timestamp speed =
            plot pen timestamp (76.0<px> - (speed * pixelsPerMeterPerSecond)) image
        let plotMax image value =
            match value.WindSpeedMax with
            | Some speed -> min speed 20.0<m/s> |> plotSpeed penMaxGraph value.Timestamp
            | None -> image
        let plotAverage image value =
           match value.WindSpeed with
           | Some speed -> min speed 20.0<m/s> |> plotSpeed penNormalGraph value.Timestamp
           | None -> image
        List.fold plotMax image data
        |> List.fold plotAverage <| data
        |> drawText currentAvg Color.White (458.0<px>, 50.0<px>)
        |> drawText currentMax Color.Cyan (458.0<px>, 66.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 66.0<px>)

    let drawPressureGraph data image =
        let (minPressure, maxPressure) = data |> minMaxBy (fun x -> x.AirPressureSeaLevel) 5.0<hPa>
        let pixelsPerhPa = 28.0<px> / (maxPressure - minPressure)
        let current =
            match (List.last data).AirPressureSeaLevel with
            | Some v -> v |> float |> sprintf "%6.1f"
            | None -> "  ---"
        let range = sprintf "[%4.0f-%4.0f]" minPressure maxPressure
        let plot image value =
            match value.AirPressureSeaLevel with
            | Some pressure -> plot penNormalGraph value.Timestamp (108.0<px> - (pressure - minPressure) * pixelsPerhPa) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (442.0<px>, 82.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 98.0<px>)

    let drawTemperatureGraph data image =
        let (minTemp, maxTemp) = data |> minMaxBy (fun x -> x.TemperatureAir) 5.0<degC>
        let pixelsPerDegree = 28.0<px> / (maxTemp - minTemp)
        let current = 
            match (List.last data).TemperatureAir with
            | Some v -> v |> float |> sprintf "%4.1f"
            | None -> "---"
        let range = sprintf "[%+2.0f-%+2.0f]" minTemp maxTemp
        let plot image value =
            match value.TemperatureAir with
            | Some temp -> plot penNormalGraph value.Timestamp (140.0<px> - (temp - minTemp) * pixelsPerDegree) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (458.0<px>, 114.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 130.0<px>)

    let drawRelativeHumidity data image =
        let (minRh, maxRh) = data |> minMaxBy (fun x -> x.RelativeHumidity) 5.0<percent>
        let pixelsPerPercent = 28.0<px> / (maxRh - minRh)
        let current =
            match (List.last data).RelativeHumidity with
            | Some v -> v |> float |> sprintf "%2.0f"
            | None -> "--"
        let dewPoint =
            match (List.last data).DewPoint with
            | Some v -> v |> float |> sprintf "%4.1f"
            | None -> " ---"
        let range = sprintf "[%2.0f-%2.0f]" minRh maxRh
        let plot image value =
            match value.RelativeHumidity with
            | Some rh -> plot penNormalGraph value.Timestamp (172.0<px> - (rh - minRh) * pixelsPerPercent) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (472.0<px>, 146.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 162.0<px>)
        |> drawText dewPoint Color.White (458.0<px>, 162.0<px>)
    
    let drawPulses data image =
        let (_, maxPulses) = data |> minMaxBy (fun x -> x.Pulses) 5.0<p>
        let pixelsPerPulse = 28.0<px> / maxPulses
        let current =
            match (List.last data).Pulses with
            | Some v -> v |> float |> sprintf "%3.0f"
            | None -> "---"
        let range = sprintf "[0-%3.0f]" maxPulses
        let plotPulses timestamp pulses =
            let x = timeOffset timestamp
            drawLine penNormalGraph (x, 204.0<px>) (x, 204.0<px> - pulses * pixelsPerPulse) image
        let plot image value =
            match value.Pulses with
            | Some p when p > 0.0<p> -> plotPulses value.Timestamp p
            | _ -> image
        List.fold plot image data
        |> drawText current Color.White (466.0<px>, 178.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 192.0<px>)

    let drawVisibility data image =
        let (minVis, maxVis) = data |> minMaxBy (fun x -> x.Visibility) 5.0<km>
        let pixelsPerKm = 28.0<px> / (maxVis - minVis)
        let current = 
            match (List.last data).Visibility with
            | Some v -> v |> float |> sprintf "%2.0f"
            | None -> "--"
        let range = sprintf "[%1.0f-%2.0f]" minVis maxVis
        let plot image value =
            match value.Visibility with
            | Some vis -> plot penNormalGraph value.Timestamp (236.0<px> - (vis - minVis) * pixelsPerKm) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (472.0<px>, 210.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 226.0<px>)

    let drawShortWave data image =
        let (minShort, maxShort) = data |> minMaxBy (fun x -> x.ShortWave) 10.0<W/m^2>
        let pixelsPerShortWave = 28.0<px> / (maxShort - minShort)
        let current =
            match (List.last data).ShortWave with
            | Some v -> v |> float |> sprintf "%3.0f"
            | None -> "---"
        let range = sprintf "[%1.0f-%1.0f]" minShort maxShort
        let plot image value =
            match value.ShortWave with
            | Some sw -> plot penNormalGraph value.Timestamp (268.0<px> - (sw - minShort) * pixelsPerShortWave) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (466.0<px>, 242.0<px>)
        |> drawText range Color.LightGray (536.0<px>, 242.0<px>)

    let generateImage data =
        loadImage "weather_template.png"
        |> drawTimeScales
        |> drawWindDirectionGraph data
        |> drawWindSpeedGraph data
        |> drawPressureGraph data
        |> drawTemperatureGraph data
        |> drawRelativeHumidity data
        |> drawPulses data
        |> drawVisibility data
        |> drawShortWave data

let readSensorData path =
    let today = DateOnly.FromDateTime(DateTime.Today)
    let last24hrs = DateTime.Now.AddDays(-1)
    let now = DateTime.Now
    Input.read path (today.AddDays(-1)) today
    |> List.filter (fun data -> data.Timestamp >= last24hrs && data.Timestamp <= now)

let run inputPath outputPath =
    readSensorData inputPath
    |> Process.generateImage
    |> saveImage (Path.Combine(outputPath, "weather.png"))