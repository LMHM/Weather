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
        let inDrawArea (_, offsetX) = offsetX >= margin && offsetX <= (24.0<h> * pixelsPerHour + margin)
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

    let drawWindDirectionGraph data image =
        let plotDirection (timestamp : DateTime) direction = 
            (timeOffset timestamp, 44.0<px> - direction * pixelsPerDegree)
            |> drawPoint penNormalGraph <| image
        let plot image value =
            match value.WindDirection with
            | Some direction -> plotDirection value.Timestamp direction
            | None -> image
        data |> List.fold plot image

    let drawWindSpeedGraph data image =
        let (minAvg, _) = data |> minMaxBy (fun x -> x.WindSpeed) 5.0<m/s>
        let (_, maxMax) = data |> minMaxBy (fun x -> x.WindSpeedMax) 5.0<m/s>
        let pixelsPerMeterPerSecond = 28.0<px> / (maxMax - minAvg)
        let plotSpeed pen timestamp speed =
            (timeOffset timestamp, 76.0<px> - speed * pixelsPerMeterPerSecond)
            |> drawPoint pen <| image
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

    let drawPressureGraph data image =
        let (minPressure, maxPressure) = data |> minMaxBy (fun x -> x.AirPressureSeaLevel) 5.0<hPa>
        let pixelsPerhPa = 28.0<px> / (maxPressure - minPressure)
        let plotPressure timestamp pressure =
            (timeOffset timestamp, 108.0<px> - (pressure - minPressure) * pixelsPerhPa)
            |> drawPoint penNormalGraph <| image
        let plot image value =
            match value.AirPressureSeaLevel with
            | Some pressure -> plotPressure value.Timestamp pressure
            | None -> image
        List.fold plot image data

    let drawTemperatureGraph data image =
        let (minTemp, maxTemp) = data |> minMaxBy (fun x -> x.TemperatureAir) 5.0<degC>
        let pixelsPerDegree = 28.0<px> / (maxTemp - minTemp)
        let plotTemp timestamp temp =
            (timeOffset timestamp, 140.0<px> - (temp - minTemp) * pixelsPerDegree)
            |> drawPoint penNormalGraph <| image
        let plot image value =
            match value.TemperatureAir with
            | Some temp -> plotTemp value.Timestamp temp
            | None -> image
        List.fold plot image data

    let drawRelativeHumidity data image =
        let (minRh, maxRh) = data |> minMaxBy (fun x -> x.RelativeHumidity) 5.0<percent>
        let pixelsPerPercent = 28.0<px> / (maxRh - minRh)
        let plotRelativeHumidity timestamp rh =
            (timeOffset timestamp, 172.0<px> - (rh - minRh) * pixelsPerPercent)
            |> drawPoint penNormalGraph <| image
        let plot image value =
            match value.RelativeHumidity with
            | Some rh -> plotRelativeHumidity value.Timestamp rh
            | None -> image
        List.fold plot image data
    
    let drawPulses data image =
        let (minPulses, maxPulses) = data |> minMaxBy (fun x -> x.Pulses) 5.0<p>
        let pixelsPerPulse = 28.0<px> / (maxPulses - minPulses)
        let plotPulses timestamp pulses =
            let x = timeOffset timestamp
            drawLine penNormalGraph (x, 204.0<px>) (x, 204.0<px> - (pulses - minPulses) * pixelsPerPulse) image
        let plot image value =
            match value.Pulses with
            | Some p when p > 0.0<p> -> plotPulses value.Timestamp p
            | _ -> image
        List.fold plot image data

    let drawVisibility data image =
        let (minVis, maxVis) = data |> minMaxBy (fun x -> x.Visibility) 5.0<km>
        let pixelsPerKm = 28.0<px> / (maxVis - minVis)
        let plotVisibility timestamp vis =
            (timeOffset timestamp, 236.0<px> - (vis - minVis) * pixelsPerKm)
            |> drawPoint penNormalGraph <| image
        let plot image value =
            match value.Visibility with
            | Some vis -> plotVisibility value.Timestamp vis
            | None -> image
        List.fold plot image data

    let drawShortWave data image =
        let (minShort, maxShort) = data |> minMaxBy (fun x -> x.ShortWave) 10.0<W/m^2>
        let pixelsPerShortWave = 28.0<px> / (maxShort - minShort)
        let plotShortWave timestamp sw =
            (timeOffset timestamp, 268.0<px> - (sw - minShort) * pixelsPerShortWave)
            |> drawPoint penNormalGraph <| image
        let plot image value =
            match value.ShortWave with
            | Some sw -> plotShortWave value.Timestamp sw
            | None -> image
        List.fold plot image data

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