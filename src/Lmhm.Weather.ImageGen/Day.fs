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

    let maxSpanBy<[<Measure>]'u> (projection : Input.SensorData -> float<'u> option) modulo span data =
        let maximum =
            data 
            |> List.choose projection
            |> List.max
            |> modUp modulo
        (maximum - span, maximum)

    let formatValue<[<Measure>]'u> (projection : Input.SensorData -> float<'u> option) format novalue data =
        match (List.last data) |> projection with
        | Some v -> v |> float |> sprintf format
        | None -> novalue

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
            |> drawText label Color.LightGreen (12.0<px>, offsetY))

    let last24hourOffsets() =
        let hourNow = float DateTime.Now.Hour * 1.0<h>
        let minuteOffset = (float DateTime.Now.Minute * 1.0<minute>) * pixelsPerMinute
        let toHourOffset hour = (hour, (hour - hourNow) * pixelsPerHour - minuteOffset)
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
        let current = formatValue (fun x -> x.WindDirection) "%3.0f" "---" data
        let plotDirection image value =
            match value.WindDirection with
            | Some direction -> plot penNormalGraph value.Timestamp (44.0<px> - (direction * pixelsPerDegree)) image
            | None -> image
        List.fold plotDirection image data
        |> drawText current Color.White (466.0<px>, 18.0<px>)

    let drawWindSpeedGraph data image =
        let maxMax =
            data 
            |> List.choose (fun x -> x.WindSpeedMax)
            |> List.max
            |> modUp 5.0<m/s>
            |> max 20.0<m/s>
        let pixelsPerMeterPerSecond = 28.0<px> / maxMax
        let currentAvg = formatValue (fun x -> x.WindSpeed) "%4.1f" " ---" data
        let currentMax = formatValue (fun x -> x.WindSpeedMax) "%4.1f" " ---" data
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
        let (minPressure, maxPressure) = data |> maxSpanBy (fun x -> x.AirPressureSeaLevel) 5.0<hPa> 20.0<hPa>
        let pixelsPerhPa = 28.0<px> / (maxPressure - minPressure)
        let current = formatValue (fun x -> x.AirPressureSeaLevel) "%6.1f" "  ---" data
        let currentChange = formatValue (fun x -> x.AirPressureChange) "%+4.1f" " ---" data
        let range = sprintf "[%4.0f-%4.0f]" minPressure maxPressure
        let plot image value =
            match value.AirPressureSeaLevel with
            | Some pressure -> plot penNormalGraph value.Timestamp (108.0<px> - (pressure - minPressure) * pixelsPerhPa) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (442.0<px>, 82.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 98.0<px>)
        |> drawText currentChange Color.White (532.0<px>, 82.0<px>)

    let drawTemperatureGraph data image =
        let (minTemp, maxTemp) = data |> maxSpanBy (fun x -> x.TemperatureAir) 5.0<degC> 20.0<degC>
        let pixelsPerDegree = 28.0<px> / (maxTemp - minTemp)
        let current = formatValue (fun x -> x.TemperatureAir) "%4.1f" " ---" data
        let currentGround = formatValue (fun x -> x.TemperatureGround) "%4.1f" " ---" data
        let range = sprintf "[%+2.0f-%+2.0f]" minTemp maxTemp
        let plot image value =
            match value.TemperatureAir with
            | Some temp -> plot penNormalGraph value.Timestamp (140.0<px> - (temp - minTemp) * pixelsPerDegree) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (458.0<px>, 114.0<px>)
        |> drawText currentGround Color.White (576.0<px>, 114.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 130.0<px>)

    let drawRelativeHumidity data image =
        let (minRh, maxRh) = (20.0<percent>, 100.0<percent>)
        let pixelsPerPercent = 28.0<px> / (maxRh - minRh)
        let current = formatValue (fun x -> x.RelativeHumidity) "%2.0f" "--" data
        let dewPoint = formatValue (fun x -> x.DewPoint) "%4.1f" " ---" data
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
        let maxPulses = 400.0<p>
        let pixelsPerPulse = 28.0<px> / maxPulses
        let current = formatValue (fun x -> x.Pulses) "%3.0f" "---" data
        let range = sprintf "[0-%3.0f]" maxPulses
        let plotPulses timestamp pulses =
            let x = timeOffset timestamp
            drawLine penNormalGraph (x, 204.0<px>) (x, 204.0<px> - pulses * pixelsPerPulse) image
        let plot image value =
            match value.Pulses with
            | Some p when p > 0.0<p> -> plotPulses value.Timestamp (min p maxPulses)
            | _ -> image
        List.fold plot image data
        |> drawText current Color.White (466.0<px>, 178.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 192.0<px>)

    let drawVisibility data image =
        let maxVis = 20.0<km>
        let pixelsPerKm = 28.0<px> / maxVis
        let current = formatValue (fun x -> x.Visibility) "%2.0f" "--" data
        let range = sprintf "[0-%2.0f]" maxVis
        let plot image value =
            match value.Visibility with
            | Some vis -> plot penNormalGraph value.Timestamp (236.0<px> - (min vis maxVis) * pixelsPerKm) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (472.0<px>, 210.0<px>)
        |> drawText range Color.LightGray (350.0<px>, 226.0<px>)

    let drawShortWave data image =
        let maxShort = 1000.0<W/m^2>
        let pixelsPerShortWave = 28.0<px> / maxShort
        let current = formatValue (fun x -> x.ShortWave) "%3.0f" "---" data
        let range = sprintf "[0-%1.0f]" maxShort
        let plot image value =
            match value.ShortWave with
            | Some sw -> plot penNormalGraph value.Timestamp (268.0<px> - (min sw maxShort) * pixelsPerShortWave) image
            | None -> image
        List.fold plot image data
        |> drawText current Color.White (466.0<px>, 242.0<px>)
        |> drawText range Color.LightGray (536.0<px>, 242.0<px>)

    let drawGraphs data image =
        image
        |> drawTimeScales
        |> drawWindDirectionGraph data
        |> drawWindSpeedGraph data
        |> drawPressureGraph data
        |> drawTemperatureGraph data
        |> drawRelativeHumidity data
        |> drawPulses data
        |> drawVisibility data
        |> drawShortWave data

    let drawTimestamp data image =
        let currentTime = (List.last data).Timestamp
        image
        |> drawText (currentTime.ToString("yyyy-MM-dd")) Color.Orange (552.0<px>, 18.0<px>)
        |> drawText (currentTime.ToString("HH:mm")) Color.Orange (576.0<px>, 32.0<px>)

    let drawPrecipitation data image =
        let last hours =
            data
            |> List.filter (fun x -> x.Timestamp > DateTime.Now.AddHours(-hours))
            |> List.choose (fun x -> x.Precipitation)
            |> List.sum
            |> sprintf "%3.0f"
        image
        |> drawText (last 3) Color.White (584.0<px>, 162.0<px>)
        |> drawText (last 6) Color.White (584.0<px>, 178.0<px>)
        |> drawText (last 12) Color.White (584.0<px>, 194.0<px>)
        |> drawText (last 24) Color.White (584.0<px>, 210.0<px>)

    let drawWindDirectionMeter data image =
        let current = formatValue (fun x -> x.WindDirection) "%03.0f" "---" data
        let drawIndicator pen (minLength : float<px>) (maxLength : float<px>) angle =
            let xStart = 75.0<px> + (Math.Sin(angle) * minLength)
            let yStart = 390.0<px> - (Math.Cos(angle) * minLength)
            let xStop = 75.0<px> + (Math.Sin(angle) * maxLength)
            let yStop = 390.0<px> - (Math.Cos(angle) * maxLength)
            drawLine pen (xStart, yStart) (xStop, yStop) image
        let drawDirection = drawIndicator penNormalGraph 12.0<px> 52.0<px>
        let drawMinMax = drawIndicator penSolidTick 32.0<px> 52.0<px>
        let drawMeter (projection : Input.SensorData -> float<deg> option) draw data image =
            match (List.last data) |> projection with
            | Some v -> draw (v * Math.PI / 180.0<deg>)
            | None -> image
        image
        |> drawText current Color.White (64.0<px>, 385.0<px>)
        |> drawMeter (fun x -> x.WindDirection) drawDirection data
        |> drawMeter (fun x -> x.WindHistoryMaxDirection) drawMinMax data
        |> drawMeter (fun x -> x.WindHistoryMinDirection) drawMinMax data

    let drawMeters data image =
        image
        |> drawWindDirectionMeter data

    let generateImage data =
        loadImage "weather_template.png"
        |> drawGraphs data
        |> drawTimestamp data
        |> drawPrecipitation data
        |> drawMeters data

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