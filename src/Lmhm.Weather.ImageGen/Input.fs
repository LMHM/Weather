module Lmhm.Weather.ImageGen.Input

open System
open System.IO

[<Measure>] type deg     // Degrees 0 - 360
[<Measure>] type degC    // °C
[<Measure>] type m       // meter
[<Measure>] type mm      // millimeter
[<Measure>] type km      // kilometer
[<Measure>] type s       // second
[<Measure>] type hPa     // hectoPascal
[<Measure>] type percent // Relative humidity %
[<Measure>] type W       // Watt
[<Measure>] type p       // Pulse

type Measuring<[<Measure>]'u> = 
  { Value: float<'u>
    Time: DateTime }

type SensorData =
  { Timestamp : DateTime                        // tim, min
    WindSpeed : float<m/s> option               // fa2
    WindDirection : float<deg> option           // da2
    WindSpeedMax : float<m/s> option            // fx2
    WindHistoryMaxSpeed : float<m/s> option     // fx10
    WindHistoryMinSpeed : float<m/s> option     // fm10
    WindHistoryMaxDirection : float<deg> option // dx10
    WindHistoryMinDirection : float<deg> option // dm10
    AirPressureSeaLevel: float<hPa> option      // qff
    AirPressureChange : float<hPa> option       // p3
    RelativeHumidity  : float<percent> option   // rh
    TemperatureAir : float<degC> option         // t1i
    TemperatureGround : float<degC> option      // t3i
    DewPoint  : float<degC> option              // td
    ShortWave : float<W/m^2> option             // kvn
    LongWave : float<W/m^2> option              // lvn
    Precipitation : float<mm> option            // d1
    Pulses : float<p> option                    // d3
    Visibility: float<km> option }              // sikt1 + sikt2 / 2
    // Unused data: p, t2i, kvu, lvu, a5, a6, a7, d2

module private Datafiles =

    let filePath path pattern (date: DateOnly) =
        let filename = date.ToString("yyMMdd") |> sprintf pattern
        Path.Combine(path, filename)

    let limit<[<Measure>]'u> min max value =
        if min <= value && value <= max then
            LanguagePrimitives.FloatWithMeasure<'u> value |> Some
        else
            None

    let toSensorData (date: DateOnly) (line: string) =
        let values = line.Split(',' , StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        { Timestamp = DateTime(date.Year, date.Month, date.Day, int values[1], int values[2], 0)
          WindSpeed = float values[3] |> limit<m/s> 0 100
          WindDirection = float values[4] |> limit<deg> 0 360
          WindSpeedMax = float values[5] |> limit<m/s> 0 100
          WindHistoryMaxSpeed = float values[6] |> limit<m/s> 0 100
          WindHistoryMinSpeed = float values[7] |> limit<m/s> 0 100
          WindHistoryMaxDirection = float values[8] |> limit<deg> 0 360
          WindHistoryMinDirection = float values[9] |> limit<deg> 0 360
          AirPressureSeaLevel = float values[11] |> limit<hPa> 500 2000
          AirPressureChange = float values[12] |> limit<hPa> -1000 1000
          RelativeHumidity = float values[13] |> limit<percent> 0 100
          TemperatureAir = float values[14] |> limit<degC> -100 100
          TemperatureGround = float values[16] |> limit<degC> -100 100
          DewPoint  = float values[17] |> limit<degC> -100 100
          ShortWave = float values[18] |> limit<W/m^2> 0 1000
          LongWave = float values[19] |> limit<W/m^2> 0 1000
          Precipitation = float values[25] |> limit<mm> 0 5000
          Pulses = float values[27] |> limit<p> 0 1000
          Visibility = ((float values[28]) + (float values[29]) / 2.0) |> limit<km> 0 1000 }

    let readDay path date =
        let fileName = filePath path "MM%s.TXT" date
        if File.Exists(fileName) then
            File.ReadAllLines(fileName)
            |> Array.map (toSensorData date)
            |> Array.toList
        else
            List.Empty

let read path (fromDate: DateOnly) (toDate: DateOnly) =
    [for dayNumber = fromDate.DayNumber to toDate.DayNumber do
        yield! DateOnly.MinValue.AddDays(dayNumber) |> Datafiles.readDay path]
