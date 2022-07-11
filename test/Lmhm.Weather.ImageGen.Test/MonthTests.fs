module Month.Tests

open System
open FsUnit.Xunit
open Xunit
open Lmhm.Weather.ImageGen.Physics
open Lmhm.Weather.ImageGen.Month

[<Fact>]
let ``readDay should read data from file`` () =
    let date = DateOnly(2022, 05, 19)

    Input.readDay "./" date |> should equal {
      Date = date
      TempAvg = Some 17.2<degC>
      TempMin = Some { Value = 10.3<degC>; Time = date.ToDateTime(TimeOnly(01, 17)) }
      TempMax = Some { Value = 24.3<degC>; Time = date.ToDateTime(TimeOnly(12, 03)) }
      WindMax = Some { Value = 5.9<m/s>; Time = date.ToDateTime(TimeOnly(12, 20)) }
      PressureMin = Some { Value = 1013.4<hPa>; Time = date.ToDateTime(TimeOnly(22, 23)) }
      PressureMax = Some { Value = 1021.5<hPa>; Time = date.ToDateTime(TimeOnly(00, 00)) }
      Rain = Some 3.0<mm>
    }
