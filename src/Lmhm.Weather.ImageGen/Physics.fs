module Lmhm.Weather.ImageGen.Physics

open System

[<Measure>] type degC   // °C
[<Measure>] type m      // meter
[<Measure>] type mm     // millimeter
[<Measure>] type s      // second
[<Measure>] type hPa    // hectoPascal

type Measuring<[<Measure>]'u> = 
  { Value: float<'u>
    Time: DateTime }
