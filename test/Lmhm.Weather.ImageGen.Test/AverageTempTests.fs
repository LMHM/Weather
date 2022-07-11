module AverageTemp.Tests

open System
open System.IO
open FsUnit.Xunit
open Xunit
open Lmhm.Weather.ImageGen.Physics
open Lmhm.Weather.ImageGen.AverageTemp

[<Fact>]
let ``readCurrent should read data from MTSum.txt`` () =
    File.WriteAllText("./MTSum.txt", " 10097.8       758          220520")

    Input.readCurrent "./" |> should equal {
        Sum = 10097.8<degC>
        Count = 758
        Date = DateOnly(2022, 05, 20) }

[<Fact>]
let ``readUpdate should read temperature from VDATA.TXT`` () =
    Input.readUpdate "./" |> should equal 14.8<degC>

[<Fact>]
let ``writeAverage should write new MT-file with average`` () =
    let data = {
        Sum = 100.0<degC>
        Count = 10
        Date = DateOnly(2022, 05, 30) }

    Output.writeAverage "./" data

    File.ReadAllText("./MT220530.TXT") |> should equal " 10.0"

[<Fact>]
let ``writeCurrent should update MTSum.txt`` () =
    let data = {
        Sum = 100.0<degC>
        Count = 10
        Date = DateOnly(2022, 05, 30) }
    
    Output.writeCurrent "./" data

    File.ReadAllText("./MTSum.txt") |> should equal "      100.0         10 220530"