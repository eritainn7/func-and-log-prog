// Задание1
let printHelloWorld() = printfn "Hello, World!1"



//Задание 2
let SolveQuadEq a b c =
    let D = b * b - 4.0 * a * c
    match compare D 0 with
    | -1 -> []
    | 0 -> [-b/(2.0 * a)]
    | 1 -> [(-b + sqrt D)/(2.0 * a); (-b - sqrt D)/(2.0 * a)]

//Задание 3
open System

[<EntryPoint>]
let main argv=
    let RoundSquare r = Math.PI * r * r
    
    let VolumeCylinder r h =
        (RoundSquare r) * h

    printfn "%f" (VolumeCylinder 5.0 12.45)

    //Каррирование
    let VolumeCylinderFixedR = VolumeCylinder 5.0
    let volume = VolumeCylinderFixedR 12.45
    printfn "%f" volume

    0

//Задание 5 Рекурсия вниз хвостовая
let SumDigits num =
    let rec loop num sum_digits =
        match compare num 10 with
        | -1 -> sum_digits + num % 10
        | _ -> loop ((num - num % 10) / 10) (sum_digits + num % 10)
    loop num 0