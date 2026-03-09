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

//Задание 4 Рекурсия вверх
let rec UpSumDigits num =
    match compare num 10 with
    | -1 -> num % 10
    | _ -> (num % 10) + UpSumDigits((num - num % 10) / 10) 

//Задание 5 Рекурсия вниз хвостовая
let SumDigits num =
    let rec loop num sum_digits =
        match compare num 10 with
        | -1 -> sum_digits + num % 10
        | _ -> loop ((num - num % 10) / 10) (sum_digits + num % 10)
    loop num 0

//Задание 6
let fact num =
    let rec loop num acc =
        match compare num 2 with
        | -1 -> acc
        | _ -> loop (num - 1) (num * acc)
    loop num 1

let SumDigitORfact flag =
    match flag with
    | false -> fact
    | _ -> SumDigits

printfn "%i" (SumDigitORfact false 5) // 120
printfn "%i" (SumDigitORfact true 5) // 5
printfn "%i" (SumDigitORfact false 16) // 2004189184
printfn "%i" (SumDigitORfact true 16) // 7

//Задание 7
let HandlingNum num handling_func flag = 
    let flag = (flag = 1)
    handling_func flag num

//Задание 9
let Handler num (F: int -> int -> int) (G: int -> bool) =
    match G num with
    | true -> F num 3
    | _ -> F 3 num