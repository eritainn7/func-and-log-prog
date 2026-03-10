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

// [<EntryPoint>]
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

//Задание 10
let F x y = 
    match compare x y with
    | -1 -> fact x
    | _ -> fact y

let G x = x % 2 = 0

printfn "%i" (Handler 15 F G)
printfn "%i" (Handler 2 F G)
printfn "%i" (Handler 7 F G)

//Задание 11
let isLoveLang lang =
    match lang with
    | lang when lang = "F#" || lang = "Prolog" -> printfn "Подлиза вы!))"
    | _ -> printfn "А почему?"
isLoveLang "F#"    

//Задание 12 Каррирование и суперпозиция
let main1 func arg=
    func arg
//Каррирование
let MiddlewareFunc = main1 isLoveLang
MiddlewareFunc "Python"

//Задание 13
let rec gcd a b =
    if b = 0 then a
    else gcd b (a % b)

let traverseCoprimesCompact n f init =
    [1..n-1] 
    |> List.filter (fun x -> gcd x n = 1)
    |> List.fold f init

//Задание 14
let eulerPhi n =
    traverseCoprimesCompact n (fun acc _ -> acc + 1) 0
printfn "%i" (eulerPhi 12)

//Задание 15
let getDigits n =
    let rec loop acc remaining =
        if remaining = 0 then acc
        else loop (abs (remaining % 10) :: acc) (remaining / 10)
    loop [] n

let traverseCoprimeDigits n f init condition =
    n
    |> getDigits
    |> List.distinct
    |> List.filter (fun d -> condition d && gcd d n = 1)
    |> List.fold f init

printfn "%i" (traverseCoprimeDigits 12345 (+) 0 (fun _ -> true))