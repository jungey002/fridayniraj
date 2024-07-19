// For more information see https://aka.ms/fsharp-console-apps
let rec productListHelper acc lst =
    match lst with
    | [] -> acc
    | head :: tail -> productListHelper (acc * head) tail
let productList lst =
    productListHelper 1 lst
let listOfItems = [2; 3; 4] // Example list of integers
let result = productList listOfItems
printfn "Product of all elements: %d" result
 


 //product of all odd number 
let productOfOdd (n: int) =
    let rec helper (current: int) (acc: int) =
        if current <= 0 then acc
        else if current % 2 = 1 then helper (current - 2) (acc * current)
        else helper (current - 1) acc
    helper n 1
let result = productOfOdd 11
printfn "The product of all odd numbers from 11 to 1 is %d" result
 
 //Map Operation 
let trimTheGivenSpace (givenList: string list) =
    givenList |> List.map (fun x -> x.Trim())
let list = [" Charles"; "Babbage  "; "  Von Neumann  "; "  Dennis Ritchie  "]
let trimmedNames = trimTheGivenSpace list
trimmedNames |> List.iter (printfn "%s")

//Sequence
let sequence = seq {1 .. 700}
let numberList = Seq.toList sequence
let filteredList = List.filter (fun x -> x % 35 = 0) numberList
let sumOfFilteredNumbers = List.fold (+) 0 filteredList
printfn "%A" filteredList
printfn "The sum of all numbers that are multiples of both 7 and 5 is %d" sumOfFilteredNumbers

//Concatenation
let names: string list = ["James"; "Robert"; "John"; "William"; "Michael"; "David"; "Richard"]
let filteredNames: string list =
    List.filter (fun name -> name.Contains("I", System.StringComparison.OrdinalIgnoreCase)) names
let concatenatedNames: string =
    List.fold (fun acc name -> acc + name) "" filteredNames
printfn "Concatenated names containing 'I': %s" concatenatedNames

