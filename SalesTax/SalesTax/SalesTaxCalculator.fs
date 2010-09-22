#light
open System
open System.Collections.Generic

let GetQtyFromInput(input: string) = 
    match input.Split(' ').Length with
    | 0 -> failwith ("Bad/No input to process")
    | _ -> 
        try
            int (input.Split(' ').[0])
        with
            | :? System.FormatException -> failwith ("Bad input to process")

let GetPriceFromInput(input: string) = 
    match input.Split(" at ".ToCharArray()).Length with
    | 0 -> failwith ("Bad/No input to process")
    | _ -> 
        try
            float (input.Replace(" at ","|").Split('|').[1]) //Refactor
        with
            | :? System.FormatException -> failwith ("Bad input to process")

let GetNameOfProduct(input: string) = 
    if input.Contains(" ") && input.Contains(" at ") then
        let start = input.IndexOf(' ')
        let length = input.IndexOf(" at ")
        input.Substring(start,length-start)
    else
        String.Empty
        
let RoundTax(tax: float)=
    Math.Round((Math.Round(tax*100.0,MidpointRounding.AwayFromZero)/100.0)*20.0)/20.0

let (|Contains|) keyword (input: string) = 
    input.ToLower().Contains(keyword)
    
//Ideally this information should be looked up against a file/data store - possible candidate for ReFactor
let IsExempt = function
    | Contains "book" true -> true
    | Contains "chocolate" true -> true
    | Contains "pills" true -> true
    | _ -> false
    
let IsImported (input: string) = 
    input.Contains "imported"

let mutable TotalSalesTax = 0.0
let mutable TotalCostPrice = 0.0

let computeTax (input: float, exempt: bool, imported: bool) = 
    let mutable tax = 0.0
    if exempt=false then
        tax <- input * 0.1
    tax <-tax +
            match imported with
                |true -> input * 0.05
                |_ -> 0.0
    RoundTax(tax)
    
let computeTotal(input: string) =
    let costPrice =GetPriceFromInput(input)
    TotalCostPrice <- TotalCostPrice + costPrice 
    let tax = computeTax(costPrice,IsExempt(input),IsImported(input))
    TotalSalesTax <- TotalSalesTax + tax
    costPrice + tax
    
let PrintOutput(input: string)=
    printf "%d %s: %.2f\n" (GetQtyFromInput(input)) (GetNameOfProduct(input)) (computeTotal(input))
    
let rec PrintTotal(list: List<string>)=
    match list.Count with
    | 0 -> ()
    | 1 -> PrintOutput(list.[0])
    | x -> 
        PrintOutput(list.[0])
        PrintTotal(list.GetRange(1,x-1)) 
    
let inputList = new List<string>()
printf "Enter the products purchased:\n"
let mutable input=" "
while input<>"" do
    input <-Console.ReadLine()
    if input <> "" then
        inputList.Add(input)

try
    PrintTotal(inputList)
    printf "Sales Tax: %.2f\n" TotalSalesTax
    printf "Total: %.2f\n" (TotalCostPrice+TotalSalesTax)
with
    Failure(msg) -> printfn "%s" msg

Console.ReadLine() |>ignore