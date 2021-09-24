module Houses

    type House = {Address: string; Price: decimal}
    type PriceBrand =
        | Cheap
        | Medium
        | Expensive

    let getHouses count =
        let random = System.Random(Seed = 1)
        Array.init count (fun i -> 
            { Address = sprintf "%i Stochastic Street" (i+1) 
              Price = random.Next(50_000, 500_000) |> decimal })

    let random = System.Random(Seed = 1)

    let trySchoolDistance (house: House) =
        let dist = random.Next(10) |> double
        if dist < 8. then
            Some dist
        else
            None
    
    let priceBand ({Price = price}) =
        match price with
        | i when i < 100_000m -> Cheap
        | i when i < 200_000m -> Medium
        | i when i >= 200_000m -> Expensive

    let houses = 
        getHouses 20
        |> Array.map (fun h -> sprintf "Address: %s - Price: %f" h.Address h.Price)

    let avg =
        getHouses 20
        |> Array.averageBy (fun h -> h.Price)

    let over250k =
        getHouses 20
        |> Array.filter (fun h -> h.Price > 250_000m)

    let housesNearSchoolsMyAnswer =
        getHouses 20
        |> Array.map (fun h -> 
            match trySchoolDistance h with
            | None -> None
            | Some d -> Some (h.Address, d))
        |> Array.choose (fun f -> f)
        
    let housesNearSchoolsExerciseAnswer =
        getHouses 20
        |> Array.choose (fun h -> 
            match h |> trySchoolDistance with
            | None -> None
            | Some d -> Some (h.Address, d))
    
    let printHouses =
        Array.iter (fun h -> printfn "%s %f" h.Address h.Price)

    let housesOverAmount amount =
        getHouses 20
        |> Array.filter (fun h -> h.Price > amount)
    
    let printOver100kSorted =
        getHouses 20
        |> Array.filter (fun h -> h.Price > 100_000m)
        |> Array.sortBy (fun h -> h.Price)
        |> printHouses
        
    let avgOver200k =
        housesOverAmount 200_000m
        |> Array.averageBy (fun h -> h.Price)
        
    let firstLessThan100k =
        getHouses 20
        |> Array.filter (fun h -> h.Price < 100_000m)
        |> Array.choose (fun t -> 
            match trySchoolDistance t with
            | Some r -> Some(t, r)
            | None -> None)
        |> Array.tryHead
        
    
    let housesByPriceBand =
        getHouses 20
        |> Array.map (fun d -> (priceBand d, d))
        |> Array.groupBy (fun (priceBand,_) -> priceBand)
        |> Array.map (fun (k ,v) -> k, v |> Array.sortBy (fun (_, c) -> c.Price))

    let inline tryAverageBy f (a: 'T[]) =
        if a.Length = 0 then 
            None
        else
            a |> Array.averageBy f |> Some

    let avgerageOver200K = 
        getHouses 20
        |> Array.filter (fun f -> f.Price >= 200_000m)
        |> tryAverageBy (fun f -> f.Price)

    let firstHouseLessThan100kWithSchoolDistance =
        getHouses 20
        |> Array.filter (fun h -> h.Price < 100_000m)
        |> Array.choose (fun h -> 
            match trySchoolDistance h with
            | Some s -> 
            {|
            Address = h.Address
            Price = h.Price
            Distance = s 
            |} |> Some
            | None -> None)
        |> Array.head

    let firstHouseLessThan100kWithSchoolDistanceFromExercise =
        getHouses 20
        |> Array.filter (fun h -> h.Price < 100_000m)
        |> Array.tryPick (fun h -> 
            match trySchoolDistance h with
            | Some s -> Some(h,s)
            | None -> None)