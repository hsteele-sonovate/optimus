module Optimus.Utils

    module ListHelpers =
        let allButLast l =
            match l with
            | [] -> []
            | l -> l |> List.rev |> List.tail |> List.rev

        let firstOrNothing (s : seq<'t>) : Option<'t> = 
            if Seq.isEmpty s then None else Some (Seq.head s)

    module TypeHelpers =
        let typeHasAttribute (t:System.Type) (attributeName:string) : bool =
            t.GetCustomAttributesData()
            |> Seq.filter (fun (att) -> att.AttributeType.Name.ToLower().Equals(attributeName))
            |> Seq.length > 0

        let getAttributeFromType  (t:System.Type) (attributeName:string) : Option<System.Reflection.CustomAttributeData> =
           
            t.GetCustomAttributesData()
            |> Seq.filter (fun attr -> attr.AttributeType.Name.ToLower().Equals attributeName )
            |> ListHelpers.firstOrNothing        

        let getAttributeFromMethod  (t:System.Reflection.MethodInfo) (attributeName:string) : Option<System.Reflection.CustomAttributeData> =
            t.GetCustomAttributesData()
            |> Seq.filter (fun attr -> attr.AttributeType.Name.ToLower().Equals attributeName )
            |> ListHelpers.firstOrNothing

        let getFirstCtorArgument<'T> (attr:System.Reflection.CustomAttributeData) : 'T =
            let arg =attr.ConstructorArguments |> Seq.head 
            arg.Value
            :?> 'T

        let paramHasAttribute (attrName : string) (p: System.Reflection.ParameterInfo)  : bool =
            let attrs = p.GetCustomAttributesData() |> List.ofSeq
            if ( List.isEmpty attrs)
            then false
            else List.contains attrName ( List.map (fun (a : System.Reflection.CustomAttributeData) -> a.AttributeType.Name) attrs)
            

    module Consts = 
        open System
        let primitiveTypes = [typedefof<string>; typedefof<Guid>; typedefof<Single>; typedefof<int>; typedefof<Int64>; typedefof<decimal>; typedefof<float>; typedefof<bool>; typedefof<DateTime>; typedefof<TimeSpan>]

        let logo = "───────────▄▄▄▄▄▄▄▄▄───────────
────────▄█████████████▄────────
█████──█████████████████──█████
▐████▌─▀███▄───────▄███▀─▐████▌
─█████▄──▀███▄───▄███▀──▄█████─
─▐██▀███▄──▀███▄███▀──▄███▀██▌─
──███▄▀███▄──▀███▀──▄███▀▄███──
──▐█▄▀█▄▀███─▄─▀─▄─███▀▄█▀▄█▌──
───███▄▀█▄██─██▄██─██▄█▀▄███───
────▀███▄▀██─█████─██▀▄███▀────
───█▄─▀█████─█████─█████▀─▄█───
───███────────███────────███───
───███▄────▄█─███─█▄────▄███───
───█████─▄███─███─███▄─█████───
───█████─████─███─████─█████───
───█████─████─███─████─█████───
───█████─████─███─████─█████───
───█████─████▄▄▄▄▄████─█████───
────▀███─█████████████─███▀────
──────▀█─███─▄▄▄▄▄─███─█▀──────
─────────▀█▌▐█████▌▐█▀─────────
────────────███████────────────"    