namespace Optimus.Rollout
module ArgHelper =
    type CommandLineOptions = {
      dllPath : string;
      mainTemplatePath: string;
      controllerBlacklist: array<string>
    };

    let defaultOptions = {
        dllPath = "";
        mainTemplatePath = "";
        controllerBlacklist = [||]
    }

    let rec parseFromCommandLine (args: seq<string>) (optionsSoFar : CommandLineOptions) =
        match List.ofSeq args with
        | [] -> optionsSoFar
        | "-dll"::xs ->
            match xs with
            | path::xss when path.EndsWith(".dll") = true -> 
                let newOptionsSoFar = { optionsSoFar with dllPath=path }
                parseFromCommandLine xss newOptionsSoFar
            | _ -> failwith "Dll path not recognised"
        | "-template"::xs ->
            match xs with
            | path::xss when path.EndsWith(".optimus") = true ->
                let newOptionsSoFar = { optionsSoFar with mainTemplatePath = path }
                parseFromCommandLine xss newOptionsSoFar
            | _ -> failwith "Config path not recognised"
        | "-controllerBlacklist"::xs ->
            match xs with
            | blacklist::xss when blacklist.Length > 0 = true ->
                let newOptionsSoFar = { optionsSoFar with controllerBlacklist = blacklist.Split(';') }
                parseFromCommandLine xss newOptionsSoFar
            | _ -> failwith "Controller blacklist param invalid"
        |_ -> failwith "Unrecognised option"