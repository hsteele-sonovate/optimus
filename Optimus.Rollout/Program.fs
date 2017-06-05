// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Optimus.Prime;
open Optimus.Rollout;

let showSplash =
    printfn "%s" Optimus.Utils.Consts.logo
    printfn "%s" ""
    printfn "%s" "OPTIMUS PRIME / API TRANSFORMER"
    printfn "%s" "- Rolling out..."

[<EntryPoint>]
let main argv = 
    showSplash
    let opts = ArgHelper.parseFromCommandLine argv ArgHelper.defaultOptions
    let template = FileHelper.getFileContents opts.mainTemplatePath
    let api, types = Discovery.fromDllPath opts.dllPath  opts.controllerBlacklist
    printfn "- %s" "API mapping complete!"
    printfn "- %i controllers discovered..." (Seq.length api.Controllers)
    printfn "- %i endpoints discovered..." (api.Controllers |> Seq.collect (fun c -> c.Endpoints) |> Seq.length)
    printfn "- %i types discovered..." types.Length
    let mapped = Translation.generateTestFramework api template
    0 // return an integer exit code


