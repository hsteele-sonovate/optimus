namespace Optimus.Prime

module Translation =
    open System
    open System.Text.RegularExpressions
    open Optimus.Prime.Types
    
    module Builder =

        let masterTemplate = """using System.Net.Http;
        using System.Threading.Tasks;

        namespace Optimus.Tests
        {
            public class Tests
            {
                private HttpClient client;   

                {methods}
            }
        }
        """

        let private getTemplate = """  public async Task<{endpoint.response}> {controller.name}_{endpoint.name}({endpoint.params})
        {
            var url = $"{endpoint.url}";
            var response = await client.GetAsync(url);
            var result = await response.Content.ReadAsAsync<{endpoint.response}>();
            return result;
        }"""

        let private sanitiseTypeName (name : string) =
            match name.Split('`') |> List.ofArray with
            | [] -> ""
            | head::tail -> head

        let rec private removeCharactersUntil (targetChar : char) (subject : string) =
            match subject with
            | s when s.StartsWith(targetChar.ToString()) -> subject
            | _ -> removeCharactersUntil targetChar (subject.[1..])

        let rec private sanitiseTypesInRouteUrl (url: string) : string =
            let parts = url.Split([|':'; '='|], 2)
            let result = match parts.Length with
                        | 1 -> url
                        | _ -> parts.[0] + removeCharactersUntil '}' parts.[1]
            match result.Contains(":") || result.Contains("=") with
            | true -> sanitiseTypesInRouteUrl result
            | false -> result

        let private getTypeName (t : System.Type) : string =
            match t with
            | i when i = typeof<Int32> -> "int"
            | i when i = typeof<Int64> -> "int"
            | i when i = typeof<String> -> "string"
            | i when i = typeof<string> -> "string"
            | _ -> sanitiseTypeName t.FullName

        let rec private rebuildGenericDefinition (t : System.Type) : string = 
            match t.IsGenericType with
            | false -> getTypeName t
            | true -> (getTypeName t) + "<" + (rebuildGenericDefinition (t.GetGenericArguments()).[0]) + ">"

        let private doParams (ps : seq<ParamType>) =
            let pss = ps
                    |> Seq.map (fun p -> (rebuildGenericDefinition p.Type) + " " + p.ParamName)
            String.Join(",", pss)

        let private generateEndpoint (template : string) (controllerName : string) (endpoint : Endpoint) : string =
            let mutable result = template
            result <- result.Replace("{controller.name}", controllerName)
            result <- result.Replace("{endpoint.name}", endpoint.Name)
            result <- result.Replace("{endpoint.url}", (sanitiseTypesInRouteUrl endpoint.Route))
            if (endpoint.ResponseType.IsSome) then 
                let s = (rebuildGenericDefinition endpoint.ResponseType.Value)
                result <- result.Replace("{endpoint.response.taskreturn}", "<" + s + ">")
                result <- result.Replace("{endpoint.response.httpreturn}", s)
                result <- result.Replace("{return}", "return result")
            else 
                result <- result.Replace("{endpoint.response.taskreturn}", "")
                result <- result.Replace("{endpoint.response.httpreturn}", "dynamic")
                result <- result.Replace("{return}", "return")
            
            result <- result.Replace("{endpoint.params}", doParams (Seq.append (if endpoint.BodyType.IsSome then [endpoint.BodyType.Value] else []) endpoint.RouteTypes))
            result

        let private matchToVerb (cntrllrName : string) (matches : seq<Match>) (endpoint : Endpoint) : string =
            let mtch = matches |> Seq.filter (fun m -> m.Groups.[2].Value.ToLower().Trim() = endpoint.Verb.ToString().ToLower())
            let x = match (Seq.isEmpty mtch) with
                    | true -> failwith "No matching verb template found"
                    | false -> Seq.head mtch
            generateEndpoint x.Groups.[3].Value cntrllrName endpoint

        let private mapController (matchColl : MatchCollection) (controller : Controller) : string =
            let matches : seq<Match> = Seq.cast matchColl
            String.Join("\n", (controller.Endpoints |> Seq.map (matchToVerb controller.Name matches)))
            

        let private buildEndpoints (template:string) (controllers:seq<Controller>) : string =
            let endpointRegex = "({endpoint)([^}]*)}(.*?){\/endpoint}"
            let before = template.Split([|"{endpoint"|], 2, StringSplitOptions.None);
            let after = template.Split([|"{/endpoint}"|], StringSplitOptions.None);
            let m = Regex.Matches(template, endpointRegex, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
            match m.Count > 0 with
            | true -> before.[0] + String.Join("\n", (controllers |> Seq.map (mapController m))) + (after |> Seq.rev |> Seq.head)
            | false -> failwith "Unable to parse template -- endpoint config invalid"

        let private buildControllerGroups (template:string) (api:ApiDefinition) : string * string =
            let controllerConfig = "{controller[^}]*}(.*){\/controller}"
            let m = Regex.Match(template, controllerConfig, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)
            match m.Success with
            | false -> (api.Name, (buildEndpoints template api.Controllers))
            | true -> failwith "Not done this yet!"
            //| true -> (List.map (fun c -> [c]) (List.ofSeq api.Controllers))

        //let generateFromController (controller: Controller) : seq<string> =
        //    let builder = generateEndpoint controller.Name
        //    controller.Endpoints
        //    |> Seq.filter (fun e -> e.Verb = HttpVerb.Get && e.ResponseType.IsSome)
        //    |> Seq.map builder      
            
        let build (template:string) (api:ApiDefinition) : string * string =
            buildControllerGroups template api

    let generateTestFramework (api : ApiDefinition) (template : string) : string * string = 
        //let endpointMethods = api.Controllers
        //                    |> Seq.map Builder.generateFromController
        //                    |> Seq.concat
        //                    |> Seq.toList
        //let mutable result = Builder.masterTemplate
        //result <- result.Replace ("{methods}", String.Join("\n", endpointMethods))
        //result
        Builder.build template api