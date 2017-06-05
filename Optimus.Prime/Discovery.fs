namespace Optimus.Prime

module Discovery =
    open System.Reflection
    open System.IO
    open Optimus.Utils    
    open Optimus.Prime.Types

    // Operations to fetch the DLL and load it into the appdomain for reflection
    module AssemblyLoader =
        
        let private getRootPath (path : string) =
            path.Split('/')
            |> Array.toList
            |> ListHelpers.allButLast   
            |> String.concat "/"    

        let private initialiseHandler path = 
            let handler = System.ResolveEventHandler(fun _ args ->        
                let shortName = Seq.head (args.Name.Split(','))        
                let sanitisedPath = getRootPath path
                let path = sanitisedPath + "/" + shortName + ".dll"
                match File.Exists(path) with
                | true -> Assembly.ReflectionOnlyLoadFrom(path)
                | false -> Assembly.ReflectionOnlyLoad(args.Name)
                )
            System.AppDomain.CurrentDomain.add_ReflectionOnlyAssemblyResolve(handler)

        let loadDllFromPath (path : string) =
            initialiseHandler path
            Assembly.ReflectionOnlyLoadFrom(path)
            

    // Operations to find controllers, endpoints and key API attributes
    module ApiMapper =

        exception UnrecognisedVerb of string

        let private mapVerb verbString = 
            match verbString with
            | "HttpGetAttribute" -> HttpVerb.Get
            | "HttpPostAttribute" -> HttpVerb.Post
            | "HttpPutAttribute" -> HttpVerb.Put
            | "HttpDeleteAttribute" -> HttpVerb.Delete
            | _ -> raise (UnrecognisedVerb "Unable to match Http verb attribute")

        let private getVerb (method : System.Reflection.MethodInfo) =
            let verbs = Set.ofList ["HttpGetAttribute"; "HttpPostAttribute"; "HttpPutAttribute"; "HttpDeleteAttribute"]
            method.GetCustomAttributesData()
            |> Seq.map (fun attr -> attr.AttributeType.Name)
            |> Set.ofSeq
            |> Set.intersect verbs
            |> Seq.head
            |> mapVerb

        let private constructEndpoint (method : System.Reflection.MethodInfo) (routePrefix : string) : Endpoint =
            let isPrimitive (t : System.Type) : bool = List.contains t Consts.primitiveTypes
            let verb = getVerb method
            let args = method.GetParameters()
            let bodyParam = args
                            |> Array.filter (fun p -> ((TypeHelpers.paramHasAttribute "FromBodyAttribute" p) || (not (TypeHelpers.paramHasAttribute "FromUriAttribute" p) && not (isPrimitive p.ParameterType) && not (p.ParameterType.IsEnum))))
                            |> Array.map (fun p -> {Type = p.ParameterType; ParamName = p.Name})
                            |> List.ofArray                            
                            |> ListHelpers.firstOrNothing
            let uriParams = args
                            |> Array.filter (fun p -> ((TypeHelpers.paramHasAttribute "FromUriAttribute" p) || ((not (TypeHelpers.paramHasAttribute "FromBodyAttribute" p)) && (isPrimitive p.ParameterType || p.ParameterType.IsEnum))))
                            |> Array.map (fun p -> {Type = p.ParameterType; ParamName = p.Name})
                            |> List.ofArray
            let route = match TypeHelpers.getAttributeFromMethod method "routeattribute" with
                | Some r -> routePrefix + "/" + TypeHelpers.getFirstCtorArgument r
                | None -> routePrefix
            let responseType = match (TypeHelpers.getAttributeFromMethod method "responsetypeattribute" ) with
                | Some r -> Some (TypeHelpers.getFirstCtorArgument<System.Type> r)
                | None -> None
            { Name = method.Name; Route = route; Verb = verb; ResponseType = responseType; BodyType = bodyParam; RouteTypes = uriParams }
            
        let private constructEndpoints (controller:System.Type) (routePrefix : string) : seq<Endpoint> =
            let methods = controller.GetMethods(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
            methods
            |> Array.toSeq 
            |> Seq.filter (fun method -> not method.IsSpecialName)
            |> Seq.map (fun e -> constructEndpoint e routePrefix)
        
        let filterControllers (ignoreTypes : seq<string>) (types : System.Type[]) =
            let checkIsController (t : System.Type) =
                not t.IsAbstract && t.IsPublic && t.Name.ToLower().EndsWith("controller") && not (Seq.exists (fun elem -> elem = t.FullName) ignoreTypes)
            types
            |> Seq.filter (checkIsController)
        
        let private constructControllers (t : System.Type) : Controller =
            let route = match TypeHelpers.getAttributeFromType t "routeprefixattribute" with
                        | Some r -> TypeHelpers.getFirstCtorArgument<string> r
                        | None -> ""
            let endpoints = constructEndpoints t route       
            { Name = t.Name; Endpoints = endpoints }

        let constructApiDefinition (ignoreTypes : seq<string>) (assembly : System.Reflection.Assembly)  =
            let controllers = assembly.GetExportedTypes() |>  filterControllers ignoreTypes |> Seq.map constructControllers 
            { Name = "Gateway"; Controllers = controllers}
    
    // Operations to explore the type tree and explicitly identify all relevant types
    module TypeLoader =
        open System
        open System.Collections.Generic
        
        let private isMappable (t : System.Type) : bool =
            let ignoredType = [typedefof<Dictionary<_,_>>; typedefof<List<_>>; typedefof<IEnumerable<_>>; typedefof<ISet<_>>; typedefof<IDictionary<_,_>>; typedefof<Array>; typedefof<IList<_>>; typedefof<Nullable<_>>]
            
            (not (Seq.contains t ignoredType)) && (not (Seq.contains t Consts.primitiveTypes)) && (not (t.Name = null))

        let rec private unwrap (t : System.Type) : list<System.Type> = 
            match t.IsGenericType with
            | true -> let l = t.GenericTypeArguments
                            |> Array.toList
                            |> List.map (fun g -> unwrap g)
                            |> List.concat
                      t.GetGenericTypeDefinition() :: l
                      |> List.filter isMappable
            | false -> [t]

        let private flattenTypes (api : ApiDefinition) = 
            let endpoints = api.Controllers |> Seq.collect (fun ctrl -> ctrl.Endpoints)
            Seq.append (Seq.collect (fun ep -> if ep.BodyType.IsSome then [ep.BodyType.Value.Type] else []) endpoints) (Seq.collect (fun ep -> (ep.RouteTypes |> Seq.map (fun r -> r.Type))) endpoints)
            |> Seq.append (endpoints |> Seq.map (fun e -> e.ResponseType) |> Seq.choose id)
            |> Seq.map unwrap
            |> Seq.concat
            |> Seq.distinctBy (fun t -> t.FullName)
            |> Seq.filter isMappable
            |> Seq.toList

        let private explodeTypesByProperty (t : System.Type) =
            t.GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
            |> Array.toList
            |> List.map (fun ty -> ty.PropertyType)
            |> List.distinctBy (fun t -> t.AssemblyQualifiedName)
            |> List.filter isMappable
        
        let private getProps (t : System.Type) : list<Type> =
            explodeTypesByProperty t
            |> List.filter isMappable
            

        let loadTypes (api : ApiDefinition) =
            let mutable types = api
                                |> flattenTypes
            let mutable newTypes = types
                                |> List.map getProps
                                |> Seq.toList
                                |> List.concat
                                |> List.map unwrap
                                |> List.concat
                                |> List.filter (fun i -> not (Seq.contains i types))
            while (not (List.isEmpty newTypes)) do
                types <- List.append types newTypes
                newTypes <- newTypes
                            |> List.map getProps   
                            |> List.concat
                            |> List.map unwrap
                            |> List.concat
                            |> List.filter (fun i -> not (List.contains i types))
            types
            

    let fromDllPath (path : string) (ignoreTypes : seq<string>) : ApiDefinition * list<System.Type> =
        let apiDefinition = AssemblyLoader.loadDllFromPath path
                            |> ApiMapper.constructApiDefinition ignoreTypes
        let typeList = apiDefinition
                        |> TypeLoader.loadTypes
        (apiDefinition, typeList)