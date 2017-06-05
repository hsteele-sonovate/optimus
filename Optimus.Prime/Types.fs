namespace Optimus.Prime

module Types =
    type HttpVerb = Get | Post | Put | Patch | Delete
    type Type = { Type:System.Type }
    type ParamType = { Type:System.Type; ParamName:string}
    type Endpoint = { Name:string; Route:string; Verb:HttpVerb; ResponseType:Option<System.Type>; BodyType:Option<ParamType>; RouteTypes:seq<ParamType> }
    type Controller = { Name:string; Endpoints:seq<Endpoint> }
    type ApiDefinition = { Name:string; Controllers:seq<Controller> }
