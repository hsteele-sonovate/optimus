namespace Optimus.Prime
open System.Reflection
open Optimus.Prime.Types
    module Discovery =
        val fromDllPath : string -> seq<string> -> ApiDefinition * list<System.Type>