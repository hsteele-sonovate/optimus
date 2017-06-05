namespace Optimus.Rollout
    open System
    open System.IO
    module FileHelper =
        let getFileContents (path:string) : string =
            let lines = seq {
                    use sr = new StreamReader (path)
                    while not sr.EndOfStream do
                    yield sr.ReadLine ()
                }
            String.Join("\n", lines)