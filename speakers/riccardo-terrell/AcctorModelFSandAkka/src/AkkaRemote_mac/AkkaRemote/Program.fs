#if INTERACTIVE
#r @"..\..\bin\Akka.dll"
#r @"..\..\bin\Akka.FSharp.dll"
#r @"..\..\bin\Akka.Remote.dll"
#r @"..\..\bin\FSharp.PowerPack.dll"
#endif

open Akka.FSharp
open Akka.Actor
open Akka.Remote
open Akka.Configuration
open System

[<EntryPoint>]
let main argv = 
    
    // NO REFERENCE

    let config = Configuration.parse """
        akka {
            log-config-on-start = on
            stdout-loglevel = DEBUG
            loglevel = DEBUG

            actor.provider = "Akka.Remote.RemoteActorRefProvider, Akka.Remote"
            remote.helios.tcp {
                hostname = 192.168.1.9 #10.211.55.2
                port = 9234
            }
        }"""


    Console.Title <- sprintf "Remote Actor - PID %d" (System.Diagnostics.Process.GetCurrentProcess().Id)

    use remoteSystem = System.create "remote-system" config

    Console.ForegroundColor <-
     ConsoleColor.Red

    printfn "Remote Actor %s listening..." remoteSystem.Name

    System.Console.ReadLine() |> ignore

    remoteSystem.Shutdown()

    0 // return an integer exit code
    