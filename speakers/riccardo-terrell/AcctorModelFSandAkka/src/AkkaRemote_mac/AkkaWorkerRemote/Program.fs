open Akka
open Akka.FSharp
open Akka.Configuration
open System

[<EntryPoint>]
let main argv =

    Console.Title <- sprintf "Remote Worker Actor - PID %d" (System.Diagnostics.Process.GetCurrentProcess().Id)
    
    let config = 
        Configuration.parse """
        akka {  
            log-config-on-start = on
            stdout-loglevel = DEBUG
            loglevel = DEBUG
            actor {
                provider = "Akka.Remote.RemoteActorRefProvider, Akka.Remote"
            }
            remote {
                helios.tcp {
                    transport-class = "Akka.Remote.Transport.Helios.HeliosTcpTransport, Akka.Remote"
                    transport-protocol = tcp
                    port = 8091
                    hostname = "10.211.55.2"
                }
            }
          } 
        """ 

    let config' = Configuration.parse """
        akka {
            log-config-on-start = on
            stdout-loglevel = DEBUG
            loglevel = DEBUG

            actor.provider = "Akka.Remote.RemoteActorRefProvider, Akka.Remote"
            remote.helios.tcp {
                hostname = 10.211.55.2
                port = 8091
            }
        }"""
          
    use system = System.create "worker" config'

    Console.ForegroundColor <- ConsoleColor.Red

    printfn "Remote Worker Actor %s listening..." system.Name

    Console.ForegroundColor <- ConsoleColor.Green

    Console.ReadLine() |> ignore
    0