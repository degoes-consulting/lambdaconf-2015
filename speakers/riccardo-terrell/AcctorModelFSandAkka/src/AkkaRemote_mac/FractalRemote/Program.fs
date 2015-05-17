open Akka
open Akka.FSharp
open Akka.Configuration
open System

[<EntryPoint>]
let main argv =

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
                    port = 8090
                    hostname = localhost
                }
            }
          } 
        """   
    use system = System.create "worker" config

    Console.ReadLine() |> ignore
    0    