(*
   FSS - F# Web Server
   Copyright 2012 Darren Platt

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

*)

namespace Fss
module Sec =
    open System
    open System.Security.Cryptography
    let rng = new System.Random()

    /// Make a random string.  Might want to add other sources of noise into this to really mix things up in a robust implementation
    let genRandKey() =
        use sha = new SHA256Managed()
        sha.Initialize()

        let bytes = Array.init 100 (fun _ -> 0uy)
        rng.NextBytes(bytes)

        let hash = sha.ComputeHash(bytes)
        seq { for h in hash ->sprintf "%02x" h } |> String.concat ""

    let csumToString (csum:byte array) = String.Concat(csum |> Array.map (fun b -> sprintf "%02x" b))
