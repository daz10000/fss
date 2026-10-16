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
    /// Make a random 256-bit key as 64 lower-case hex characters, from a cryptographically secure RNG.
    /// (Previously used System.Random, which is predictable and not thread-safe.)
    let genRandKey() =
        RandomNumberGenerator.GetBytes(32) |> Convert.ToHexString |> fun s -> s.ToLowerInvariant()

    let csumToString (csum:byte array) = String.Concat(csum |> Array.map (fun b -> sprintf "%02x" b))
