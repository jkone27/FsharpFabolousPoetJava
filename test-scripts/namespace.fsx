#r "nuget:Fabulous.AST"
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text
open System
open Fabulous.AST
open type Fabulous.AST.Ast

(** `namespace A.B.C` : https://fsprojects.github.io/fantomas-tools
    Oak (1,0-1,15)
        ModuleOrNamespaceNode (1,0-1,15)
            ModuleOrNamespaceHeaderNode (1,0-1,15)
                MultipleTextsNode (1,0-1,9)
                    namespace (1,0-1,9)
                IdentListNode (1,10-1,15)
                    A (1,10-1,11)
                    B (1,12-1,13)
                    C (1,14-1,15)
*)

module FantomasReader = 
    let extractNs (iln: IdentListNode) =
        iln.Content 
        |> List.collect (fun iod ->  
            match iod with
            | IdentifierOrDot.Ident(stn) -> [ stn.Text ]
            | _ -> []
        )
        |> fun l -> 
            if l.Length > 0 then
                String.Join('.',l) |> Some
            else
                None


    // Define a function to extract the namespace from an Oak AST
    let extractNsFromOak (oak: Oak) =
        oak.ModulesOrNamespaces.Head.Header
        |> Option.bind (
            fun header -> 
                match header.LeadingKeyword.Content with
                | [x] when x.Text = "namespace" -> 
                    header.Name 
                    |> Option.bind extractNs
                | _ -> None
        )

let oakAst = 
    Ast.Oak(){
        Ast.Namespace("hello.one.two") {}
    }
    |> Gen.mkOak

oakAst 
|> FantomasReader.extractNsFromOak 
|> Option.iter (printfn "EXTRAXTED NAMESPACE IS: %s")
