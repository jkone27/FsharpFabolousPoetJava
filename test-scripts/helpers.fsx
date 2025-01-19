#r "nuget:Fabulous.AST"
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text
open Fabulous.AST
open System

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

    //      module Hello
    //
    //      let hello () =
    //         "hello world"
    //
    //     : https://fsprojects.github.io/fantomas-tools
    //  
    //      Oak (1,0-4,17)
    //         ModuleOrNamespaceNode (1,0-4,17)
    //             ModuleOrNamespaceHeaderNode (1,0-1,12)
    //                 MultipleTextsNode (1,0-1,6)
    //                     module (1,0-1,6)
    //                 IdentListNode (1,7-1,12)
    //                     Hello (1,7-1,12)
    //             Newline (2,0-2,0)
    //             BindingNode (3,0-4,17)
    //                 MultipleTextsNode (3,0-3,3)
    //                     let (3,0-3,3)
    //                 IdentListNode (3,4-3,9)
    //                     hello (3,4-3,9)
    //                 UnitNode (3,10-3,12)
    //                     ( (3,10-3,11)
    //                     ) (3,11-3,12)
    //                     = (3,13-3,14)
    //                     "hi" (4,4-4,17)

    // define a function to extract the module Name from an AST or None if anonymous
    //     Oak (1,0-1,9)
    // ModuleOrNamespaceNode (1,0-1,9)
        // BindingNode (1,0-1,9)
        // MultipleTextsNode (1,0-1,3)
        // let (1,0-1,3)
        // IdentListNode (1,4-1,5)
        // x (1,4-1,5)
        // = (1,6-1,7)
        // 5 (1,8-1,9)

    // check all places where a module can be in Oak before suggesting this function,
    // should this use a inner recursive helper?


// namespace a.b.c

// module MODULE1 =

//     let x = 1
    // Home
    // AST
    // Oak
    // Fantomas
    // Oak (1,0-5,13)
    // ModuleOrNamespaceNode (1,0-5,13)
    //      ModuleOrNamespaceHeaderNode (1,0-1,15)
    //          MultipleTextsNode (1,0-1,9)
    //              namespace (1,0-1,9)
    //          IdentListNode (1,10-1,15)
    //              a (1,10-1,11)
    //              b (1,12-1,13)
    //              c (1,14-1,15)
    // Newline (2,0-2,0)
    // NestedModuleNode (3,0-5,13)
    //      module (3,0-3,6)
    // IdentListNode (3,7-3,14)
    //      MODULE1 (3,7-3,14)
    // = (3,15-3,16)
    // Newline (4,0-4,0)
    // BindingNode (5,4-5,13)
    // MultipleTextsNode (5,4-5,7)
    // let (5,4-5,7)
    // IdentListNode (5,8-5,9)
    // x (5,8-5,9)
    // = (5,10-5,11)
    // 1 (5,12-5,13)

module FantomasReader = 
    
    let fcsMkTreeFromCode codeSample =
        let (result, diag)  = 
            codeSample
            |> SourceText.ofString 
            |> fun src -> Fantomas.FCS.Parse.parseFile false src []
        result

    let oakToFcs (oak: Oak) = 
        oak |> Gen.run |> fcsMkTreeFromCode

    let namespaceOrModulenameFromListNode (iln: IdentListNode) =
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


    let getHeaderName kind (header: ModuleOrNamespaceHeaderNode) =
        match header.LeadingKeyword.Content with
        | [x] when x.Text = kind -> 
            header.Name 
            |> Option.bind namespaceOrModulenameFromListNode
        | _ -> None

    
    // Define a function to extract the namespace from an Oak AST
    // modules can also be inside namespaces! make this recursive
    let extractNamespaceFromOak (oak: Oak) =
        oak.ModulesOrNamespaces.Head.Header
        |> Option.bind (getHeaderName "namespace")

    let extractModuleFromOak (oak: Oak) =
        let topLevelModule () = // TOP LEVEL MODULE
            oak.ModulesOrNamespaces.Head.Header
            |> Option.bind (getHeaderName "module")

        // NESTED MODULE (first in namespace)
        let nestedModule () = 
            oak 
            |> extractNamespaceFromOak
            |> Option.bind (fun _ -> 
                oak.ModulesOrNamespaces.Tail 
                |> Seq.tryPick (fun x -> 
                    x.Header
                    |> Option.bind (getHeaderName "module")
                )
            )
        
        topLevelModule () 
        |> Option.orElse (nestedModule ())


module Tests = 

    open Fabulous.AST
    open type Fabulous.AST.Ast

    let oakAstNamespace = 
        Ast.Oak(){
            Ast.Namespace("hello.one.two") {}
        }
        |> Gen.mkOak

    let nsTest () = 
        oakAstNamespace 
        |> FantomasReader.extractNamespaceFromOak 
        |> Option.iter (printfn "EXTRAXTED NAMESPACE IS: %s")


    let oakAstModule = 
        Ast.Oak(){
            // this was the fix for the above issue, adding AnonymousModule
            Ast.AnonymousModule() {
                Ast.Module("HelloModule") { }
            }
        }
        |> Gen.mkOak

    let moduleTest () =
        oakAstModule 
        |> FantomasReader.extractModuleFromOak 
        |> Option.iter (printfn "EXTRAXTED MODULE IS: %s")


// RUN TESTS here
Tests.nsTest()
Tests.moduleTest()