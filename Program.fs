open System
open System.IO
open Fabulous.AST
open Fantomas.FCS
open Fantomas.FCS.Text
open Fantomas.Core

// using JavaPoet via IKVM .NET !yey
// latest version from palantir does not work so using the older one here
open com.squareup.javapoet

module FSharpLoader =
    
    let fsharpStringToOak str =
        let parsed, _ = 
            str
            |> SourceText.ofString
            |> fun source -> Parse.parseFile false  source []

        parsed |> CodeFormatter.TransformAST
        
module FantomasReader = 

    open Fantomas.Core.SyntaxOak

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


module AstMapper =

    open javax.lang.model.element
    open Fantomas.Core.SyntaxOak
    open System.Text.RegularExpressions

    // Active pattern to match Oak node for a namespace string
    let (|NamespaceNode|_|) (oak: Oak) =
        let program = (oak |> Gen.run)
        let m = Regex.Match(program, "namespace\s+(w+)")
        if m.Success then
            Some m.Groups.[0].Value
        else
            None

    let (|ModuleNode|_|) (oak: Oak) =
        let program = (oak |> Gen.run)
        let m = Regex.Match(program, "module\s+(w+)")
        if m.Success then
            Some m.Groups.[0].Value
        else
            None

    let (|TopLevelCodeBlock|_|) (oak: Oak) =
        let program = (oak |> Gen.run)
        let m = Regex.Match(program, "(w+)")
        if m.Success then
            Some m.Groups.[0].Value
        else
            None


    // input: Fabolous.AST/oak --> output: Java code, using poet 
    let fromFsharpToJava (oakAst : SyntaxOak.Oak) =

        let nsJava = 
            match oakAst with
            | NamespaceNode(ns) -> ns.ToLowerInvariant()
            | _ -> "com.example.helloworld"

        // TODO: traverse oakAst and assemble classes and methods in the fs file, based on oak AST code
        let javaCodeBlock = 
            match oakAst with
            | TopLevelCodeBlock(codeBlockOak) -> 
                codeBlockOak |> translateToJavaCodeString
            | _ -> failwith "no top level method found!"

                
        let main = 
            MethodSpec
                .methodBuilder("main")
                .addCode(javaCodeBlock)
                .build()

        let classJava = 
            match oakAst with
            | ModuleNode(moduleName) -> moduleName
            | _ -> "HelloWorld"

        let helloWorld = 
            TypeSpec
                .classBuilder(classJava)
                .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
                .addMethod(main)
                .build()

        let javaFile =  
            JavaFile
                .builder(nsJava, helloWorld)
                .build()

        javaFile


module HelloWorldTest = 

    let fsharpSample = 
        """
        printfn "hello java!"
        """

    let javaExpectation = 
        """
        System.out.println("hello java!");
        """

    let getOak() =
        fsharpSample 
        |> FSharpLoader.fsharpStringToOak

    let parse () =
        getOak ()
        |> AstMapper.fromFsharpToJava




// HelloWorldTest.getOak() 
// |> CodeFormatter.FormatOakAsync 
// |> Async.RunSynchronously
// |> printfn "%s"

HelloWorldTest.parse() 
|> _.ToString()
|> printfn "%s"
