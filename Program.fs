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
    open Fantomas.Core.SyntaxOak
    
    let fsharpStringToOak str =
        let parsed, _ = 
            str
            |> SourceText.ofString
            |> fun source -> Parse.parseFile false  source []

        parsed |> CodeFormatter.TransformAST

    let fcsMkTreeFromCode codeSample =
        let (result, diag)  = 
            codeSample
            |> SourceText.ofString 
            |> fun src -> Fantomas.FCS.Parse.parseFile false src []
        result

    let oakToFcs (oak: Oak) = 
        oak |> Gen.run |> fcsMkTreeFromCode

        
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
    open Fantomas.FCS.Syntax

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

    // input: Fabolous.AST/oak --> output: Java code, using poet 
    let fromFsharpToJava (oakAst : SyntaxOak.Oak) =

        let nsJava = 
            match oakAst with
            | NamespaceNode(ns) -> ns.ToLowerInvariant()
            | _ -> "com.example.helloworld"

        // traverse oakAst and convert a code block to java codeblock
        let javaCodeBlockFromSynExpr (node: SynExpr) : CodeBlock.Builder = 
            let builder = CodeBlock.builder()
                    
            let rec convertNode (node: SynExpr) (builder: CodeBlock.Builder) : CodeBlock.Builder =
                match node with
                | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
                    let mutable b = builder
                    for binding in bindings do
                        match binding with
                        | SynBinding(_, _, _, _, _, _, _, headPat, _, expr, _, _, _) -> 
                            let name = headPat.ToString()
                            let value = expr.ToString()
                            b <- builder.addStatement(format = $"var {name} = {value};")
                    convertNode body b
                | SynExpr.App(_, _, SynExpr.Ident(ident), args, _) when ident.idText = "printfn" ->
                    let code = sprintf "System.out.println(%s);" (args.ToString())
                    builder.addStatement(code)
                | SynExpr.App(_, _, SynExpr.Ident(ident), _, _) when ident.idText = "System.Console.ReadLine" ->
                    let code = "new Scanner(System.in).nextLine();"
                    builder.addStatement(code)
                | _ -> failwith "not implemented"

            let finalB = convertNode node builder
            finalB

        let parsedInputToSynExpr (fcsAst: ParsedInput) =
            match fcsAst with
            | ParsedInput.ImplFile(implFile) -> implFile.Contents
            | _ -> failwith "Expected an implementation file"

        let oakToSynExpr (oak: Oak) =
            let pi = oak |> FSharpLoader.oakToFcs
            parsedInputToSynExpr pi

        let synModules = oakToSynExpr oakAst
        let javaCodeBlock = 
            synModules 
            |> List.collect (fun synModule -> 
                match synModule with
                | SynModuleOrNamespace(_, _, _, decls, _, _, _, _, _) ->
                    decls |> List.collect (fun decl -> 
                        match decl with
                        | SynModuleDecl.Let(_, bindings, _) ->
                            bindings
                        | _ -> []
                    )
            )
            |> List.map (fun binding -> 
                match binding with
                | SynBinding(_, _, _, _, _, _, _, _, _, expr, _, _, _) -> javaCodeBlockFromSynExpr expr
            )
            |> List.reduce (fun acc block ->  acc.addStatement(block.build().toString()))
    
           
        let main = 
            MethodSpec
                .methodBuilder("main")
                .addCode(javaCodeBlock.build())
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
