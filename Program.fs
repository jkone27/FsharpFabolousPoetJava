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

module AstMapper =

    open javax.lang.model.element

    // TODO: read Fable.AST/oak and output a Java using poet 
    let fromFsharpToJava oakAst =

        let main = 
            MethodSpec
                .methodBuilder("main")
                .addCode("")
                .build()

        // TODO:
        let helloWorld = 
            TypeSpec
                .classBuilder("HelloWorld")
                .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
                .addMethod(main)
                .build()

        let javaFile =  
            JavaFile
                .builder("com.example.helloworld", helloWorld)
                .build()

        javaFile

module Test = 

    let fsharpSample = 
        """
        pritnfn "hello java!"
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




Test.getOak() 
|> CodeFormatter.FormatOakAsync 
|> Async.RunSynchronously
|> printfn "%s"

Test.parse() 
|> _.ToString()
|> printfn "%s"
