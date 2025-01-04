open System.IO
open Fabulous.AST
open Fantomas.FCS
open Fantomas.FCS.Text

// using JavaPoet via IKVM .NET !yey
// latest version from palantir does not work so using the older one here
open com.squareup.javapoet

module FSharpLoader =
    
    let loadFile filePath =
        filePath
        |> File.ReadAllText
        |> SourceText.ofString
        |> fun source -> Parse.parseFile false  source []

module AstMapper =

    // TODO: read Fable.AST/oak and output a Java using poet 
    let fromFsharpToJava oakAst =
        // TODO:
        ()


module JavaWriter =
    open javax.lang.model.element
    
    let outputJava main =   

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

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
