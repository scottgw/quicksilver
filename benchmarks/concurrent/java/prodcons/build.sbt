import AssemblyKeys._

name := "ProdCons"

version := "1.0"

autoScalaLibrary := false

logLevel := Level.Error

// Allows the use of JAR assembly
assemblySettings

jarName in assembly := "Main.jar"
