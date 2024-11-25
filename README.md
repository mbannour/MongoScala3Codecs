# MongoScala3Codecs: A Macro-Based BSON Codec Generator for Scala

![mongoScala3Codecs version](https://img.shields.io/badge/mongoScala3Codecs-0.0.1-brightgreen)
![mongoScala3Codecs compatibility](https://img.shields.io/badge/Scala-3.0%2B-blue)
`MongoScala3Codecs` is a lightweight and powerful library designed to simplify BSON serialization and deserialization for Scala case classes. It leverages Scala 3 macros to generate BSON codecs at compile time, providing type-safe and performant handling of BSON data. This library is particularly useful when working with MongoDB in Scala.

## Compatibility

- **Scala 3**: This library is compatible only with Scala 3, leveraging its advanced macro capabilities for compile-time codec generation.


## Features
- **Compile-Time Validation**: Ensures only valid case classes can have codecs generated.
- **Automatic Codec Generation**: Avoids boilerplate for encoding and decoding BSON.
- **Nested Case Class Support**: Handles complex nested structures.
- **Integration with MongoDB**: Compatible with MongoDB's `CodecRegistry` and BSON codecs.


## How It Works

The library leverages Scala 3 macros to analyze case classes at compile time and generates efficient BSON codecs. The macro ensures:

- **Validation**: The target type is a valid case class.
- **Encoding and Decoding**: All fields, including nested ones, are properly encoded and decoded.
- **MongoDB Compatibility**: The generated codecs conform to MongoDB's `Codec` interface and are fully compatible with `CodecRegistry`.

## Supported Features

- **Primitive Types**: Handles standard Scala primitives like `Int`, `Double`, `String`, etc.
- **Custom Types**: Supports `ObjectId` and other MongoDB-specific types.

---

## Limitations

- **Non-Case Classes**: Only case classes are supported; non-case classes are not compatible.
- **Scala 3 Requirement**: The library relies on Scala 3 macros for codec generation and requires a Scala 3 project.



---

## Installation

To include MongoScala3Codecs in your Scala project, add the following dependency:

```scala
libraryDependencies += "m.bannour" %% "mongoScala3Codecs" % "0.0.1"
```



