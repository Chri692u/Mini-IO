# MiniIO - A Runtime System for Shell-like Languages

MiniIO is a runtime system designed for shell-like languages, implemented in Haskell with a C backend. It provides a controlled environment for performing IO operations, encapsulating them within the `World` data type and the `WorldT` and `WorldM` monads.

## Core Features

- **World State Management**: The `World` data type represents the state of the world, including the current working directory and environment variables.

- **IO Operations**: MiniIO provides a set of core IO functions that interact with the operating system via the Foreign Function Interface (FFI). These functions include:

    - `printStrM`: Prints a string to the console.
    - `readStrM`: Reads a string from the user input.
    - `getCwdM`: Retrieves the current working directory.
    - `listDirectoryM`: Lists the contents of a directory.
    - `changeDirectoryM`: Changes the current working directory.
    - `getFileContentM`: Reads the contents of a file.
    - `declareVar`: Declares a variable in the environment.
    - `createFileM`: Creates a file in the working directory.
    - `removeFileM`: Removes a file in the working directory.

- **Safety**: The use of `WorldT` and `WorldM` monads provides a safe interface, encapsulating the IO operations and world state. It should be noted that MiniIO is a runtime system for shell-like languages and the side maintain no transparency. Developers should be careful of branching states. Mechanisms such as uniqueness typing is recommended.

- **Extendable library**: ...

## Getting Started
To be written

### Example `app/LambdaIO`
The provided Haskell code demonstrates the usage of MiniIO in a simple interpreter for a lambda calculus extended with built-in functions. The built-in functions are `VPrint` and `VRead` which are used to interact with the outside world.

The example demonstrates how to use MiniIO to perform IO operations in a controlled environment, encapsulating them within the `World` data type and the monads to implement a little language with side-effects

Run the example: `cabal run lambda-io`

## Extending the runtime system
To be written