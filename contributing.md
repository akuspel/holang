# Contributing to HoLang
While **HoLang** is in very early stages, contributions are still welcome and appreciated.

## Building
Building and running **HoLang** on *Windows* is a very simple affair - acquire the latest (nightly) 
release of Odin, and with its' root in PATH, run `build.bat`. This requires a `bin` directory in the 
project root.

This compiles and runs the project, and you should get builtin debug prints from the generated AST.

Once the project evolves, and building into a library / standalone interpreter become pressing matters, 
more build modes / flags will be added to help with the compilation step.

The project should compile well on *Linux* and other platforms, but it might require some fiddling around, 
if copying the commands from `build.bat` doesn't do it. Proper support TODO.

## Testing
Proper tests will be implemented with Odin's builtin testing functionality in the future. For now, 
the default build mode covers most language use cases, and should work OK as the language is yet to 
reach a usable state.

## How To Contribute
Contributing is simple!
1. Fork the repository, make a new branch
2. Do what you do (fixes, updates, functionality)
3. (Once applicable) update tests accordingly
4. Open a pull request, communicate your changes (and reasoning) clearly

## Code Style
**HoLang**'s code style differs from 'idiomatic' Odin code in certain key aspects.

### Naming
Underneath are listed certain rules when it comes to naming variables, types, etc.
```go
// --- Types ---
// Types in PascalCase by default
MyType :: struct { ... } 

// Certain types might have a prefix, E.G. AST nodes
AST_MyCustomNode :: struct { ... }

// Pointer aliases can be in UPPER_SNAKE_CASE
// NOTE: usually these are private to the package or file
@(private="file") MY_TYPE :: ^MyType


// --- General ---
// Variables and procedures in snake_case
my_var : int
my_proc :: proc(a : int, b : uint) -> (my_return : int) { ... }

// --- Enums ---
// Enum type names in PascalCase, as well as members by default
MyEnum :: enum {
	Default,
	Other,
	Another,
}

// Error enums in Whatever_This_Case_Is
MyError :: enum {
	None,
	Very_Bad_Error,
	Not_So_Bad_Error,
}
```

### Comments
Single line comments should start with a capital letter
`// This is a comment`

Certain comment types, like notes, todos, etc. follow certain conventions and alignment
```go
// --- Note Comments ---
// NOTE: this is a note, notice the
//       lack of capitals, and how
//       the text is aligned

// --- Todo Comments ---
// TODO: fix this certain thing,
//       very similar to notes

// --  Important Todo  --
// !!! TODO !!! fix this goddman thing!
//              as you can see, this is
//              extremely important

```

Categories serve to divide the code into clean and easily divisible chunks
```go
// All words in a category is capitalized

// --- Big Category ---
// A big category with three dashes is the biggest divider
// Often used to divide type, global, procedure, etc. sectors in a file


// --  Mid Category  --
// A mid category uses only two dashes, and is a sub of the previous big one

// -   Small Category   -
// Small categories are rarely necessary, but like the mid ones,
// They are children to the larger categories preceeding them
```

Short explaining code blocks should be provided at the top of each file, and before most 'important' procedures
```go
/* --- File Title ---
 * in this file we define X, do Y,
 * and handle all such things ...
 */

...

// --- Procedures ---

/* --- my_procedure ---
 * this procedure is very important
 * and does important things like uhh
 */
my_procedure :: proc() { ... }

```

### Alignation
In this repository, we align things as much as physically possible, while trying to keep our
sanity in check. This is no easy task. So here are a few guidelines.

#### Typing, Assignation
Unlike idiomatic Odin code, we align colons - with *minimum* one step after identifier - instead of types. Why? 
This makes the behaviour of `:` more consistant with `:=` and `::`. 
Also, types are automatically aligned after the colons, and big lines of colons are cool!

So, like this:
```go
a : int
b : int

thing : uint
other : f32
nope  : bool

MyStruct :: struct {
	a, b : int,
	mode : enum { None, Other, Another, },
	
	c : string,
}
```

And not like this:
```go
a: int
b: int

x:    f32
y:    f32
fact: bool
```

#### Names
We try to keep names close in length to each other when possible. This makes aligning look less stupid.
```go
// Looks fine
my_var  : int
another : int

// Looks atrocious
a    : string
more : int
// In such cases, not aligning colons might be preferable

b : string
thing : bool
```

### TLDR
Write code that looks nice to read, -ish! Aligning stuff is nice, but don't overdo it. 
Existing code should be (mostly) fine to copy in terms of style - do what you see there.