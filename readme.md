# ![Holang Logo](res/logo.svg) HoLang - Typed Interpreted Scripting Language
**HoLang**, **.ho**, or more formally, **Homeshift Language**,
is a WIP interpreted language written in Odin.

Its creation was the result of annoyance from a lack of existing scripting languages with *STRONG* typing and simple syntax. The end goal is to create an easily embeddable language, whose capabilities will serve well for creating Gameplay scripts for *Homeshift*, an easily moddable 2D platformer in the works by *HarjuTales*.

Because safety is boring, HoLang supports *dangerous* pointers and pointer arithmetic (TODO), though there are guardrails in place to prevent accessing memory outside of the VM or dereferencing null pointers (insert meme here).

## Features

Thus far, only the Tokeniser is in a near-complete state. The last remaining feature is String Literal tokenisation.

In addition to the Tokeniser, the Parser is beginning to take shape, with the after mentioned systems (almost) fully implemented:
- Type definitions
	- Structs (nested)
	- Arrays
	- Pointers
	- References (unique names for existing type)
- Constant definitions
	- Single value
	- Constant expressions (can parse existing constants)

The parser is developing with great speed, next upcoming variable declarations? (no promises)

Planned features that will take a while to develop:
- Commands
	- Runtime AST
	- Constant VS Runtime
- Functions
	- Multiple return values
- Importing
	- Borrowed VM's?
- FFI
	- Foreign function definition API
	- Foreign memory? (and types)
- Any proper code running


## Syntax
The syntax of **HoLang** borrows elements from **Odin**, **C**, and the family favourite - **Zig**!
Example:
```go
/* --- HoLang Example ---
 * an example of valid HoLang code
 * 
 *	  --- NOTE ---
 * the code currently passes*
 * the	Parsing step up 'till
 * // <-- Works Until
 * !!! unless marked with !!!
 * // <-- Doesn't work
 * 
 * (* this doesn't mean it does anything,
 *	only that the Parser recognises the
 *	syntax )
 */

// --- Types ---
#type u8 = unique byte; // Unique Reference to byte, needs a cast to assign <--->

#type Vec2 = [2]float;
#type Vec3 = [3]float;
#type Vec4 = [4]float;

#type Color = Vec4; // Non-unique reference, no casting needed!
#type Color255 = [4]u8;

#const STRING_BUF_SIZE = 1024;
#type StringBuf = [STRING_BUF_SIZE + 1]byte; // Extra for null character
#type cstring = ^byte; // Null terminated string

// --- Struct Stuff ---
#type Vertex = struct {
	pos : Vec3,
	col : Color,
}

#type Triangle = [3]Vertex;
#const MAX_TRINGLES = 2048;
#type TrianleBuff = [MAX_TRIS]Triangle;

#type Mesh = struct {
	col : Color,
	
	n_tris : int,
	tris : TriangleBuff,
};

// --- Global Variables ---
var my_global : int; // Defaults to zero
var MY_FAKE_CONSTANT : immutable uint = 75;

// Constant expressions are evaluated
// During the parsing step
var a : float = 10.0 - 12 / (MAX_TRIANGLES - 512); // Evaluated during parsing
var b : float = (a + float(MY_FAKE_CONSTANT)) * 2; // Evaluated at runtime

// <-- Works until
// --- Functions ---
fn clamp(v : int, min : int, max : int) -> int {
	if (v < min) return min;
	if (v > max) return max;
	return v;
}

fn max(a : int, b : int) -> (c : int) {
	c = a;
	if (b > a) c = b;
	return; // Auto return named returns
}

fn new_mesh(tris : int, col : Color255) -> Mesh {
	
	var mesh : Mesh;
	mesh.col = {
		float(col[0]) / 256,
		float(col[1]) / 256,
		float(col[2]) / 256,
		float(col[3]) / 256,
	};

	mesh.n_tris = clamp(tris, 0, MAX_TRIANGLES);
	
	return mesh;
}

fn mesh_add_tri(mesh : &Mesh, tri : Triangle) {
	if (mesh.n_tris >= MAX_TRIANGLES) return;
	
	// References can be mutated
	mesh.tris[mesh.n_tris] = tri;
	mesh.n_tris += 1;
	
	// You may be asking:
	//   " if you have references, (safe btw)
	//     the hell you need pointers for? "
	//
	// And the answer is...
	// Well, cause I want them!
	// Also cause we can do beloved
	// C-style strings with ease (IO)
}

// Horrible concept that hopefully won't get implemented:
//     Methods!

// Method parents are automatically a reference
fn set_color <self : Mesh> (col : Color255) {
	
	self.col = {
		float(col[0]) / 256,
		float(col[1]) / 256,
		float(col[2]) / 256,
		float(col[3]) / 256,
	};
}

// Would be called like:
//     var MY_COLOR : immutable Color255 =
//         {0, 255, 100, 255};
//
//     my_mesh.set_color(MY_COLOR);
//
// Semantically identical to:
//     fn set_color(mesh : &Mesh, col : Color255) { ... }
//     set_color(my_mesh, MY_COLOR);
//
// So, this probably never gets implemented!
// NOTE: immutable variables can't be passed as references!
//     Works again -->

// --- Entry Point ---
// Execution begins from an entry block, where
// Defined variables are local to that block,
// And can't be accessed globally
entry {
	var x : int = 1;
	var y : int = 2;
	var z : int =
		-(x + y) * 13 + x * 2 + y;

	// <-- Works until
	// You can write logic in an entry frame
	for (z < x + y) {
		z += 1 + max(x, y);
		if (mod(z, 2) == 0) {
			y -= 1;
		}
	}
	
	print(x, y, z, clamp(z, y, x));

	// If we only want en entry function, we can call
	// One in an entry block, E.G. entry { main() }
	var my_mesh = new_mesh(100, { 255, 100, 53, 200 });
	mesh_add_tri(my_mesh, {});
	// The parser checks that ref members
	// Are a valid variable of given type
}


// Nice example, say I so myself!
```

After this short and concise explanation and example, you must too understand why **HoLang** is the future of interpreted scripting languages. Imagine using something that actually works, hah!
