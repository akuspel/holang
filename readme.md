# ![Holang Logo](res/logo.svg) HoLang - Typed Interpreted Scripting Language
**HoLang**, **.ho**, or more formally, **Homeshift Language**,
is a WIP interpreted language written in Odin.

Its creation was the result of annoyance from a lack of existing scripting languages with *STRONG* typing and simple syntax. The end goal is to create an easily embeddable language, whose capabilities will serve well for creating Gameplay scripts for *Homeshift*, an easily moddable 2D platformer in the works by *HarjuTales*.

Because safety is boring, HoLang supports *dangerous* pointers and pointer arithmetic, though there are guardrails in place to prevent accessing memory outside of the VM or dereferencing null pointers (insert meme here).

### Raw Scopes
One of HoLang's provided safety features are *raw scopes*. The compiler prevents access to certain data, and pointer operations (deref, as_ptr) outside of raw scopes, which can be defined with the keyword `raw;`, after which the whole rest of the given scope is raw.
```go
entry {
	// Defaults to not raw
	
	raw { /* This scope is raw */ }
	
	// Not raw again
	
	raw {
		// Another raw scope
		raw { // Error! Scope is already raw
			
		}
	}
}
```

Certain types (arrays, structs) can be marked as opaque, which prevents access to its data without using a raw scope. Furthermore pointers can't be dereferenced or gotten outside of raw scopes. Example:
```go
#type MySecrets = opaque struct {
	_secret_int : int,
	_scary_bool : bool,
	_data_ptr   : byte_ptr
};

#type MyStruct = struct {
	value : int,
	data  : MySecrets
};

entry {
	
	var my_var : MyStruct;
	my_var.value = 10;
	
	// Trying to access opaque data will result in an error
	my_var.data._secret_int = my_var.value + 1; // Error!
	
	// These can still be assigned with other such variables
	var secret : MySecrets;
	my_var.data = secret; // No error!
	
	// A pointer to a byte, defaults to a null pointer
	var my_ptr : byte_ptr;
	var a_byte : byte;
	var anotha : byte = 102;
	var athird : byte = 105;
	
	// Can't get variable pointers outside of raw scopes
	my_ptr  = as_ptr(byte_ptr, a_byte); // Error!
	my_ptr += 10; // Pointer arithmetic can still be performed
	
	// If we want to access the data itself, we'll need to make the scope raw
	raw {
		// Now we can access the data freely
		my_var.data._secret_int = my_var.value + 1; // No error!
		
		// We can do as many pointer operations as we want!
		my_ptr = as_ptr(byte_ptr, a_byte);
		my_var.data._data_ptr = as_ptr(byte_ptr, anotha) + 1;
	}
	
	// Any attempt to dereference outside of raw will end horribly
	deref(byte_ptr, my_ptr) = 10; // Error!
	
	raw {
		
		// Now this scope is raw, we can do what we want
		deref(byte_ptr, my_ptr) = 10;
		var x : int = int(
			byte, // Cast from a byte to int
			deref(byte_ptr, my_var.data._data_ptr) +
			deref(byte_ptr, my_ptr)
		);
		
		if (bool(int, x == 115)) {
			// True!
		}
	}
}
```

Functions can also be marked as raw, and as such can't be called outside of raw scopes.
```go

// The whole function is marked as raw
fn raw my_raw_fn(p : byte_ptr, x : int) {
	if (p == 0) return;
	deref(p + byte_ptr(int, x)) += byte(int, x);
}

fn my_non_raw_fn(p : byte_ptr) {
	var len : int = strlen(p);
	
	// Call our raw function in a raw scope
	// This function remains non-raw
	raw { my_raw_fn(p, len / 2) }
}

entry {
	
	var my_cstring : byte_ptr = "Hello World!";
	
	// Calling the function outside of a raw scope will result in an error
	my_raw_fn(my_cstring, 5);
	
	raw { // Works fine in a raw scope
		my_raw_fn(my_cstring, 6);
	}
	
	// We can call the non-raw function outside of a raw scope
	my_non_raw_fn(my_cstring);
}
```

## Features
Thus far completed features:
- Tokeniser
- Parser

Coming Soon™:
- Parser
	- In the future:
		- Switch statements?
		- Enums? (Typed or untyped)
- Interpreter
	- Simple commands
   	- Execute generated AST

Planned features that will take a while to develop:
- Importing
	- Borrowed VM's?
   	- Might end up just as single file
- FFI
	- Foreign function definition API
	- Foreign memory? (and types)


## Syntax
The syntax of **HoLang** borrows elements from **Odin**, **C**, and **Zig**!
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
#type StringBuf = opaque [STRING_BUF_SIZE + 1]byte; // Extra for null character
#type cstring = ^byte; // Null terminated string

// --- Struct Stuff ---
#type Vertex = struct {
	pos : Vec3,
	col : Color,
};

#type Triangle = [3]Vertex;
#const MAX_TRINGLES = 2048;
#type TrianleBuff = [MAX_TRIS]Triangle;

// Opaque types (array, struct) members
// Can only be accessed in raw scopes
#type Mesh = opaque struct {
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
var b : float = (a + float(int, MY_FAKE_CONSTANT)) * 2; // Evaluated at runtime

// <-- Works until
// --- Functions ---
fn clamp(v : int, min : int, max : int) -> int {
	if (bool(int, v < min)) return min;
	if (bool(int, v > max)) return max;
	return v;
}

fn max(a : int, b : int) -> (c : int) {
	c = a;
	if (bool(int, b > a)) c = b;
	return; // Auto return named returns
}

fn new_mesh(tris : int, col : Color255) -> Mesh {
	
	var mesh : Mesh;
	
	raw { // Mark scope as raw to edit mesh data
		mesh.col = {
			float(u8, col[0]) / 256,
			float(u8, col[1]) / 256,
			float(u8, col[2]) / 256,
			float(u8, col[3]) / 256,
		};
		
		mesh.n_tris = clamp(tris, 0, MAX_TRIANGLES);
	}
	
	return mesh;
}

fn mesh_add_tri(mesh : &Mesh, tri : Triangle) {
	raw {
		if (bool(int, mesh.n_tris >= MAX_TRIANGLES)) return;
		
		// References can be mutated
		mesh.tris[mesh.n_tris] = tri;
		mesh.n_tris += 1;
	}
	
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
	raw {
		self.col = {
			float(u8, col[0]) / 256,
			float(u8, col[1]) / 256,
			float(u8, col[2]) / 256,
			float(u8, col[3]) / 256,
		};
	}
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
	for (bool(int, z < x + y)) {
		z += 1 + max(x, y);
		if (bool(int, mod(z, 2) == 0)) {
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
