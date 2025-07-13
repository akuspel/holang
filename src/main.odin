package holang

/* --- Ho-Lang ---
 * holang is a
 * - simple
 * - strongly typed
 * - interpreted
 * - game focused
 * programming language
 * 
 * made for (you guessed it)
 * - homeshift!
 * 
 * it is light and easily embeddable
 * in Odin projects and shi
 * 
 * - hell yeah!
 */

import "core:fmt"

// --- Procedures ---

/* --- main ---
 * entry point of the program
 * when in shell mode
 */
main :: proc() {
	
	
	vm, vm_err := vm_init(1024, 1024)
	if vm_err != nil do fmt.println(vm_err)
	
	vm.text = `
#type MyType = struct {
	a : int,
	b : int,
	c : float,
	d : bool,
};

#type OtherType = struct {
	hmm : MyType,
	a : byte,
	b : byte,
	c : int,
	
	_ : int,
	_ : bool,
};

#const NUM_ELEMS = 10;

#type MyArray = [NUM_ELEMS] OtherType;
#type MyPtr = ^OtherType;
#type MyID = unique int;

#type SlightByte = byte;

#const MY_CONSTANT = 10;
#const OTHER_CONSTANT = 6 + 2 * (3 + 5 / 2.0);

#const FIRST_TRUTH = NUM_ELEMS > OTHER_CONSTANT;

#const THIRD_CONSTANT = (MY_CONSTANT + OTHER_CONSTANT) / 3;

#const SECOND_TRUTH = NUM_ELEMS > THIRD_CONSTANT;

#const THIRD_TRUTH = (2 == 1) || true;

var y : int = 12;
var x : int = MY_CONSTANT;

// PEEK EXPRESSION
// TO FIGURE OUT IF ITS CONSTANT
// OR NOT!

fn main() {
	
	var a : MyType;
	a.a =  0b10;
	a.b = 0b100;
	
	if (a.a == a.b) return;
	if 0b01 return; // Should result in parsing error
}

`
	
	tokenise_err := vm_tokenise_remainder(vm)
	if tokenise_err != nil do fmt.println(tokenise_err)
	
	parse_err := vm_parse_remainder(vm)
	if parse_err != nil do fmt.println(parse_err)
	
	vm_destroy(&vm)
}


