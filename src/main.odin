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
};

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
}


