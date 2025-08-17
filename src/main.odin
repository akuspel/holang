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

import "core:os"
import "core:fmt"

// --- Procedures ---

/* --- main ---
 * entry point of the program
 * when in shell mode
 */
main :: proc() {
	
	// Example expression
	// 1.0 + 1.5 * (1.5 + 29.1 * 17.0)
	expr : PrattExpression
	append(&expr, Variant(f64(1.0)))
	append(&expr, Operator.Add)
	append(&expr, Variant(f64(1.5)))
	append(&expr, Operator.Mul)
	append(&expr, Delimiter.ParenL)
	append(&expr, Variant(f64(1.5)))
	append(&expr, Operator.Add)
	append(&expr, Variant(f64(29.1)))
	append(&expr, Operator.Mul)
	append(&expr, Variant(f64(17.0)))
	append(&expr, Delimiter.ParenR)
	
	fmt.println(pratt_parse(&expr, context.temp_allocator))
	
	// when true do return
	vm, vm_err := vm_init(1024, 1024)
	if vm_err != nil do fmt.println(vm_err)
	
	vm.text = #load("../res/example.ho", string)
	
	tokenise_err := vm_tokenise_remainder(vm)
	if tokenise_err != nil do fmt.println(tokenise_err)
	
	parse_err := vm_parse_remainder(vm)
	if parse_err != nil do fmt.println(parse_err)
	
	vm_destroy(&vm)
}


