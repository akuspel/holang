package holang

/* --- Virtual Machine ---
 * holang VM, definitions, behaviour
 * and all that fun sorta stuff
 */

import "core:mem"
import "core:strings"
import "core:unicode/utf8"

// --- Types ---
@(private)
VirtualMachine :: struct {
	
	// AST Base
	ast_root : AST_Frame,
	
	// Runtime
	variables : [dynamic]Variable,
	functions : [dynamic]Function,
	constants : [dynamic]Constant,
	
	types : [dynamic]Type,
	
	scope : int,
	
	// Parsing
	text	: string,
	tokens	: [dynamic]Token,
	errors  : [dynamic]ParseErrorMessage,
	tokenised_to : int, // How far has the text been tokenised
	parsed_to	 : int, // How far have the tokens been parsed
	
	// Memory
	data  : rawptr,
	size  : int,
	heap  : int,
	stack : ^Stack,
	
	cmd_arena  : mem.Dynamic_Arena,
	type_arena : mem.Dynamic_Arena,
}

VM :: ^VirtualMachine

// --- Procedures ---
vm_init :: proc(stack, heap : int) -> (vm : VM, err : Error) {
	if min(stack, heap) <= 0 do return nil, .Invalid_Size
	alloc := context.allocator
	
	new_vm, new_err := new(VirtualMachine, alloc)
	if new_err != nil do return nil, new_err
	defer if err != nil do free(new_vm)
	
	size := stack + heap
	ptr, alloc_err := mem.alloc(size, allocator = alloc)
	if alloc_err != nil do return nil, alloc_err
	defer if err != nil do free(ptr)
	
	stack, stack_err := stack_make(ptr, stack, alloc)
	if stack_err != nil do return nil, stack_err
	
	vm = new_vm
	vm.data  = ptr
	vm.stack = stack
	vm.size  = size
	vm.heap  = heap
	
	mem.dynamic_arena_init(&vm.cmd_arena)
	mem.dynamic_arena_init(&vm.type_arena)
	
	// Standard setup
	types_err := base_types_init(vm)
	if types_err != nil do return nil, types_err
	
	return
}

vm_destroy :: proc(vm : ^VM) -> (err : Error) {
	if vm  == nil do return .No_VM
	if vm^ == nil do return .No_VM
	
	err = stack_destroy(&vm^.stack)
	
	delete(vm^.variables)
	delete(vm^.functions)
	delete(vm^.constants)
	delete(vm^.types)
	
	delete(vm^.tokens)
	delete(vm^.errors)
	
	mem.dynamic_arena_destroy(&vm^.cmd_arena)
	mem.dynamic_arena_destroy(&vm^.type_arena)
	
	// TODO: handle AST memory	
	
	free(vm^)
	vm^ = nil
	
	return
}

/* --- vm_reset ---
 * reset the VM into
 * a clean state
 */
vm_reset :: proc(vm : VM) -> (err : Error) {
	if vm == nil do return .No_VM
	
	clear(&vm.types)
	mem.dynamic_arena_free_all(&vm.cmd_arena)
	mem.dynamic_arena_free_all(&vm.type_arena)
	err = stack_reset(vm.stack)
	
	// TODO: handle AST memory
	
	clear(&vm.variables)
	clear(&vm.functions)
	clear(&vm.constants)
	
	return
}

@(private)
/* --- vm_get_type_allocator ---
 * get the dynamic arena allocator
 * for storing type data
 */
vm_get_type_allocator :: proc(vm : VM) -> (alloc : mem.Allocator, err : Error) {
	if vm == nil do return {}, .No_VM
	return mem.dynamic_arena_allocator(&vm.type_arena), nil
}

@(private)
/* --- vm_get_ast_allocator ---
 * get the dynamic arena allocator
 * for storing  AST nodes / frames
 */
vm_get_ast_allocator :: proc(vm : VM) -> (alloc : mem.Allocator, err : Error) {
	if vm == nil do return {}, .No_VM
	return mem.dynamic_arena_allocator(&vm.cmd_arena), nil
}

/* --- vm_tokenise_remainder ---
 * tokenise the remaining text
 * stored in the virtual machine
 */
vm_tokenise_remainder :: proc(vm : VM) -> (err : Error) {
	if vm == nil do return .No_VM
	
	n_runes := utf8.rune_count_in_string(vm.text)
	if n_runes == vm.tokenised_to do return nil // No need to tokenise
	if n_runes	< vm.tokenised_to do return .Text_Mismatch
	sub := strings.substring(vm.text, vm.tokenised_to, n_runes) or_else ""
	if sub == "" do return .Text_Invalid // Empty string must be a result of previous or_else
	
	n, t_err := tokenise(sub, &vm.tokens)
	if t_err != nil do return t_err
	
	vm.tokenised_to += n
	if vm.tokenised_to != n_runes do return .Text_Mismatch
	
	return
}

vm_parse_remainder :: proc(vm : VM) -> (err : Error) {
	if vm == nil do return .No_VM
	
	n_tokens := len(vm.tokens)
	if n_tokens == vm.parsed_to do return nil // No need to parse
	if n_tokens  < vm.parsed_to do return .Token_Mismatch
	if vm.tokenised_to < 0 do return .Token_Mismatch
	
	// n, p_err := parse_tokens(
	// 	vm, vm.text, vm.tokens[vm.parsed_to:n_tokens],
	// 	TokenID(vm.parsed_to)
	// );	if p_err != nil do return p_err
	
	// vm.parsed_to += n
	
	p_err := parse_vm(
		vm, TokenID(vm.parsed_to)
	)
	
	if vm.parsed_to != n_tokens - 1 do return .Token_Mismatch
	
	return
}


// --- Getters ---
@(private, require_results)
get_token :: proc(vm : VM, id : TokenID) -> (token : Token, err : Error) {
	if vm == nil do return {}, .No_VM
	if id < 0 || int(id) >= len(vm.tokens) do return {}, .Invalid_Token
	return vm.tokens[id], nil
}

@(private, require_results)
get_constant :: proc(vm : VM, id : ConstID) -> (const : Constant, err : Error) {
	if vm == nil do return {}, .No_VM
	if id < 0 || int(id) >= len(vm.constants) do return {}, .Unknown_Const
	return vm.constants[id], nil
}

@(private, require_results)
get_const_id_by_name :: proc(vm : VM, name : string) -> (id : ConstID, err : Error) {
	if vm == nil do return	-1, .No_VM
	if name == "" do return -1, .Invalid_Name
	for &c, i in vm.constants {
		(c.name == name) or_continue
		
		// Found type
		return ConstID(i), nil
	}
	
	// No matching type
	return -1, .Unknown_Const
}

@(private, require_results)
get_variable :: proc(vm : VM, id : VarID) -> (var : Variable, err : Error) {
	if vm == nil do return {}, .No_VM
	num_vars := len(vm.variables)
	if int(id) >= num_vars || int(id) < -num_vars do return {}, .Unknown_Var
	if id < 0 do return vm.variables[num_vars + int(id)], nil
	return vm.variables[id], nil
}

@(private, require_results)
get_var_id_by_name :: proc(vm : VM, name : string, reverse := false) -> (id : VarID, err : Error) {
	if vm == nil do return	0, .No_VM
	if name == "" do return 0, .Invalid_Name
	for &c, i in vm.variables {
		(c.name == name) or_continue
		
		// Found type
		id = VarID(i)
		if reverse do id = -VarID(len(vm.variables) - 1) + id
		return id, nil
	}
	
	return 0, .Unknown_Var
}

@(private, require_results)
get_function :: proc(vm : VM, id : FunctionID) -> (func : Function, err : Error) {
	if vm == nil do return {}, .No_VM
	if id < 0 || int(id) >= len(vm.functions) do return {}, .Unknown_Func
	return vm.functions[id], nil
}

@(private, require_results)
get_func_id_by_name :: proc(vm : VM, name : string) -> (id : FunctionID, err : Error) {
	if vm == nil do return	-1, .No_VM
	if name == "" do return -1, .Invalid_Name
	for &f, i in vm.functions {
		(f.name == name) or_continue
		
		// Found type
		return FunctionID(i), nil
	}
	
	// No matching type
	return -1, .Unknown_Const
}

// --- Registering ---

register_constant :: proc(vm : VM, const : Constant) -> (err : Error) {
	if vm == nil do return .No_VM
	
	// Check for constant redifinitions
	if get_identifier_type(vm, const.name) != .Unknown do return .Constant_Over
	
	// Append
	append(&vm.constants, const)
	
	return
}

// --- Utils ---
get_identifier_type :: proc(vm : VM, name : string) -> (type : Identifier) {
	// Named value precedence
	for &c in vm.constants do if c.name == name do return .Constant
	for &t in vm.types	   do if t.name == name do return .Type
	for &f in vm.functions do if f.name == name do return .Function
	for &v in vm.variables do if v.name == name do return .Variable
	return // Unknown identifier type
}