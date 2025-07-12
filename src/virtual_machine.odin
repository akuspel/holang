package holang

/* --- Virtual Machine ---
 * holang VM, definitions, behaviour
 * and all that fun sorta stuff
 */

import "core:mem"

// --- Types ---
@(private)
VirtualMachine :: struct {
	
	stack : ^Stack,
	
	variables : [dynamic]Variable,
	functions : [dynamic]Function,
	constants : [dynamic]Constant,
	
	types : [dynamic]Type,
	
	scope : int,
	
	type_arena : mem.Dynamic_Arena,
}

VM :: ^VirtualMachine

// --- Procedures ---
vm_init :: proc(stack_size : int) -> (vm : VM, err : Error) {
	
	new_vm, new_err := new(VirtualMachine)
	if new_err != nil do return nil, new_err
	defer if err != nil do free(new_vm)
	
	alloc := context.allocator
	stack, stack_err := stack_make(stack_size, alloc)
	if stack_err != nil do return nil, stack_err
	
	vm = new_vm
	vm.stack = stack
	
	mem.dynamic_arena_init(&vm.type_arena)
	
	// Standard setup
	base_types_init(vm)
	
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
	
	mem.dynamic_arena_destroy(&vm^.type_arena)
	
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
	mem.dynamic_arena_free_all(&vm.type_arena)
	err = stack_reset(vm.stack)
	
	clear(&vm.variables)
	clear(&vm.functions)
	clear(&vm.constants)
	
	return
}