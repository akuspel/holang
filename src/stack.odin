#+private
package holang

/* --- Stack ---
 * virtual stack for the holang
 * virtual machine
 */

import "core:mem"

// --- Types ---
Stack :: struct {
	
	data : rawptr,
	off  : uintptr,
	size : int,
	
}

// --- Constants ---
DEFAULT_STACK_SIZE :: mem.Megabyte
MAX_STACK_SIZE :: mem.Gigabyte

// --- Procedures ---
stack_make :: proc(size : int = DEFAULT_STACK_SIZE, alloc := context.allocator) -> (stack : ^Stack, err : Error) {
	if size < 0 || size > MAX_STACK_SIZE do return nil, .Wrong_Stack_Size
	
	ns, new_err := new(Stack, alloc)
	if new_err != nil do return nil, new_err
	defer if err != nil do free(ns)
	
	ptr, alloc_err := mem.alloc(size, allocator = alloc)
	if alloc_err != nil do return nil, alloc_err
	defer if err != nil do free(ns)
	
	stack = ns
	stack.data = ptr
	stack.size = size
	
	return stack, nil
}

stack_destroy :: proc(stack : ^^Stack) -> (err : Error) {
	if stack  == nil do return .No_Stack
	if stack^ == nil do return .No_Stack
	
	if stack^.data != nil {
		free(stack^.data)
	} else {
		err = .Empty_Stack
	}
	
	free(stack^)
	stack^ = nil
	
	return
}

stack_reset :: proc(s : ^Stack)-> (err : Error) {
	if s == nil do return .No_Stack
	
	// Reset all mutating stack parameters
	s.off = 0
	
	return
}

stack_push :: proc(
	s : ^Stack,
	from : rawptr,
	size, align : int
) -> (val_ptr : uintptr, err : Error) {
	if s == nil do return 0, .No_Stack
	if from == nil do return 0, .Null_Pointer
	
	if min(size, align) <= 0 do return 0, .Invalid_Size
	val_ptr = mem.align_forward_uintptr(
		s.off, uintptr(align)
	);	new_off := val_ptr + uintptr(size)
	
	if int(new_off) > s.size do return 0, .Stack_Overflow
	ptr := mem.copy(
		rawptr(uintptr(s.data) + s.off),
		from, size
	);	if ptr == nil do return 0, .Copy_Issue
	
	s.off = new_off
	
	return
}

stack_pop :: proc(
	s : ^Stack,
	size : int
) -> (err : Error) {
	if s == nil do return .No_Stack
	if size <= 0 do return .Invalid_Size
	
	if size > int(s.off) do return .Stack_Underflow
	s.off -= uintptr(size)
	
	return
}