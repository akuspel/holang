package holang

/* --- Memory ---
 * handle the internal memory
 * of the holang virtual machine
 */

import "core:mem"

// --- Variables ---

// --- Procedures ---
reset_memory :: proc(vm : VM) -> Error {
	
	for i in 0..<vm.size {
		// Set all bytes to zero
		(^byte)(uintptr(vm.data) + uintptr(i))^ = 0
	}
	
	return nil
}

get_global_ptr :: proc(vm : VM, ptr : uintptr) -> uintptr {
	
	// NOTE: in the future we might want to
	//		 have a global offset for our VM
	//		 memory starting point, and
	//		 calculate pointers in relation
	//		 to that :)
	
	return uintptr(vm.data) + ptr
}

@(require_results)
heap_allocate :: proc(vm : VM, size, align : int, type : TypeID) -> (ptr : uintptr, err : Error) {
	if vm == nil do return 0, .No_VM
	if vm.data == nil do return 0, .Uninitialized
	
	start : uintptr; found := true
	main_loop: for a in vm.allocations {
		found = false
		
		// Add to start until you find free block
		start = mem.align_forward_uintptr(
			a.ptr + uintptr(a.size), uintptr(align))

		// Check stack start		
		end := start + uintptr(size)
		(end < uintptr(vm.heap)) or_continue main_loop
		
		// Check each allocation
		for s in vm.allocations {
			s_start := s.ptr
			s_end   := s.ptr + uintptr(s.size)
			
			(end < s_start || start >= s_end) or_continue main_loop
		}
		
		// Found free spot
		found = true
		break
	}
	
	// Can't fit allocation in frame
	if !found do return 0, .Alloc_Overflow
	
	new_alloc := VM_Allocation {
		ptr  = start,
		size = size,
		
		type = type
	}
	
	append(&vm.allocations, new_alloc)
	return start, nil
}