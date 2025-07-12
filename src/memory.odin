package holang

/* --- Memory ---
 * handle the internal memory
 * of the holang virtual machine
 */

// --- Variables ---

// --- Procedures ---
get_global_ptr :: proc(vm : VM, ptr : uintptr) -> uintptr {
	
	// NOTE: in the future we might want to
	//		 have a global offset for our VM
	//		 memory starting point, and
	//		 calculate pointers in relation
	//		 to that :)
	
	return ptr
}