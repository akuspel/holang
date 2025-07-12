package holang

/* --- Error ---
 * handle internal, vm and
 * interpretation errors
 */

import "core:mem"

// --- Types ---

/* --- Error ---
 * fatal error, which causes
 * the VM (if applicable) to
 * shut down and clean up
 */
Error :: union #shared_nil {
	
	mem.Allocator_Error,
	
	StackError,
	MemoryError,
	VMError,
}

StackError :: enum {
	
	None,
	
	No_Stack,			// No stack given
	Empty_Stack,		// Stack hasn't been initialized
	Wrong_Stack_Size,	// Wrong init size
	
	Stack_Overflow, 	// Stack memory overflow
	Stack_Underflow,	// Popping too hard
	
}

MemoryError :: enum {
	
	None,
	
	Null_Pointer,	// Null pointer given
	Copy_Issue, 	// Unable to copy memory
	Invalid_Size,	// Invalid memory size, usually negative or zero
}

VMError :: enum {
	
	None,
	No_VM,			// No VM given
	Unknown_Type,	// Type doesn't exist
	Type_Overwrite, // Type already exists
	Unknown_Member, // Can't get the member from Array or Struct
	Bounds_Check,	// Bounds overflow in Array or Struct
}


// --  Safe Errors	--

/* --- NFError ---
 * non-fatal error, which allows
 * the VM to continue running
 */
NFError :: union #shared_nil {
	
	InterpreterError,
}

InterpreterError :: enum {
	None,
}