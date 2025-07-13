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
	TokeniserError,
	ParserError,
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
	Invalid_Name,	// Invalid type name
	Invalid_Token,	// Can't get the specific member
	
	Text_Mismatch,	// Mismatch with text length and tokenisation count
	Text_Invalid,	// Unable to get substring from text
	Token_Mismatch, // Mismatch with token amount and parse count
}

TokeniserError :: enum {
	
	None,
	
	Empty_String,		// Given string is empty
	Invalid_String, 	// Given string isn't valid utf8
	No_Destination, 	// No tokenisation destination specified
}

ParserError :: enum {
	
	None,
	
	Token_Unexpected,	// Received unexpected token
	Token_Unknown,		// Token type should be known, but isn't
	
	Invalid_Array_Size, // Trying to define an array of invalid size
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