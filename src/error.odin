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
	Pointer_OOB,	// Pointing to Out Of Bounds memory
	
	Copy_Issue, 	// Unable to copy memory
	Invalid_Size,	// Invalid memory size, usually negative or zero
}

VMError :: enum {
	
	None,
	
	No_VM,			// No VM given
	Unknown_Type,	// Type doesn't exist
	Unknown_Const,	// Can't get the specific constant
	Unknown_Var,	// Can't get the spicific variable
	Unknown_Func,	// Can't get the specific function
	
	Invalid_Name,	// Invalid named value name
	Unknown_Member, // Can't get the member from Array or Struct
	Bounds_Check,	// Bounds overflow in Array or Struct
	Invalid_Token,	// Can't get the specific member
	
	Text_Mismatch,	// Mismatch with text length and tokenisation count
	Text_Invalid,	// Unable to get substring from text
	Token_Mismatch, // Mismatch with token amount and parse count
	
	Unimplemented,	// Given feature is yet to be implemented
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
	Struct_Member_Over, // Trying to override struct member (duplicate name)
	Struct_Member_None, // Trying to not give a name to a struct member
	
	Expression_Depth,	// Expression isn't able to exit with depth == 0
	Expression_Invalid, // Expression results in an invalid (nil) value
	
	Invalid_Operator,	// Invalid operator used in expression
	Invalid_Value,		// Expected a value, none received
	Invalid_Type,		// Generated type turned out invalid
	Invalid_Variable,	// Generated variable is invalid

	Constant_Over,		// Trying to override existing constant
	Variable_Over,		// Trying to override existing
	Type_Over,			// Trying to override existing
	Type_Mismatch,		// Types don't match
	
	Scope_Incomplete,	// Scope has not been finished
	EOF,				// File has reached its end
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
	
	Variable_Invalid,	// Variable doesn't exist
	Type_Cast_Invalid,	// Unable to cast types
	Mutating_Immutable, // Trying to modify immutable variable
	
	Type_Check_Failure,	// Types don't match
	Expression_Invalid,	// Expression result is invalid
	Command_Failure,	// Generic command failure
	
	Argument_Mismatch,	// Arguments don't match function call
	
}



