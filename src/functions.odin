package holang

/* --- Functions ---
 * function definitions and behaviour
 * for the holang virtual machine
 */


// --- Types ---
FunctionID :: distinct int
Function :: struct {
	
	name : string,
	raw  : bool,
	
	arguments   : []FunctionArgument,
	return_val  : Maybe(FunctionArgument),
	does_return : bool,
	// NOTE: once I have figured out
	//		 single return values do
	//		 consider multi returns
	
	ast_root : AST_Frame,
}

FunctionArgument :: struct {
	name : string,
	type : TypeID,
	as_ref : bool,
}
