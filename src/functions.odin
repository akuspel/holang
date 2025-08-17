package holang

/* --- Functions ---
 * function definitions and behaviour
 * for the holang virtual machine
 */


// --- Types ---
FunctionID :: distinct int
Function :: struct {
	
	name : string,
	
	arguments : []FunctionArgument,
	returns   : Maybe(FunctionArgument),
	// NOTE: once I have figured out
	//		 single return values do
	//		 consider multi returns
}

FunctionArgument :: struct {
	name : string,
	type : TypeID,
}
