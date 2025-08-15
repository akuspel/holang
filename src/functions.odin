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
	returns   : []FunctionArgument,
}

FunctionArgument :: struct {
	name : string,
	type : TypeID,
}
