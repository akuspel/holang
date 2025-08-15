package holang

/* --- Commands ---
 * handle  the types, creation and
 * execution of *runtime commands*
 *
 * values, expressions, properties
 * are stored in slices, which are
 * allocated with the VM's command
 * arena
 */


// --- Types ---
Command :: struct {
	
	
	body : CommandBody,
	
	meta : struct {
		num_line : int,
		num_rune : int,
	}
}

CommandBody :: union {

	VariableCreateCommand,
	VariableAssignCommand,	
}


// --  Bodies  --
CommandValue :: union { Variant, Expression } // Const VS Runtime

VariableCreateCommand :: struct {
	
	name : string,
	
	immutable : bool, 	// Cannot be modified by commands
	reference : bool,	// Don't push new memory on stack
						// -> Variable.before = SP
	
	base_type : TypeID, // Variable type
	base_var  : string, // Only used if (reference == true)
	
	values : []CommandValue,	// Values must be an array to
								// Account for arrs / structs
	
	
}


VariableAssignCommand :: struct {
	
	base_var : string,
	
	values : []CommandValue,
}

FunctionCallCommand :: struct {
	
	function : FunctionID,
	arguments : []CommandValue,
	
}

ReturnCommand :: struct {
	
}

// --- Scopes ---
// each BeginScope MUST have a
// conditionless EndScope at the
// end
BeginScopeCommand :: struct {
	condition : CommandValue,	// Boolean condition
	
}

EndScopeCommand :: struct {
	condition : CommandValue,	// Boolean condition
	depth : Maybe(int),			// To which depth (default one scope less)
								// NOTE: this is calculated per function
	
}


// --- Procedures ---
execute_command :: proc(vm : VM, cmd : Command) -> (err : NFError) {
	
	
	return
}



