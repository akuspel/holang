package holang

/* --- Keywords ---
 * define holangs keywords
 */

// --- Variables ---

TOKEN_KEYWORD := [Keyword]string {
	
	.If   = "if",
	.Else = "else",
	.Elif = "elif",
	.For  = "for",
	
	.Constant	= "#const",
	.Type		= "#type",
	.Function	= "fn",
	.Variable	= "var",
	
	.Struct    = "struct",
	.Unique    = "unique",
	.Immutable = "immutable",
	
	.Entry = "entry",
}

KEYWORD_EXPECTATIONS := [Keyword]Expectation {
	
	.If = {
		// After an if statement, we MUST get parentheses
		
		positive = {
			TokenDelimiter {
				field = { .ParenL }
			}
		},
	},
	
	.Else = {
		// After an else, we MUST get curly brackets
		
		positive = {
			TokenDelimiter {
				field = { .CurlyL }
			}
		},
	},
	
	.Elif = {
		// Same as if
		
		positive = {
			TokenDelimiter {
				field = { .ParenL }
			}
		},
	},
	
	.For = {
		// Same as if
		
		positive = {
			TokenDelimiter {
				field = { .ParenL }
			}
		},
	},
	
	// Other keywords
	.Type = {
		// After a type keyword, we must get an identifier
		// E.G. #type MyType = ...
		
		positive = {
			TokenIdentifier {}
		},
	},
	
	.Function = {
		// After a function keyword, we must get an identifier
		// E.G. fn MyFunction() ...
		
		positive = {
			TokenIdentifier {}
		},
	},
	
	.Variable = {
		// After a variable keyword, we must get an identifier
		// E.G. var my_variable : int = ...
		
		positive = {
			TokenIdentifier {}
		},
	},
	
	.Constant = {
		// After a constant keyword, we must get an identifier
		// E.G. #const MY_CONSTANT = ...
		
		positive = {
			TokenIdentifier {}
		},
	},
	
	.Unique = {
		// After a unique keyword, we must get an identifier
		// E.G. #type UniqueInt = unique int
		
		positive = {
			TokenIdentifier {}
		},
	},
	
	.Struct = {
		// After a struct keyword, we must get Curlies
		// E.G. #type MyStruct = struct { ... }
		
		positive = {
			TokenDelimiter {
				field = { .CurlyL }
			}
		}
	},
	
	.Immutable = {
		// A type identifier
		// Must follow immutable
		
		positive = {
			TokenIdentifier {
				field = { .Unknown, .Type }
			}
		}
	},
	
	.Entry = {
		positive = {
			TokenDelimiter {
				field = { .CurlyL }
			}
		}
	}
}