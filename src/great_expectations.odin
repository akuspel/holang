package holang

/* --- Great Expectations ---
 * handle parser expectations
 * and group them satisfyingly
 */



// --- Variables ---
expectation_identifier := Expectation {
	positive = {
		TokenIdentifier {},
	}
}

expectation_operator := Expectation {
	positive = {
		TokenOperator {},
	}
}

expectation_equals := Expectation {
	positive = {
		TokenOperator {
			field = { .Equals }
		}
	}
}

expectation_terminator := Expectation {
	positive = {
		TokenDelimiter {
			field = { .Terminator }
		}
	}
}

expectation_col := Expectation {
	positive = {
		TokenOperator {
			field = { .Colon }
		}
	}
}

expectation_struct_curly := Expectation {
	positive = {
		TokenDelimiter {
			field = { .CurlyL }
		}
	}
}

expectation_curly_or_comma := Expectation {
	positive = {
		TokenDelimiter {
			field = { .Comma, .CurlyR }
		}
	}
}

expectation_numeric := Expectation {
	positive = {
		TokenLiteral {
			field = { .Number }
		}
	}
}

expectation_num_or_ident := Expectation {
	positive = {
		TokenLiteral {
			field = { .Number }
		},
		TokenIdentifier {}
	}
}

expectation_square_close := Expectation {
	positive = {
		TokenDelimiter {
			field = { .SquareR }
		}
	}
}

expectation_lit_or_ident := Expectation {
	positive = {
		TokenLiteral {},
		TokenIdentifier {}
	}
}

expectation_lit_or_ident_or_paren := Expectation {
	positive = {
		TokenLiteral {},
		TokenIdentifier {},
		TokenDelimiter {
			field = { .ParenL }
		}
	}
}

expectation_variable_typedef := Expectation {
	
	// Possible scenarios
	// > Keyword unique
	// > Identifier
	// > END
	//
	// > Identifier
	// > END
	//
	// > Keyword struct
	// > Delimiter {	(begin array)
	//	 > struct body
	//	 > Delimiter }	(end struct)
	// > END
	//
	// > Delimiter [		(begin array)
	//	 > Literal Numeric	(array size)
	//	 > Delimiter ]		(end array)
	// > END
	//
	// > Operator ^ (pointer)
	// > Identifier
	// > END
	
	positive = {
		TokenKeyword {
			field = { .Struct, .Unique }
		},
		
		TokenDelimiter {
			field = { .SquareL }
		},
		
		TokenOperator {
			field = { .Pointer }
		},
		
		TokenIdentifier {}
	}
}