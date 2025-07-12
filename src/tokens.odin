package holang

/* --- Tokens ---
 * holang tokens for tokenisation
 * and parsing
 */

import "base:intrinsics"

// --- Types ---
Token :: struct {
	
	start, end : int,
	body : TokenType,
	
	value : Variant,
	ident : int,
}

// --  Tokenisation  --
TokenType :: union {
	
	TokenDelimiter,
	TokenKeyword,
	TokenLiteral,
	TokenOperator,
	TokenIdentifier,
}

TokenBody :: struct($T : typeid) where intrinsics.type_is_enum(T) {
	type : T,
	field : bit_set[T],
}

TokenKeyword :: TokenBody(Keyword)
Keyword :: enum {
	
	If,
	Else,
	Elif,
	For,
	
	Type,
	Function,
	Variable,
	Constant,
	Unique,
}

TokenDelimiter :: TokenBody(Delimiter)
Delimiter :: enum {
	
	CurlyL, CurlyR,
	ParenL, ParenR,
	SquareL, SquareR,
	
	Terminator,
	Comma,
	Period,
	
}

TokenLiteral :: TokenBody(Literal)
Literal :: enum {
	
	String,
	Number,
	Boolean,
}

TokenOperator :: TokenBody(Operator)
Operator :: enum {
	
	Add, Sub, Mul, Div,
	
	Pointer,	// *
	Reference,	// &
}

TokenIdentifier :: TokenBody(Identifier)
Identifier :: enum {
	Type,
	Variable,
	Constant,
	Function,
}

// --  Runes  --
RuneType :: enum {
	
	Space,			// E.G. spaces and tabs
	Separator,		// E.G. ',' ';' '\n'
	Operator,		// E.G. '+' '-'
	
}


// --- Procedures ---

/* --- expect ---
 * expect the next token to
 * follow specified pattern
 *
 * on not expect anything BUT
 * NOTE: when using not,
 *		 you must join expressions with &&,
 *		 not with || like in normal case
 */
expect :: proc(tokens : []Token, type : Maybe(TokenType), not : bool) -> bool {
	if type == nil do return true // No expectations
	if len(tokens) == 0 do return false // Great expectations
	
	next := tokens[0]
	
	a, b : typeid
	#partial switch t in type.? {
	case: a = typeid_of(type_of(t))
	}
	
	#partial switch t in next.body {
	case: b = typeid_of(type_of(t))
	}
	
	// Continue check if types match
	match := (a == b)
	if !match do return not
	
	t := type.?
	switch &b in next.body {
	case TokenKeyword:		 match = b.type in t.(TokenKeyword).field
	case TokenIdentifier:	 match = b.type in t.(TokenIdentifier).field
	case TokenOperator: 	 match = b.type in t.(TokenOperator).field
	case TokenLiteral:		 match = b.type in t.(TokenLiteral).field
	case TokenDelimiter:	 match = b.type in t.(TokenDelimiter).field
	}
	
	return match != not
}

/* backup
TokenType :: enum {
	
	Unknown,
	
	Keyword,
	Identifier,
	Operator,
	Literal,
	Delimiter,
	
}
*/