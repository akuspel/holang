package holang

/* --- Parser ---
 * handle the parsing of given tokens
 */

import "core:fmt"
import "core:strings"

// --- Types ---
Expectation :: struct {
	positive,
	negative : []TokenType,
}

ParserState :: struct {
	
	depth : int,
	body  : ParserStateBody,
	
	parent : ^ParserState,
}

ParserStateBody :: union {
	
	
}

// --  States  --
StructState :: struct {
	
	// #type TypeName = struct {
	//	   member_1,
	//	   member_2,
	//	   ... ,
	//	   member_n
	// }
	
	state : enum {
		Define,
		
	}
}


// --- Procedures ---

parse_tokens :: proc(
	text : string,
	tokens : []Token,
) -> (num : int, err : Error) {
	if text == "" do return 0, .Empty_String
	if tokens == nil do return 0, .No_Destination
	
	num_tokens := len(tokens)
	last : Token
	
	state : ParserState
	
	for i in 0..<num_tokens {
		next := tokens[i]
		
		sub, s_ok := strings.substring(text, next.start, next.end)
		if !s_ok do return 0, .Invalid_String
		
		sub_last := strings.substring(text, last.start, last.end) or_else ""
		
		switch &s in state {
			
			
		case:
			// File scope
			// Possibilities are endless
			
			
		}
		
		last = next
	}
	
	return num_tokens, nil
}

parse_expectations :: proc(
	next : Token,
	expects : Expectation
) -> bool {
	
	// Handle positive cases
	positive : bool
	if expects.positive == nil {
		positive = true
	} else {
		
		for &t in expects.positive {
			positive ||= expect(next, t, false)
		}
	}
	
	// Handle negative cases
	if expects.negative == nil {
		return positive
	} else {
		
		for &t in expects.negative {
			negative := expect(next, t, true)
			if !negative do return false
		}
	}
	
	return positive
}

print_expectations :: proc(
	expects : Expectation
) {
	
	fmt.print("Expected ")
	if expects.positive != nil {
		for &f in expects.positive do print(f)
	}
	
	if expects.negative != nil && len(expects.negative) > 0 {
		fmt.print("but not ")
		for &f in expects.negative do print(f)
	}
	
	print :: proc(f : TokenType) {
		switch &b in f {
		case TokenKeyword:		print_field(b)
		case TokenOperator: 	print_field(b)
		case TokenIdentifier:	print_field(b)
		case TokenLiteral:		print_field(b)
		case TokenDelimiter:	print_field(b)
		}
	}
}

print_field :: proc(
	body : TokenBody($T)
) {
	for v in body.field do fmt.println(v)
	if body.field == {} do for t in T do fmt.println(t)
}

/* --- expect ---
 * expect the next token to
 * follow specified pattern
 *
 * on not expect anything BUT
 * NOTE: when using not,
 *		 you must join expressions with &&,
 *		 not with || like in normal case
 */
expect :: proc(next : Token, type : Maybe(TokenType), not : bool) -> bool {
	if type == nil do return true // No expectations
	
	a, b : typeid
	any_of : bool
	switch &b in type.? {
	case TokenKeyword:		
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenIdentifier:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenOperator:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenLiteral:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenDelimiter:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	}
	
	switch &t in next.body {
	case TokenKeyword:		
		b = typeid_of(type_of(t))
	case TokenIdentifier:
		b = typeid_of(type_of(t))
	case TokenOperator:
		b = typeid_of(type_of(t))
	case TokenLiteral:
		b = typeid_of(type_of(t))
	case TokenDelimiter:
		b = typeid_of(type_of(t))
	}
	
	// Continue check if types match
	match := (a == b)
	if !match do return not
	if any_of do return !not
	
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