package holang

/* --- Tokens ---
 * holang tokens for tokenisation
 * and parsing
 */

import "base:intrinsics"

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:unicode/utf8"

// --- Types ---
Token :: struct {
	
	start, end : int,
	body : TokenType,
	
	value : Variant,
	ident : int,
}

TokenID :: distinct int

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
	
	Struct,
}

TokenDelimiter :: TokenBody(Delimiter)
Delimiter :: enum {
	
	Unknown,
	CurlyL, CurlyR,
	ParenL, ParenR,
	SquareL, SquareR,
	
	Terminator,
	Newline,
	Comma,
	Period,
	
	EOF,
}

TokenLiteral :: TokenBody(Literal)
Literal :: enum {
	
	Unknown,
	String,
	Number,
	Boolean,
}

TokenOperator :: TokenBody(Operator)
Operator :: enum {
	
	Unknown,
	
	Equals, 	// =
	
	// Simple maths
	Add, Sub, Mul, Div,
	AddEq, SubEq,
	MulEq, DivEq,
	
	// Comparison
	EQ, NE,
	LS, LE,
	GR, GE,
	
	// Boolean
	And, Or,
	
	// Other stuff
	Pointer,	// ^
	Reference,	// &
	Return, 	// ->
	Colon,		// :
}

TokenIdentifier :: TokenBody(Identifier)
Identifier :: enum {
	
	Unknown,
	Type,
	Variable,
	Constant,
	Function,
}

// --  Runes  --
RuneType :: enum {
	
	Null,			// 0x0
	Space,			// E.G. spaces and tabs
	Separator,		// E.G. ',' ';' '\n'
	Operator,		// E.G. '+' '-'
	
	Numerical,		// E.G. '1' '2' '3'
	Textual,		// E.G. 'A' '_'
	
}

// --- Variables
TOKEN_OPERATOR := [Operator]string {
	.Unknown = "",
	
	.Equals = "=",
	
	.Add = "+",
	.Sub = "-",
	.Mul = "*",
	.Div = "/",
	
	.AddEq = "+=",
	.SubEq = "-=",
	.MulEq = "*=",
	.DivEq = "/=",
	
	.EQ = "==",
	.NE = "!=",
	.LS =  "<",
	.LE = "<=",
	.GR =  ">",
	.GE = ">=",
	
	.And = "&&",
	.Or  = "||",
	
	.Pointer   = "^",
	.Reference = "&",
	.Return    = "->",
	.Colon	   = ":",
}

TOKEN_DELIMITER := [Delimiter]string {
	.Unknown = "",
	
	.CurlyL = "{",
	.CurlyR = "}",
	.ParenL = "(",
	.ParenR = ")",
	.SquareL = "[",
	.SquareR = "]",
	
	.Terminator = ";",
	.Newline = "\n",
	.Comma = ",",
	.Period = ".",
	
	.EOF = "",
}

@(private="file")
RUNE_OPERATOR := [?]rune {
	'+', '-', '*', '/', '^', '<', '>', '=', '!', '?', ':',
}

@(private="file")
RUNE_TEXTUAL := [?]rune {
	'_', '#', '@',
}

// --- Procedures ---

/* --- get_rune_type ---
 * return the type of given
 * rune for parsing purposes
 */
get_rune_type :: proc(r : rune) -> RuneType {
	if r == 0 do return .Null
	if strings.is_space(r) do return .Space
	
	if rune_within(r, '0', '9') do return .Numerical
	for o in RUNE_OPERATOR do if o == r do return .Operator
	if rune_within(r, 'a', 'z') || rune_within(r, 'A', 'Z') do return .Textual
	for t in RUNE_TEXTUAL do if t == r do return .Textual
	
	return .Separator
	
	// --- Internal Procedures ---
	rune_within :: #force_inline proc(r, a, b : rune) -> bool {
		return r >= a && r <= b
	}
}

tokenise :: proc(text : string, arr : ^[dynamic]Token) -> (num : int, err : Error) {
	if arr == nil do return 0, .No_Destination
	if !utf8.valid_string(text) do return 0, .Invalid_String
	if text == "" do return 0, .Empty_String
	
	graphemes, ng, nr, wr := utf8.decode_grapheme_clusters(text, true, context.temp_allocator)
	
	last_type := get_rune_type(utf8.rune_at_pos(text, 0))
	last_token : Token
	for g, i in graphemes {
		r := utf8.rune_at_pos(text, g.rune_index)
		type := get_rune_type(r)
		
		sub := strings.substring(text, last_token.start, last_token.end) or_else ""
		apnd: if next_token(r, sub, type, last_type, &last_token, false) {
			defer {
				last_type = type
				
				last_token = {
					start = g.rune_index,
					end   = g.rune_index,
				}
			}
			
			(last_type != .Null && last_type != .Space) or_break apnd
			(last_token.body != nil) or_break apnd
			
			append_token(arr, last_token, sub)
		}
		
		last_token.end += g.width
		
		// Take care of last token
		if i == ng - 1 {
			last_token = {
				start = g.rune_index,
				end   = g.rune_index + g.width,
			}
			
			sub := strings.substring(text, last_token.start, last_token.end) or_else ""
			next_token(r, sub, type, last_type, &last_token, true)
			append_token(arr, last_token, sub)
		}
	}
	
	return nr, nil
	
	// --- Internal Procedures ---
	append_token :: proc(
		arr : ^[dynamic]Token,
		token : Token,
		sub : string
	) {
		append(arr, token)
		// fmt.println(sub, "\n ->", token.body)
		// if token.value != nil do fmt.println(" ->", token.value)
	}
	
	next_token :: proc(
		r : rune,
		sub : string,
		curr, last : RuneType,
		token : ^Token,
		is_last : bool
	) -> bool {
		(curr != last || curr == .Separator || is_last) or_return
		
		// Identifiers can start with alphabetic rune and include numbers
		// NOTE: numbers cannot begin identifiers
		if last == .Textual && curr == .Numerical do return false
		
		// Numbers can have a second alphabetical member
		// E.G. 0x0, 0b0123
		w := token.end - token.start
		x_or_b := r == 'x' || r == 'b'
		if last == .Numerical && curr == .Textual && w == 1 && x_or_b do return false
		
		switch last {
		case .Null, .Space:
			// Ignore
		
		case .Textual:
			
			// Can be either Keyword or Identifier
			// Or a string / bool literal (TODO)
			
			// Check for boolean
			if sub == "true" || sub == "false" {
				token.body = TokenLiteral {
					type = .Boolean,
				}
				
				// Store boolean value already
				token.value = true if sub == "true" else false
				return true
			}
			
			// Check for keywords
			for k in Keyword {
				(TOKEN_KEYWORD[k] == sub) or_continue
				
				// Keyword matches
				token.body = TokenKeyword {
					type = k,
				}
				
				return true
			}
			
			// Must be an identifier
			token.body = TokenIdentifier {}
		
		case .Separator:
			
			body := TokenDelimiter {}
			for d in Delimiter {
				(TOKEN_DELIMITER[d] == sub) or_continue
				
				// Found delimiter
				body.type = d
				break
			}
			
			token.body = body
		
		case .Operator:
			
			body := TokenOperator {}
			for o in Operator {
				(TOKEN_OPERATOR[o] == sub) or_continue
				
				// Found operator
				body.type = o
				break
			}
			
			token.body = body
		
		case .Numerical:
			token.body = TokenLiteral {
				type = .Number,
			}
			
			// Attempt to parse the number
			is_float := strings.contains(sub, ".")
			is_bin := strings.contains(sub, "b")
			is_hex := strings.contains(sub, "x")
			
			if is_float {
				float, ok := strconv.parse_f64(sub)
				if ok do token.value = float
				
			} else if is_bin {
				
				
			} else if is_hex {
				
				
			} else {
				integer, ok := strconv.parse_int(sub)
				if ok do token.value = integer
			}
		}
		
		// Should select next
		return true
	}
}
