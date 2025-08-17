package holang

/* --- Pratt Parser ---
 * parse expressions using
 * the pratt algorithm
 */

import "core:mem"
import "core:fmt"

// --- Types ---
PrattNode :: struct {
	
	type : enum {
		Infix,
		Prefix,
		Postfix,
	},
	
	op : Operator,
	
	a, b : PrattValue
}

PrattValue :: struct {
	
	body : union {
		^PrattNode,
		Variant,
		Operator,
		Delimiter,
	},
}

PrattExpression :: [dynamic]PrattValue

// --- Variables ---

@(rodata)
prefix_operators := bit_set[Operator] {
	.Not,
}

@(rodata)
postfix_operators := bit_set[Operator] {
	
}

// --- Procedures ---


pratt_parse :: proc(
	expr  : ^PrattExpression,
	alloc : mem.Allocator
) -> (result : Variant, err : Error) {
	if expr == nil     do return nil, .Expression_Invalid
	if len(expr^) == 0 do return nil, .Expression_Invalid
	
	IDIOT_PRINT :: false
	// TODO: figure out how to make
	//		 this recursive instead
	//		 of looping for forever
	
	// Parse until only one node left
	num : int
	last_le, le : int
	for ;; {
		
		le = len(expr^)
		if le == 1 do break
		if le == last_le do return nil, .Expression_Invalid
		
		num += 1
		when IDIOT_PRINT do fmt.printfln("\n> Expression Loop %i:", num)
		
		// Loop through expression values
		found : bool
		for v, i in expr {
			
			when IDIOT_PRINT do fmt.println("Member", i, v)
			// Check for value or node
			#partial switch &t in v.body {
			case Variant: // Is a value
			case ^PrattNode:
			
				// Is the node a value or
				// Non complete operation
				switch t.type {
				case .Infix:
					if t.a.body == nil || t.b.body == nil do continue
				case .Prefix:  // Must be finished
				case .Postfix: // Must be finished
				}
				
			case: continue
			}
			
			when IDIOT_PRINT do fmt.println("Found Value")
			l, r := pratt_get_surrounding(expr, i)
			
			// Precedence
			PRC_NONE :: -10
			pl := PRC_NONE if l.body == nil else 0
			pr := PRC_NONE if r.body == nil else 0
			
			PRC_PAREN :: -2 // Wait to solve parentheses
			if pl != PRC_NONE do #partial switch &t in l.body {
			case Operator:   pl = get_precedence(t)
			case Delimiter:  pl = PRC_PAREN
			case ^PrattNode:
				if t.b.body != nil do return nil, .Expression_Invalid
				pl = get_precedence(t.op)
			case:
				return nil, .Expression_Invalid
			}
			
			if pr != PRC_NONE do #partial switch &t in r.body {
			case Operator:   pr = get_precedence(t)
			case Delimiter:  pr = PRC_PAREN
			case ^PrattNode:
				if t.a.body != nil do return nil, .Expression_Invalid
				pr = get_precedence(t.op)
			case:
				return nil, .Expression_Invalid
			}
			
			// Can't have multiple values
			// Or unknown powers next to each other
			if pl == 0 || pr == 0 do return nil, .Expression_Invalid
			when IDIOT_PRINT do fmt.println("Precedences:", pl, pr)
			
			join_type : enum {
				Left,
				Right,
				Remove_Paren,
			}
			
			if pr > pl  do join_type = .Right
			if pl == pr && pl == PRC_PAREN {
				if l.body.(Delimiter) == .ParenL && r.body.(Delimiter) == .ParenR {
					join_type = .Remove_Paren
				} else {
					// Can't solve this for now!
					continue
				}
			}
			
			switch join_type {
			case .Left:
				
				#partial switch &t in l.body {
				case Operator:
					node := pratt_node(alloc)
					node.b  = v
					node.op = t
					
					if t in prefix_operators  do node.type = .Prefix
					
					ordered_remove(expr, i)
					expr^[i - 1].body = node
					
				case ^PrattNode:
					t.b = v
					ordered_remove(expr, i)
				}
				
			case .Right:
				
				#partial switch &t in r.body {
				case Operator:
					node := pratt_node(alloc)
					node.a  = v
					node.op = t
					
					if t in postfix_operators do node.type = .Postfix
					
					ordered_remove(expr, i)
					expr^[i].body = node
					
				case ^PrattNode:
					t.a = v
					ordered_remove(expr, i)
				}
				
			case .Remove_Paren:
				
				// Remove left and then right
				ordered_remove(expr, i - 1)
				ordered_remove(expr, i)
			}
			
			when IDIOT_PRINT do fmt.println("Operation:", join_type)
			
			// Continue onto next loop
			found = true
			break
		}
		
		last_le = le
		when IDIOT_PRINT do fmt.println("Temp tree:", expr^)
		// Operation can't be completed
		if found == false do return nil, .Expression_Invalid
	}
	
	when IDIOT_PRINT do fmt.println("Expression tree:", expr^)
	
	// Tree has been created, now solve operations
	pratt_solve_value :: proc(value : PrattValue) -> (result : Variant, err : Error) {
		#partial switch &t in value.body {
		case ^PrattNode:
			switch t.type {
			case .Infix:
				if t.a.body == nil || t.b.body == nil do return nil, .Expression_Invalid
				
				a, b : Variant
				#partial switch &v in t.a.body {
				case Variant: a = v
				case ^PrattNode:
					ra, a_err := pratt_solve_value({v})
					if a_err != nil do return nil, a_err
					
					a = ra
				
				case: return nil, .Expression_Invalid
				}
				
				#partial switch &v in t.b.body {
				case Variant: b = v
				case ^PrattNode:
					rb, b_err := pratt_solve_value({v})
					if b_err != nil do return nil, b_err
					
					b = rb
					
				case: return nil, .Expression_Invalid
				}
				
				result = operation(a, b, t.op)
				
			case .Prefix:
				if t.b.body == nil do return nil, .Expression_Invalid
				
				a : Variant
				#partial switch &v in t.b.body {
				case Variant: a = v
				case ^PrattNode:
					ra, a_err := pratt_solve_value({v})
					if a_err != nil do return nil, a_err
					
					a = ra
				
				case: return nil, .Expression_Invalid
				}
				
				result = prefix_operation(a, t.op)
			
			case .Postfix:
				if t.a.body == nil do return nil, .Expression_Invalid
				
				a : Variant
				#partial switch &v in t.a.body {
				case Variant: a = v
				case ^PrattNode:
					ra, a_err := pratt_solve_value({v})
					if a_err != nil do return nil, a_err
					
					a = ra
				
				case: return nil, .Expression_Invalid
				}
				
				result = postfix_operation(a, t.op)
			}
			
		case Variant:
			return t, nil
		}
		
		return
	}
	
	result, err = pratt_solve_value(expr^[0])
	if err != nil    do return
	if result == nil do return nil, .Expression_Invalid
	return
}

@(private="file")
/* --- pratt_get_surrounding ---
 * get surrounding values
 * from a pratt expression
 */
pratt_get_surrounding :: proc(
	expr : ^PrattExpression, i : int
) -> (left, right : PrattValue) {
	
	le := len(expr^)
	
	l := i - 1
	r := i + 1
	
	left  = (expr^[l] if l < le && l >= 0 else {})
	right = (expr^[r] if r < le && r >= 0 else {})
	
	return
}

@(private="file")
/* --- pratt_node ---
 * allocate a new pratt node
 * with given allocator
 */
pratt_node :: proc(
	alloc : mem.Allocator
) -> ^PrattNode {
	return new(PrattNode, alloc)
}