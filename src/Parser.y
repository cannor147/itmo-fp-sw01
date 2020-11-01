{
module Parser where

import Lexer
import Grammar
}

%name parser
%tokentype { Token }
%error { parseError }

%token
	'\\'				{ Lexer.Slash }
	'||'				{ Lexer.Or }
	'^'				{ Lexer.Xor }
	'&&'				{ Lexer.And }
	'!'				{ Lexer.Not }
	'=='				{ Lexer.Eq }
	'!='				{ Lexer.Ne }
	'>='				{ Lexer.Ge }
	'<='				{ Lexer.Le }
	'>'				{ Lexer.Gt }
	'<'				{ Lexer.Lt }
	'+'				{ Lexer.Add }
	'-'				{ Lexer.Sub }
	'*'				{ Lexer.Mul }
	'/'				{ Lexer.Div }
	'.'				{ Lexer.Dot }
	','				{ Lexer.Comma }
	':'				{ Lexer.Colon }
	';'				{ Lexer.Semicolon }
	'='				{ Lexer.Assignment }
	'?'				{ Lexer.Question }
	'('				{ Lexer.OpeningBracket }
	')'				{ Lexer.ClosingBracket }
	'['				{ Lexer.OpeningSquareBracket }
	']'				{ Lexer.ClosingSquareBracket }
	'{'				{ Lexer.OpeningBrace }
	'}'				{ Lexer.ClosingBrace }
	import				{ Lexer.Import }
	public				{ Lexer.Public }
	static				{ Lexer.Static }
	class				{ Lexer.Class }
	void				{ Lexer.Void }
	main				{ Lexer.Main }
	java				{ Lexer.Java }
	util				{ Lexer.Util }
	scanner				{ Lexer.Scanner }
	system				{ Lexer.System }
	in				{ Lexer.In }
	out				{ Lexer.Out }
	nextInt				{ Lexer.NextInt }
	nextDouble			{ Lexer.NextDouble }
	nextBoolean			{ Lexer.NextBoolean }
	nextLine			{ Lexer.NextLine }
	print				{ Lexer.Print }
	printLn				{ Lexer.PrintLn }
	new				{ Lexer.New }
	if				{ Lexer.If }
	else				{ Lexer.Else }
	while				{ Lexer.While }
	return				{ Lexer.Return }
	equals				{ Lexer.Equals }
	boolean				{ Lexer.TBoolean }
	int				{ Lexer.TInt }
	double				{ Lexer.TDouble }
	string				{ Lexer.TString }
	booleanValue			{ Lexer.BooleanValue $$ }
	intValue			{ Lexer.IntValue $$ }
	doubleValue			{ Lexer.DoubleValue $$ }
	stringValue			{ Lexer.StringValue $$ }
	name				{ Lexer.Name $$ }

%%

Program
	: Imports Class							{ Grammar.Program $1 $2 }
Imports
	: import java '.' util '.' scanner ';'				{ True  }
	| {- empty -}							{ False }
Class
	: public class name '{' Function '}'				{ Grammar.Class $3 $5 }
Function
	: public static void main '(' string '[' ']' name ')' Block	{ Grammar.Main $11 }

Block
	: '{' Statements '}'						{ $2 }
Statements
	: Statement Statements						{ $1:$2 }
	| {- empty -}							{ []    }
Statement
	: Type name '=' Expression					{ Grammar.DeclarationStatement $1 $2 $4 }
	| name '=' Expression						{ Grammar.AssignmentStatement $1 $3     }
	| if '(' Expression ')' Block else Block			{ Grammar.IfStatement $3 $5 $7          }
	| while '(' Expression ')' Block				{ Grammar.WhileStatement $3 $5          }
	| system '.' out '.' print '(' Expression ')' ';'		{ Grammar.PrintStatement $7             }
	| system '.' out '.' print '(' Expression ')' ';'		{ Grammar.PrintStatement $7             }
	| system '.' out '.' printLn '(' Expression ')' ';'		{ Grammar.PrintLnStatement $7           }

Type
	: boolean							{ Grammar.TBoolean }
	| int								{ Grammar.TInt     }
	| double							{ Grammar.TDouble  }
	| string							{ Grammar.TString  }

Expression
	: ExpressionTernary						{ $1 }
ExpressionTernary
	: ExpressionOr '?' Expression ':' ExpressionTernary		{ Grammar.Ternary $1 $3 $5 }
	| ExpressionOr							{ $1			   }
ExpressionOr
	: ExpressionOr '||' ExpressionAnd				{ Grammar.Or $1 $3 }
	| ExpressionAnd							{ $1		   }
ExpressionAnd
	: ExpressionAnd '&&' ExpressionEq				{ Grammar.And $1 $3 }
	| ExpressionEq							{ $1		    }
ExpressionEq
	: ExpressionEq '==' ExpressionComp				{ Grammar.Eq $1 $3 }
	| ExpressionEq '!=' ExpressionComp				{ Grammar.Ne $1 $3 }
	| ExpressionComp						{ $1	   }
ExpressionComp
	: ExpressionAdd '>=' ExpressionAdd				{ Grammar.Ge $1 $3 }
	| ExpressionAdd '<=' ExpressionAdd				{ Grammar.Le $1 $3 }
	| ExpressionAdd '>' ExpressionAdd				{ Grammar.Gt $1 $3 }
	| ExpressionAdd '<' ExpressionAdd				{ Grammar.Lt $1 $3 }
	| ExpressionAdd 						{ $1		   }
ExpressionAdd
	: ExpressionAdd '+' ExpressionMultiply				{ Grammar.Add $1 $3 }
	| ExpressionAdd '-' ExpressionMultiply				{ Grammar.Sub $1 $3 }
	| ExpressionMultiply						{ $1		    }
ExpressionMultiply
	: ExpressionMultiply '*' ExpressionUnary			{ Grammar.Mul $1 $3 }
	| ExpressionMultiply '/' ExpressionUnary			{ Grammar.Div $1 $3 }
	| ExpressionUnary						{ $1		    }
ExpressionUnary
	: '!' ExpressionUnary						{ Grammar.Not $2    		               }
	| '-' ExpressionValue						{ Grammar.Negate $2 		               }
	| ExpressionValue						{ $1		    		               }
	| name '.' equals '(' Expression ')'				{ Grammar.StringEq (Grammar.Variable $1) $5    }
	| stringValue '.' equals '(' Expression ')'			{ Grammar.StringEq (Grammar.StringValue $1) $5 }
ExpressionValue
	: booleanValue							{ Grammar.BooleanValue $1 }
	| intValue							{ Grammar.IntValue $1	  }
	| doubleValue							{ Grammar.DoubleValue $1  }
	| stringValue							{ Grammar.StringValue $1  }
	| name								{ Grammar.Variable $1	  }
	| FunctionCall							{ $1 			  }
	| '(' Expression ')'						{ $2			  }
FunctionCall
	: name '(' ')'							{ Grammar.FunctionCall0 $1       }
	| name '(' Expression ')'					{ Grammar.FunctionCall1 $1 $3    }
	| name '(' Expression ',' Expression ')'			{ Grammar.FunctionCall2 $1 $3 $5 }
	| FunctionNext '(' ')'						{ Grammar.FunctionCall0 $1       }
FunctionNext
	: new scanner '(' system '.' in ')' '.' nextBoolean		{ "new Scanner(System.in).nextBoolean" }
        | new scanner '(' system '.' in ')' '.' nextInt			{ "new Scanner(System.in).nextInt"     }
        | new scanner '(' system '.' in ')' '.' nextDouble		{ "new Scanner(System.in).nextDouble"  }
        | new scanner '(' system '.' in ')' '.' nextLine		{ "new Scanner(System.in).nextLine"    }


{
parseError _ = error "Parse error"
}