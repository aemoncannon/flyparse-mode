// This file is based heavily on David Holroyd's AS3 Grammar from METAAS
//
// This file is part of flyparse-mode
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

grammar Javascript;

options {
	k = 2;
	output=AST;
	language=Java;
    ASTLabelType=FlyparseTree;
}

tokens {
    NEW_EXPRESSION;
    PROP_ACCESS; ARRAY_ACCESS; ARRAY_SUBSCRIPT; E4X_EXPRESSION;PRIMARY_EXPRESSION;CLASS_NAME;
    EXPRESSION;ADDITIVE_EXP;EXTENDS_CLAUSE;IMPLEMENTS_CLAUSE;INCLUDE_DIRECTIVE;METHOD_NAME;
	PACKAGE_DECL;IMPORT_DEF;DECLARATION;VAR_DECLARATION;VAR_INITIALIZER;RETURN_STATEMENT;
	CONDITION;ACCESSOR_ROLE;SWITCH_STATEMENT_LIST;FOR_IN;FOR_EACH;VARIABLE_DEF;FOR_STMT;FOR_IN_STMT;
	IDENTIFIER_STAR;ANNOTATION;ANNOTATIONS;ANNOTATION_PARAMS;ARGUMENTS;ARGUMENT;PRE_DEC;PRE_INC;
	PROPERTY_OR_IDENTIFIER;
	COMPILATION_UNIT;
	PACKAGE;
	IMPORT;
	METADATA;
	METADATA_ITEM;
	CLASS_DEF; INTERFACE_DEF;
	EXTENDS_CLAUSE; IMPLEMENTS_CLAUSE; TYPE_BLOCK;METHOD_BLOCK;
	MODIFIERS; VARIABLE_DEF; METHOD_DEF; NAMESPACE_DEF; PARAMS; PARAM; TYPE_SPEC;
	BLOCK; EXPR; ELIST; EXPR_STMNT;
	NEW_EXPR; ENCPS_EXPR;
	VAR_INIT;
	FUNCTION_CALL; ARRAY_ACC;
	UNARY_PLUS; UNARY_MINUS; POST_INC; POST_DEC;
	ARRAY_LITERAL; ELEMENT; OBJECT_LITERAL; OBJECT_FIELD; FUNC_DEF;FUNC_DEC; FUNC;
	FOR_INIT; FOR_CONDITION; FOR_ITERATOR;
	CLASS;INTERFACE;EXTENDS;IMPLEMENTS;
	METHOD;NAMESPACE;FOR_IN_CLAUSE;FOR_CLAUSE;FOR_LOOP;
	CASE;CASE_DEFAULT;SWITCH_BLOCK;SWITCH;BREAK;
	CONTINUE;
	RETURN;BREAK;IF;ELSE;THROW;ELSE_CLAUSE;IF_STMT;THROW_STMT;
	STATEMENT;WHILE;DO_WHILE;WHILE;
	STATEMENT_BLOCK;PARAM_DECL;PARAM_REST_DECL;VAR_DEC;VARIABLE_DECLARATOR;
	PARAM_LIST;WITH;IDENTIFIER;
	MODIFIER_LIST;CLASS_MEMBER;
	REGEX; 
	XML; 
	NAMESPACE_USAGE;
	CONSTANT;LITERAL_NUMBER;LITERAL_STRING;LITERAL_DOUBLE_STRING;LITERAL_SINGLE_STRING;
	LITERAL_REGEX;LITERAL_XML;
	DEFAULT_XML_NAMESPACE;
    CONSTANT;
}

scope InOperator {
	boolean allowed;
}

scope LexicalContext {
	boolean preferConstant;
}

@parser::header {
            package emacs.flyparse.javascript;
            import emacs.flyparse.*;
}

@lexer::header {
            package emacs.flyparse.javascript;
            import emacs.flyparse.*;
}

@lexer::members {
            private Token lastToken;

            public Token nextToken() {
                CommonToken t = (CommonToken)super.nextToken();
                if(t.getChannel() != HIDDEN){
                    lastToken = t;
                }
                return t;
            }

            private boolean constantIsOk() {
                int type = lastToken.getType();
                return type == ASSIGN || type == LPAREN || type == LBRACK || type == RETURN || type == COLON || type == LNOT;
            }
}

@parser::members {
            private JavascriptLexer lexer;
            private CharStream cs;
            
            public void setInput(JavascriptLexer lexer, CharStream cs) {
                this.lexer = lexer;
                this.cs = cs;
            }

       /* Javascript plays fast and loose with semicolons - they are optional in many situations - 
       so, to simplify things, we ignore mismatches at SEMIs. */ 
            protected void mismatch(IntStream input, int ttype, BitSet follow) throws RecognitionException {
                if(ttype != SEMI){
                    throw new MismatchedTokenException(ttype, input);
                }		
            }

}


/**
 * this is the start rule for this parser
 */
compilationUnit
	: 
        sourceElement* 
        endOfFile
        -> ^(COMPILATION_UNIT sourceElement*)
	;

sourceElement
    : 
        (FUNCTION ident) => functionDeclaration | statement
    ;   

endOfFile
	:	EOF!
	;

semi 
	: SEMI
	; 


variableDefinition
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	VAR variableDeclarator (COMMA variableDeclarator)* semi
		-> ^(VARIABLE_DEF VAR variableDeclarator+)
	;

variableDeclarator
	:	ident variableInitializer?
        -> ^(VAR_DECLARATION ident variableInitializer?)
	;

declaration
	:	VAR variableDeclarator declarationTail
        -> ^(DECLARATION VAR variableDeclarator declarationTail)
	;

declarationTail
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	(COMMA! variableDeclarator)*
	;

variableInitializer
	:	ASSIGN expression
        -> ^(VAR_INITIALIZER ASSIGN expression)
	;

// A list of formal parameters
// TODO: shouldn't the 'rest' parameter only be allowed in the last position?
parameterDeclarationList
	:	LPAREN
		(	parameterDeclaration
			(COMMA parameterDeclaration)*
		)?
		RPAREN
		-> ^(PARAMS LPAREN parameterDeclaration* RPAREN)
	;


parameterDeclaration
	:	basicParameterDeclaration
	;

basicParameterDeclaration
	:	ident
		-> ^(PARAM ident)
	;


block
	:	LCURLY blockEntry* RCURLY
		-> ^(BLOCK LCURLY blockEntry* RCURLY)
	;

blockEntry
	: statement
	;

condition
	:	LPAREN expression RPAREN
		-> ^(CONDITION expression)
	;

statement
	:	(LCURLY)=> block
	|	declarationStatement 
	|	expressionStatement
	|	ifStatement
	// For statement
	|	forStatement

	// While statement
	|	whileStatement

	// do-while statement
	|	doWhileStatement
	
	// with statement
	|	withStatement
	
	// switch statement
	|	switchStatement
	
	// get out of a loop (or switch)
	|	breakStatement

	// do next iteration of a loop
	|	continueStatement

	// Return an expression
	|	returnStatement

	// throw an exception
	|	throwStatement
	
	// handle exceptions
	|	tryStatement
	
	// empty statement
	|	SEMI

	;

declarationStatement
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	declaration semi
	;

expressionStatement
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	expressionList semi
		-> ^(EXPR_STMNT expressionList)
	;
	
ifStatement
	:	IF condition statement
		((ELSE)=>elseClause)?
        -> ^(IF_STMT IF condition statement elseClause?)
	;

elseClause
	:	ELSE statement
        -> ^(ELSE_CLAUSE ELSE statement)
	;

throwStatement
	:	'throw' expression semi
        -> ^(THROW_STMT expression)
	;

tryStatement
	:	'try'
		block
		catchBlock*
		finallyBlock?
	;

catchBlock
	:	'catch' LPAREN! ident RPAREN!
		block
	;

finallyBlock
	:	'finally' block
	;

returnStatement
	:	RETURN expression? semi
        -> ^(RETURN_STATEMENT RETURN expression?)
	;
		
continueStatement
	:	CONTINUE semi
	;

breakStatement
	:	BREAK semi
	;

switchStatement
	:	SWITCH condition
		switchBlock
	;

switchBlock
	:	LCURLY
		(caseStatement)*
		(defaultStatement)?
		RCURLY
		-> ^(BLOCK caseStatement* defaultStatement?)
	;

caseStatement
	:	CASE expression COLON! l=switchStatementList 
	;
	
defaultStatement
	:	DEFAULT COLON! l=switchStatementList 
	;

switchStatementList
	:	statement* -> ^(SWITCH_STATEMENT_LIST statement*)
	;

forStatement
scope InOperator;
@init {
	$InOperator::allowed = false;
}
	:	f=FOR
		LPAREN
		(	(forInClauseDecl IN)=>forInClause RPAREN statement
			-> ^(FOR_IN_STMT FOR_IN[$f] forInClause statement)

		|	traditionalForClause RPAREN statement
			-> ^(FOR_STMT $f traditionalForClause statement)
		)
	;

traditionalForClause
	:	a=forInit  SEMI!	// initializer
		b=forCond  SEMI!	// condition test
		c=forIter 		// updater
	;

forInClause
	:	forInClauseDecl IN! forInClauseTail
	;

forInClauseDecl
scope InOperator;
@init {
	$InOperator::allowed = false;
}
	:	declaration | ident
	;


forInClauseTail
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	expressionList
	;

// The initializer for a for loop
forInit	
scope InOperator;
@init {
	$InOperator::allowed = false;
}
	:	(declaration | expressionList )?
		-> ^(FOR_INIT declaration? expressionList?)
	;

forCond
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	expressionList?
		-> ^(FOR_CONDITION expressionList?)
	;

forIter
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	expressionList?
		-> ^(FOR_ITERATOR expressionList?)
	;

whileStatement
	:	WHILE^ condition statement
	;

doWhileStatement
	:	DO^ statement WHILE! condition semi
	;

withStatement
	:	WITH^ condition statement
	;


identifier 
	:	( 	qualifiedIdent -> qualifiedIdent
		)
		(	options{greedy=true;}
		: 	poi=propOrIdent -> $poi
		)*
		-> ^(IDENTIFIER $identifier)
	;

qualifiedIdent
	:	ident 
	;

identifierStar
	:	ident
		(	options{greedy=true;}
		:	DOT ident
		)* 
		(	DOT STAR
		)?
		-> ^(IDENTIFIER_STAR ident+ STAR?)
	;

arguments
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	LPAREN expressionList RPAREN
		-> ^(ARGUMENTS LPAREN expressionList RPAREN)
	|	LPAREN RPAREN
		-> ^(ARGUMENTS LPAREN RPAREN)
	;
// This is an initializer used to set up an array.
arrayLiteral
	:	LBRACK elementList? RBRACK
		-> ^(ARRAY_LITERAL LBRACK elementList? RBRACK)
	;
		
elementList
	:	COMMA!
	|	nonemptyElementList
	;
nonemptyElementList
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	assignmentExpression (COMMA! assignmentExpression)*
	;

element
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	assignmentExpression
		-> ^(ELEMENT assignmentExpression)
	;

// This is an initializer used to set up an object.
objectLiteral
	:	LCURLY fieldList? RCURLY
		-> ^(OBJECT_LITERAL LCURLY fieldList? RCURLY)
	;
	
fieldList
	:	literalField (COMMA! literalField?)*
	;
	
literalField 
	: 	fieldName COLON element
		-> ^(OBJECT_FIELD fieldName element)
	;
	
fieldName
	:	ident
	|	number
    |   stringLiteral
	;

// the mother of all expressions
expression
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	assignmentExpression
        -> ^(EXPRESSION assignmentExpression)
	;

// This is a list of expressions.
expressionList
	:	assignmentExpression (COMMA assignmentExpression)*
		-> assignmentExpression+
	;

// assignment expression (level 13)
assignmentExpression
	:	conditionalExpression 
           ((assignmentOperator) => assignmentOperator assignmentExpression)*
	;

assignmentOperator
	:	ASSIGN
	| 	STAR_ASSIGN
	|	DIV_ASSIGN
	|	MOD_ASSIGN
	|	PLUS_ASSIGN
	|	MINUS_ASSIGN
	|	SL_ASSIGN
	|	SR_ASSIGN
	|	BSR_ASSIGN
	|	BAND_ASSIGN
	|	BXOR_ASSIGN
	|	BOR_ASSIGN
	|	LAND_ASSIGN
	|	LOR_ASSIGN
	;

// conditional test (level 12)
conditionalExpression
	:	(logicalOrExpression -> logicalOrExpression)
		(
			QUESTION
			conditionalSubExpression
			-> ^(QUESTION $conditionalExpression conditionalSubExpression)
		)?
	;
conditionalSubExpression
	:	assignmentExpression COLON^ assignmentExpression
	;

// TODO: should 'and'/'or' have same precidence as '&&'/'||' ?

// logical or (||)  (level 11)
logicalOrExpression
	:	logicalAndExpression
		(logicalOrOperator logicalAndExpression)*
	;

logicalOrOperator
	:	LOR | 'or'
	;

// logical and (&&)  (level 10)
logicalAndExpression
	:	bitwiseOrExpression
		(logicalAndOperator^ bitwiseOrExpression)*
	;

logicalAndOperator
	:	LAND | 'and'
	;

// bitwise or non-short-circuiting or (|)  (level 9)
bitwiseOrExpression
	:	bitwiseXorExpression
		(BOR bitwiseXorExpression)*
	;

// exclusive or (^)  (level 8)
bitwiseXorExpression
	:	bitwiseAndExpression
		(BXOR bitwiseAndExpression)*
	;

// bitwise or non-short-circuiting and (&)  (level 7)
bitwiseAndExpression
	:	equalityExpression
		(BAND equalityExpression)*
	;

// equality/inequality (==/!=) (level 6)
equalityExpression
	:	relationalExpression
	(	equalityOperator^
		relationalExpression
	)*
	;

equalityOperator
	:	STRICT_EQUAL | STRICT_NOT_EQUAL | NOT_EQUAL | EQUAL
	;
	
// boolean relational expressions (level 5)
relationalExpression
	:	shiftExpression
		((relationalOperator)=> relationalOperator^ shiftExpression)*
	;

relationalOperator
	:	{$InOperator::allowed}? IN
	|	LT | GT | LE | GE | IS | AS | 'instanceof'
	;

// bit shift expressions (level 4)
shiftExpression
	:	additiveExpression
		(shiftOperator additiveExpression)*
	;

shiftOperator
	:	SL | SR | BSR
	;

// binary addition/subtraction (level 3)
additiveExpression
	:	multiplicativeExpression (additiveOperator multiplicativeExpression)*
	;

additiveOperator
	:	PLUS | MINUS
	;

// multiplication/division/modulo (level 2)
multiplicativeExpression
	:	unaryExpression
		(	multiplicativeOperator
			unaryExpression
		)*
	;

multiplicativeOperator
	:	STAR | DIV | MOD
	;

//	(level 1)
unaryExpression
	:	in=INC unaryExpression
	|	de=DEC unaryExpression
	|	MINUS unaryExpression
	|	PLUS unaryExpression
	|	unaryExpressionNotPlusMinus
	;

unaryExpressionNotPlusMinus
	:	'delete' postfixExpression -> ^('delete' postfixExpression)
	|	'void' unaryExpression -> ^('void' unaryExpression)
	|	'typeof' unaryExpression -> ^('typeof' unaryExpression)
	|	LNOT unaryExpression -> ^(LNOT unaryExpression)
	|	BNOT unaryExpression -> ^(BNOT unaryExpression)
	|	postfixExpression
	;

/* Array expressions, function invocation, post inc/dec

Note: $postfixExpression refers to the current tree for this
rule. So, in the *, we are repeatedly re-parenting the tree */
postfixExpression
	:	(primaryExpression -> primaryExpression)
		(	//qualified names
            propOrIdent
            -> ^(PROP_ACCESS $postfixExpression propOrIdent)
            
		|	//array access
            LBRACK expression RBRACK
            -> ^(ARRAY_ACCESS $postfixExpression ^(ARRAY_SUBSCRIPT LBRACK expression RBRACK))

		|	// A method invocation
            arguments
            -> ^(FUNCTION_CALL $postfixExpression arguments)
		)*
        
		( 	INC -> ^(POST_INC $postfixExpression INC)
	 	|	DEC -> ^(POST_DEC $postfixExpression DEC)
		)?

 	;

primaryExpression
	:	'undefined'
	|	constant
        -> ^(CONSTANT constant)
	|	arrayLiteral
	|	objectLiteral
	|	functionDefinition
	|	newExpression
	|	encapsulatedExpression
	|	qualifiedIdent

	;

propOrIdent
	:	
		DOT qualifiedIdent
		/* without further semantic analysis, we can't
		   tell if a.b is an access of the property 'b'
		   from the var 'a' or a reference to the type
		   'b' in the package 'a'.  (This could be
		   resolved in an AST post-processing step) */
		-> ^(PROPERTY_OR_IDENTIFIER qualifiedIdent)
	;

constant
	:	
		regexpLiteral
	|	number -> ^(LITERAL_NUMBER number)
	|	stringLiteral -> ^(LITERAL_STRING stringLiteral)
	|	TRUE
	|	FALSE
	|	NULL
	;

stringLiteral
    : stringLiteralDouble | stringLiteralSingle
    ;

stringLiteralDouble
    : STRING_LITERAL_DOUBLE -> ^(LITERAL_DOUBLE_STRING STRING_LITERAL_DOUBLE)
    ;

stringLiteralSingle
    : STRING_LITERAL_SINGLE -> ^(LITERAL_SINGLE_STRING STRING_LITERAL_SINGLE)
    ;


number	:	
        HEX_LITERAL
	|	DECIMAL_LITERAL
	|	OCTAL_LITERAL
	|	FLOAT_LITERAL
	;

regexpLiteral
	:	REGEX_LITERAL
		-> ^(LITERAL_REGEX REGEX_LITERAL)
	;

newExpression
	:	NEW primaryExpression arguments -> ^(NEW_EXPRESSION NEW primaryExpression arguments)
	;


propertyOperator
	:	DOT^ qualifiedIdent
	|	brackets
	;

brackets
@init {
	$InOperator::allowed = true;
}
	:	LBRACK expressionList RBRACK
	;

encapsulatedExpression
scope InOperator;
@init {
	$InOperator::allowed = true;
}
	:	LPAREN assignmentExpression RPAREN
		-> ^(ENCPS_EXPR assignmentExpression)
	;

// TODO: should anonymous and named functions have seperate definitions so that
// we can dissallow named functions in expressions?

functionDefinition
	:	FUNCTION ident? parameterDeclarationList block
		-> ^(FUNC ident? parameterDeclarationList  block)
	;

functionDeclaration
	:	FUNCTION ident parameterDeclarationList block
		-> ^(FUNC_DEC FUNCTION ident parameterDeclarationList  block)
	;


ident
	:	IDENT
	;


FUNCTION	:	'function';
VAR		:	'var';
IF		:	'if';
FOR		:	'for';
IN		:	'in';
WHILE		:	'while';
DO		:	'do';
SWITCH		:	'switch';
CASE		:	'case';
DEFAULT		:	'default';
ELSE		:	'else';
CONST		:	'const';
CLASS		:	'class';
TRUE		:	'true';
FALSE		:	'false';
IS		:	'is';
AS		:	'as';
WITH		:	'with';
RETURN		:	'return';
CONTINUE	:	'continue';
BREAK		:	'break';
NULL		:	'null';
NEW		    :	'new';

// OPERATORS
QUESTION		:	'?'	;
LPAREN			:	'('	;
RPAREN			:	')'	;
LBRACK			:	'['	;
RBRACK			:	']'	;
LCURLY			:	'{'	;
RCURLY			:	'}'	;
COLON			:	':'	;
DBL_COLON		:	'::'	;
COMMA			:	','	;
ASSIGN			:	'='	;
EQUAL            :	'==' ;
STRICT_EQUAL		:	'==='	;
LNOT			:	'!'	;
BNOT			:	'~'	;
NOT_EQUAL		:	'!='	;
STRICT_NOT_EQUAL	:	'!=='	;
PLUS			:	'+'	;
PLUS_ASSIGN		:	'+='	;
INC			:	'++'	;
MINUS			:	'-'	;
MINUS_ASSIGN		:	'-='	;
DEC			:	'--'	;
STAR			:	'*'	;
STAR_ASSIGN		:	'*='	;
MOD			:	'%'	;
MOD_ASSIGN		:	'%='	;
SR			:	'>>'	;
SR_ASSIGN		:	'>>='	;
BSR			:	'>>>'	;
BSR_ASSIGN		:	'>>>='	;
GE			:	'>='	;
GT			:	'>'	;
SL			:	'<<'	;
SL_ASSIGN		:	'<<='	;
LE			:	'<='	;
LT			:	'<'	;
BXOR			:	'^'	;
BXOR_ASSIGN		:	'^='	;
BOR			:	'|'	;
BOR_ASSIGN		:	'|='	;
LOR			:	'||'	;
BAND			:	'&'	;
BAND_ASSIGN		:	'&='	;
LAND			:	'&&'	;
LAND_ASSIGN		:	'&&='	;
LOR_ASSIGN		:	'||='	;
E4X_ATTRI		:	'@'	; 
SEMI			:	';'	;
BSLASH          :   '\\';

DOT		    :	'.'	 ;
E4X_DESC	:	'..'	;
REST		:	'...'	;


REGEX_LITERAL
	: { constantIsOk() }?=> '/' REGEX_BODY '/' REGEX_POSTFIX?
	;

DIV_ASSIGN		:	'/=';

DIV	            :	'/';

fragment REGEX_POSTFIX
    : ('a'..'z'|'A'..'Z'|'_'|'0'..'9'|'$')+
    ;

fragment REGEX_BODY
	:	(	(~('\n'|'\r'|'*'|'/'|'\\'))
		|	'\\'(~('\n'|'\r'))
		)
		(	(~('\n'|'\r'|'/'|'\\'))
		|	'\\'(~('\n'|'\r'))
		)*
    ;

IDENT 
    :
        ('a'..'z'|'A'..'Z'|'_'|'$')
    	('a'..'z'|'A'..'Z'|'_'|'0'..'9'|'$')*
	;

STRING_LITERAL_DOUBLE
	:	'"' (ESC|~('"'|'\\'|'\n'|'\r'))* '"'
	;

STRING_LITERAL_SINGLE
	:	'\'' (ESC|~('\''|'\\'|'\n'|'\r'))* '\''
	;


HEX_LITERAL	:	'0' ('x'|'X') HEX_DIGIT+ ;

DECIMAL_LITERAL	:	('0' | '1'..'9' '0'..'9'*) ;

OCTAL_LITERAL	:	'0' ('0'..'7')+ ;

FLOAT_LITERAL
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
	;


// whitespace -- ignored
WS	:	(
			' '
		|	'\t'
		|	'\f'
		)+
		{$channel=HIDDEN;}
	;
NL	
	:	(
			'\r' '\n'  	// DOS
		|	'\r'    	// Mac
		|	'\n'    	// Unix
		)
		{$channel=HIDDEN;}
	;
	
// skip BOM bytes
BOM	:	(	'\u00EF'  '\u00BB' '\u00BF'
		|	'\uFEFF'
		)
		{ $channel=HIDDEN; };

// might be better to filter this out as a preprocessing step
INCLUDE_DIRECTIVE
	:	'#include'
	;

// single-line comments
SL_COMMENT
	:	'//' (~('\n'|'\r'))* ('\n'|'\r'('\n')?)?
		{$channel=HIDDEN;}
	;
// multiple-line comments
ML_COMMENT
	:	'/*' ( options {greedy=false;} : . )* '*/'
		{$channel=HIDDEN;}
	;

fragment EXPONENT
	:	('e'|'E') ('+'|'-')? ('0'..'9')+
	;
fragment HEX_DIGIT
	:	('0'..'9'|'A'..'F'|'a'..'f')
	;

fragment OCT_DIGIT
	:	'0'..'7'
	;
	
fragment ESC
	:   CTRLCHAR_ESC
	|   UNICODE_ESC
	|   OCTAL_ESC
	;

fragment CTRLCHAR_ESC
	:	'\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\'|'/')
	;

fragment OCTAL_ESC
	:   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
	|   '\\' ('0'..'7') ('0'..'7')
	|   '\\' ('0'..'7')
	;

fragment UNICODE_ESC
	:   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
	;

