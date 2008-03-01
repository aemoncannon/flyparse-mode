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

//	@authors Martin Schnabel, David Holroyd, Aemon Cannon
//  
grammar AS3;

options {
	output=AST;
	language=Java;
    ASTLabelType=FlyparseTree;
}

tokens {
    NEW_EXPRESSION;IF_STMT;ELSE_CLAUSE;NAME;QUALIFIED_NAME;ASSIGNMENT_EXPR;
    PROP_ACCESS; ARRAY_ACCESS; ARRAY_SUBSCRIPT; E4X_EXPRESSION;PRIMARY_EXPRESSION;CLASS_NAME;
    EXPRESSION;ADDITIVE_EXP;EXTENDS_CLAUSE;IMPLEMENTS_CLAUSE;INCLUDE_DIRECTIVE;METHOD_NAME;
	PACKAGE_DECL;IMPORT_DEF;DECLARATION;VAR_DECLARATION;VAR_INITIALIZER;RETURN_STATEMENT;
	CONDITION;ACCESSOR_ROLE;SWITCH_STATEMENT_LIST;FOR_IN_LOOP;FOR_EACH_LOOP;VARIABLE_DEF;
	IDENTIFIER_STAR;ANNOTATION;ANNOTATIONS;ANNOTATION_PARAMS;ARGUMENTS;ARGUMENT;PRE_DEC;PRE_INC;
	PROP_OR_IDENT;
	COMPILATION_UNIT;
	PACKAGE;
	IMPORT;
	METADATA;
	METADATA_ITEM;
	CLASS_DEF; INTERFACE_DEF;
	EXTENDS_CLAUSE; IMPLEMENTS_CLAUSE; TYPE_BLOCK;METHOD_BLOCK;
	MODIFIERS; VARIABLE_DEF; METHOD_DEF; NAMESPACE_DEF; PARAMS; PARAM; TYPE_SPEC; TYPE;
	BLOCK; EXPR; ELIST; EXPR_STMNT;EXPR_LIST;
	NEW_EXPR; ENCPS_EXPR;
	VAR_INIT;
	FUNCTION_CALL; ARRAY_ACC;
	UNARY_PLUS; UNARY_MINUS; POST_INC; POST_DEC;
	ARRAY_LITERAL; ELEMENT; OBJECT_LITERAL; OBJECT_FIELD; FUNC_DEF;
	FOR_INIT; FOR_CONDITION; FOR_ITERATOR;
	CLASS;INTERFACE;EXTENDS;IMPLEMENTS;
	METHOD;NAMESPACE;FOR_IN_CLAUSE;FOR_CLAUSE;FOR_LOOP;
	CASE;CASE_DEFAULT;SWITCH_BLOCK;SWITCH;BREAK;TRY_STATEMENT;THROW_STATEMENT;
	CONTINUE;CONTINUE_STATEMENT;BREAK_STATEMENT;SWITCH_STATEMENT;
	RETURN;BREAK;IF;ELSE;THROW;
	STATEMENT;WHILE;DO_WHILE;WHILE;WHILE_LOOP;DO_WHILE_LOOP;
	STATEMENT_BLOCK;PARAM_DECL;PARAM_REST_DECL;VAR_DEC;VARIABLE_DECLARATOR;
	PARAM_LIST;WITH;IDENTIFIER;DECL_STMT;
	MODIFIER_LIST;CLASS_MEMBER;
	REGEX; 
	XML; 
	NAMESPACE_USAGE;
	CONSTANT;LITERAL_NUMBER;LITERAL_STRING;LITERAL_DOUBLE_STRING;LITERAL_SINGLE_STRING;
	LITERAL_REGEX;LITERAL_XML;
	DEFAULT_XML_NAMESPACE;
    CONSTANT;
}


@parser::header {
            package emacs.flyparse.as3;
            import emacs.flyparse.*;
}

@lexer::header {
            package emacs.flyparse.as3;
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
                return type == ASSIGN || type == LPAREN || type == LBRACK || 
                    type == RETURN || type == COLON || type == LNOT || type == LT || 
                    type == GT || type == EQUAL || type == COMMA;
            }
}


@parser::members {
            private AS3Lexer lexer;
            private CharStream cs;
            
            public void setInput(AS3Lexer lexer, CharStream cs) {
                this.lexer = lexer;
                this.cs = cs;
            }

            // Used in tree rewrite rules to insert semicolon tree IF it exists..
            private FlyparseTree maybeSemi(ParserRuleReturnScope semi){
                return (semi.start.getType() == SEMI ? (FlyparseTree)semi.getTree() : null);
            }

}


/**
 * this is the start rule for this parser
 */
compilationUnit
	:	(	as2CompilationUnit
		|	as3CompilationUnit
		) -> ^(COMPILATION_UNIT as2CompilationUnit? as3CompilationUnit?)
	;

as2CompilationUnit
	:	importDefinition*
		as2Type
	;

as2Type
	:	
	(	as2IncludeDirective
	|	(modifiers CLASS) => as2ClassDefinition
	|	(modifiers INTERFACE) => as2InterfaceDefinition
	)
	;

as3CompilationUnit
	:	packageDecl
		packageBlockEntry*
		EOF!
	;

packageDecl
	:	PACKAGE identifierStar?
		LCURLY	
        packageBlockEntry*
		RCURLY
		-> ^(PACKAGE_DECL PACKAGE identifierStar? LCURLY packageBlockEntry* RCURLY)
	;

 packageBlockEntry options {k=2;}
	:	    importDefinition
		|   includeDirective
		|   useNamespaceDirective
		|   (LBRACK IDENT) => annotation
		|   (modifiers NAMESPACE) => namespaceDefinition
        |   (modifiers CLASS) => classDefinition
		|   (modifiers INTERFACE) => interfaceDefinition
		|   (modifiers FUNCTION) => methodDefinition
		|   (modifiers varOrConst) => variableDefinition
        |   statement
	;



endOfFile
	:	EOF!
	;

importDefinition
	:	IMPORT identifierStar s=semi
	        -> ^(IMPORT_DEF IMPORT identifierStar {maybeSemi(s)})
	;

semi 
	: SEMI
	; 

classDefinition
	:	modifiers
		CLASS 
        ident
		classExtendsClause?
		implementsClause?
		typeBlock
		-> ^(CLASS_DEF modifiers CLASS ^(CLASS_NAME ident) classExtendsClause? implementsClause? typeBlock)
	;

as2ClassDefinition
	:	modifiers
		CLASS identifier
		classExtendsClause?
		implementsClause?
		typeBlock
		-> ^(CLASS_DEF modifiers identifier classExtendsClause? implementsClause? typeBlock)
	;

interfaceDefinition
	:	modifiers
		INTERFACE ident
		interfaceExtendsClause?
		typeBlock
		-> ^(INTERFACE_DEF modifiers INTERFACE ident interfaceExtendsClause? typeBlock)
	;

as2InterfaceDefinition
	:	modifiers
		INTERFACE identifier
		interfaceExtendsClause?
		typeBlock
		-> ^(INTERFACE_DEF modifiers identifier interfaceExtendsClause? typeBlock)
	;

classExtendsClause
	:	EXTENDS identifier
        -> ^(EXTENDS_CLAUSE EXTENDS identifier)
	;

interfaceExtendsClause
	:	EXTENDS identifier ( COMMA identifier)*
        -> ^(EXTENDS_CLAUSE EXTENDS identifier+)
	;

implementsClause
	:	IMPLEMENTS identifier ( COMMA identifier)*
        -> ^(IMPLEMENTS_CLAUSE IMPLEMENTS identifier+)
	;

typeBlock
	:	LCURLY
        typeBlockEntry*
		RCURLY
		-> ^(TYPE_BLOCK LCURLY typeBlockEntry* RCURLY)
	;

typeBlockEntry options { k=2; }
	: 
      includeDirective
	| importDefinition
	| (LBRACK IDENT) => annotation
	| (modifiers varOrConst) =>  variableDefinition -> ^(CLASS_MEMBER variableDefinition)
	| (modifiers FUNCTION) => methodDefinition -> ^(CLASS_MEMBER methodDefinition)
	| statement
	;

as2IncludeDirective
	:	INCLUDE_DIRECTIVE
		stringLiteral
	;

includeDirective
	:	'include' stringLiteral s=semi
        -> ^(INCLUDE_DIRECTIVE 'include' stringLiteral {maybeSemi(s)})
	;


methodDefinition
	:
		modifiers
		FUNCTION
        accessorRole?
		methodName
		parameterDeclarationList
		typeExpression?
        maybeBlock
		-> ^(METHOD_DEF modifiers FUNCTION accessorRole? 
                methodName
				parameterDeclarationList
				typeExpression?
				maybeBlock
        )
	;

maybeBlock options {k=1;}
    : 
    (LCURLY) => block
    |   
    ;

methodName
    : ident
        -> ^(METHOD_NAME ident)
    ;


accessorRole
	:	GET | SET
	;

namespaceDefinition
	:	modifiers NAMESPACE namespaceName
		-> ^(NAMESPACE_DEF modifiers NAMESPACE namespaceName)
	;

useNamespaceDirective
	:	USE NAMESPACE namespaceName semi
	;

variableDefinition
	:	modifiers
		decl=varOrConst variableDeclarator
		(COMMA variableDeclarator)*
		s=semi
		-> ^(VARIABLE_DEF modifiers $decl variableDeclarator+ {maybeSemi(s)})
	;

declaration
	:	varOrConst variableDeclarator declarationTail
        -> ^(DECLARATION varOrConst variableDeclarator declarationTail)
	;

varOrConst
	:	VAR | CONST
	;

declarationTail
	:	(COMMA variableDeclarator)*
	;

variableInitializer
	:	ASSIGN expression
        -> ^(VAR_INITIALIZER ASSIGN expression)
	;

variableDeclarator
	:	ident typeExpression? variableInitializer?
        -> ^(VAR_DECLARATION ident typeExpression? variableInitializer?)
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
	:	basicParameterDeclaration | parameterRestDeclaration
	;

basicParameterDeclaration
	:	CONST? ident typeExpression? parameterDefault?
		-> ^(PARAM CONST? ident typeExpression? parameterDefault?)
	;

parameterDefault
		// TODO: can we be more strict about allowed values?
	:	ASSIGN assignmentExpression
	;

parameterRestDeclaration
	:	REST ident? typeExpression?
		-> ^(PARAM REST ident? typeExpression?)
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
	:	(LCURLY) => block

	|	declarationStatement 

	|	expressionStatement

	|	ifStatement

	|	forEachStatement

	|	forStatement

	|	whileStatement

	|	doWhileStatement
	
	|	withStatement
	
	|	switchStatement
	
	|	breakStatement

	|	continueStatement

	|	returnStatement

	|	throwStatement
	
	|	tryStatement
	
	|	defaultXMLNamespaceStatement
    
    |   semi
    
	;

declarationStatement
	:	declaration s=semi
      -> ^(DECL_STMT declaration {maybeSemi(s)})
	;

expressionStatement
	: expressionList s=semi
		-> ^(EXPR_STMNT expressionList {maybeSemi(s)})
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
	:	'throw' expression s=semi
        -> ^(THROW_STATEMENT 'throw' expression {maybeSemi(s)})
	;

tryStatement
	:	'try'
		block
		catchBlock*
		finallyBlock?
        -> ^(TRY_STATEMENT 'try' block catchBlock* finallyBlock?)
	;

catchBlock
	:	'catch' LPAREN ident typeExpression? RPAREN
		block
	;

finallyBlock
	:	'finally' block
	;

returnStatement
	:	RETURN expression? s=semi
        -> ^(RETURN_STATEMENT RETURN expression? {maybeSemi(s)})
	;
		
continueStatement
	:	CONTINUE s=semi
        -> ^(CONTINUE_STATEMENT CONTINUE {maybeSemi(s)})
	;

breakStatement
	:	BREAK s=semi
        -> ^(BREAK_STATEMENT BREAK {maybeSemi(s)})
	;

switchStatement
	:	SWITCH condition
		switchBlock
        -> ^(SWITCH_STATEMENT SWITCH condition switchBlock)
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

forEachStatement
	:	f=FOR EACH
		LPAREN
		forInClause
		RPAREN
		statement
		-> ^(FOR_EACH_LOOP $f forInClause statement)
	;

forStatement

	:	FOR
		LPAREN
		(	forInClause RPAREN statement
			-> ^(FOR_IN_LOOP FOR LPAREN forInClause RPAREN statement)

		|	traditionalForClause RPAREN statement
			-> ^(FOR_LOOP FOR LPAREN traditionalForClause RPAREN statement)
		)
	;

traditionalForClause
	:	forInit  semi	// initializer
		forCond  semi	// condition test
		forIter 		// updater
	;

forInClause
	:	forInClauseDecl IN forInClauseTail
	;

forInClauseDecl
	:	varOrConst ident typeExpression? 
        | ident
	;


forInClauseTail
	:	expressionList
	;

// The initializer for a for loop
forInit	
	:	(declaration | expressionList )?
		-> ^(FOR_INIT declaration? expressionList?)
	;

forCond
	:	expressionList?
		-> ^(FOR_CONDITION expressionList?)
	;

forIter
	:	expressionList?
		-> ^(FOR_ITERATOR expressionList?)
	;

whileStatement
	:	WHILE condition statement
		-> ^(WHILE_LOOP condition statement)
	;

doWhileStatement
	:	DO statement WHILE condition semi
		-> ^(DO_WHILE_LOOP DO statement WHILE condition )
	;

withStatement
	:	WITH condition statement
	;

defaultXMLNamespaceStatement
	:	DEFAULT XML NAMESPACE ASSIGN expression semi
		-> ^(DEFAULT_XML_NAMESPACE expression )
	;

typeExpression
	:	
		COLON (typeIdentifier | 'void' | STAR)
		-> ^(TYPE_SPEC COLON ^(TYPE typeIdentifier? 'void'? STAR?))
    ;

typeIdentifier
    :
    ident (propOrIdent)*
    ;

identifier 
	:	(qualifiedIdent -> qualifiedIdent) (propOrIdent -> propOrIdent )*
	;

qualifiedIdent 
options {k=1;}
    :    (namespaceName DBL_COLON) => namespaceName DBL_COLON ident
        -> ^(QUALIFIED_NAME namespaceName DBL_COLON ident)
        | ident
    ;

namespaceName
	:	IDENT | reservedNamespace
	;

reservedNamespace
	:	PUBLIC
	|	PRIVATE
	|	PROTECTED
	|	INTERNAL
	;

identifierStar
	:	ident
		dotIdent*
		(DOT STAR)?
		-> ^(IDENTIFIER_STAR ident dotIdent* DOT? STAR?)
	;

dotIdent
    : DOT ident 
    ;

ident
	:	IDENT -> ^(NAME IDENT)
	|	USE
	|	XML
	|	DYNAMIC
	|	NAMESPACE
	|	IS
	|	AS
	|	GET
	|	SET
	|	SUPER
	|	EACH
	;

annotation
	:	LBRACK
		ident
		annotationParamList?
		RBRACK
		-> ^(ANNOTATION ident annotationParamList?)
	;

annotationParamList
	:
		LPAREN
		(	annotationParam
			(COMMA annotationParam)*
		)?
		RPAREN
		-> ^(ANNOTATION_PARAMS annotationParam*)
	;

/*
[Inspectable(name="Icon Offset", verbose = 1,type=Boolean, defaultValue=true)]
[Inspectable(defaultValue="circular")]
[Bindable]
[ChangeEvent("event")]
[Inspectable("danger", 1, true, maybe)] 
[InspectableList("flavorStr","colorStr")]
[Event("click")]
[Collection (name="name", variable="varname", collectionClass="mx.utils.CollectionImpl", collectionItem="coll-item-classname", identifier="string")] 
*/

annotationParam
	:
		ident ASSIGN constant -> ^(ASSIGN ident constant)
	|	constant -> constant
	|	ident -> ident
	;


modifiers
	: ( modifier (modifier)* )?
	-> ^(MODIFIER_LIST modifier*)
	;

modifier
	:	namespaceName
	|	STATIC
	|	'final'
	|	'enumerable'
	|	'explicit'
	|	'override'
	|	DYNAMIC
	|	'intrinsic'
	;

arguments
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
	:	assignmentExpression (COMMA! assignmentExpression)*
	;

element
	:	assignmentExpression
		-> ^(ELEMENT assignmentExpression)
	;

// This is an initializer used to set up an object.
objectLiteral
	:	LCURLY fieldList? RCURLY
		-> ^(OBJECT_LITERAL fieldList?)
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
	;

// the mother of all expressions
expression
	:	assignmentExpression
        -> ^(EXPRESSION assignmentExpression)
	;

// This is a list of expressions.
expressionList
	:	assignmentExpression (COMMA assignmentExpression)*
        -> ^(EXPR_LIST assignmentExpression+)
	;

// assignment expression (level 13)
assignmentExpression
	:	conditionalExpression ((assignmentOperator) => assignmentOperator assignmentExpression )*
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
	:	assignmentExpression COLON assignmentExpression
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
		(logicalAndOperator bitwiseOrExpression)*
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
        (equalityOperator relationalExpression)*
	;

equalityOperator
	:	STRICT_EQUAL | STRICT_NOT_EQUAL | NOT_EQUAL | EQUAL
	;
	
// boolean relational expressions (level 5)
relationalExpression
	:	shiftExpression (relationalOperator shiftExpression)*
	;

relationalOperator
	:	 LT | GT | LE | GE | IS | AS | 'instanceof'
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
            
		|	//E4X expression
            DOT e4xExpression
            -> ^(E4X_EXPRESSION $postfixExpression e4xExpression)

		|	//Extended E4X expression
            E4X_DESC e4xExpression
            -> ^(E4X_EXPRESSION $postfixExpression E4X_DESC e4xExpression)
            
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

e4xExpression
	:	STAR
	|	e4xAttributeIdentifier
	|	e4xFilterPredicate
	;

e4xAttributeIdentifier
	:	E4X_ATTRI
		(	qualifiedIdent
		|	STAR
		|	LBRACK expression RBRACK
		)
	;

e4xFilterPredicate
	:	LPAREN!
		expression
		RPAREN!
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
	|	e4xAttributeIdentifier
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
		-> ^(PROP_OR_IDENT qualifiedIdent)
	;

constant
	:	xmlLiteral
	|	regexpLiteral
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


number	:	HEX_LITERAL
	|	DECIMAL_LITERAL
	|	OCTAL_LITERAL
	|	FLOAT_LITERAL
	;

	
xmlLiteral
	:	
          XML_LITERAL
		-> ^(LITERAL_XML XML_LITERAL)		      
	;


regexpLiteral
	:	REGEX_LITERAL
		-> ^(LITERAL_REGEX REGEX_LITERAL)
	;

newExpression
	:	NEW fullNewSubexpression arguments -> ^(NEW_EXPRESSION NEW fullNewSubexpression arguments)
	;

fullNewSubexpression
	:	(	primaryExpression -> primaryExpression
		)
		(	d=DOT qualifiedIdent -> ^(PROP_OR_IDENT $fullNewSubexpression qualifiedIdent)
		|	brackets -> ^(ARRAY_ACC $fullNewSubexpression brackets)
		)*
	;

propertyOperator
	:	DOT qualifiedIdent
	|	brackets
	;

brackets
	:	LBRACK expressionList RBRACK
	;

encapsulatedExpression
	:	LPAREN assignmentExpression RPAREN
		-> ^(ENCPS_EXPR assignmentExpression)
	;

// TODO: should anonymous and named functions have seperate definitions so that
// we can dissallow named functions in expressions?
functionDefinition
	:	FUNCTION parameterDeclarationList typeExpression? block
		-> ^(FUNC_DEF parameterDeclarationList typeExpression? block)
	;

PACKAGE		:	'package';
PUBLIC		:	'public';
PRIVATE		:	'private';
PROTECTED	:	'protected';
INTERNAL	:	'internal';
FUNCTION	:	'function';
EXTENDS		:	'extends';
IMPLEMENTS	:	'implements';
VAR		:	'var';
STATIC		:	'static';
IF		:	'if';
IMPORT		:	'import';
FOR		:	'for';
EACH		:	'each';
IN		:	'in';
WHILE		:	'while';
DO		:	'do';
SWITCH		:	'switch';
CASE		:	'case';
DEFAULT		:	'default';
ELSE		:	'else';
CONST		:	'const';
CLASS		:	'class';
INTERFACE	:	'interface';
TRUE		:	'true';
FALSE		:	'false';
DYNAMIC		:	'dynamic';
USE		:	'use';
XML		:	'xml';
NAMESPACE	:	'namespace';
IS		:	'is';
AS		:	'as';
GET		:	'get';
SET		:	'set';
WITH		:	'with';
RETURN		:	'return';
CONTINUE	:	'continue';
BREAK		:	'break';
NULL		:	'null';
NEW		    :	'new';
SUPER		:	'super';

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
EQUAL			:	'=='	;
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

DOT		:	'.'	;
E4X_DESC	:	'..'	;
REST		:	'...'	;

REGEX_LITERAL
	: { constantIsOk() }?=> '/' REGEX_BODY '/' REGEX_POSTFIX?
	;


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

DIV_ASSIGN		:	'/=';

DIV	            :	'/';


XML_LITERAL
	:	(XML_LITERAL) =>
      '<' IDENT (XML_WS | XML_ATTRIBUTE)*
		(	'>' (XML_SUBTREE | XML_TEXTNODE | XML_COMMENT | XML_CDATA | XML_BINDING)* 
            '</' IDENT '>'
		|	'/>'
		)
	;


fragment XML_SUBTREE
	:	'<' IDENT (XML_WS | XML_ATTRIBUTE)*
		(	'>' (XML_SUBTREE | XML_TEXTNODE | XML_COMMENT | XML_CDATA | XML_BINDING)*
			'</' IDENT '>'
		|	'/>'
		)
	;

fragment XML_ATTRIBUTE
	:	IDENT XML_WS* ASSIGN XML_WS* (STRING_LITERAL_DOUBLE | STRING_LITERAL_SINGLE | XML_BINDING)
	;

fragment XML_BINDING
	:	'{' XML_AS3_EXPRESSION '}'
	;

// it should be parsed as an AS3 expression...
fragment XML_AS3_EXPRESSION
	:	
		 (~('{'|'}'))*
	;

fragment XML_TEXTNODE
	:	(	
			XML_WS
		|	('/' ~'>') => '/'
		|	~('<'|'{'|'/'| XML_WS)
		)
	;

fragment XML_COMMENT
	:	'<!--'
		(
			XML_WS
		|	~('-'| XML_WS)
		|	('-' ~'-') => '-'
		)*
		'-->'
	;

fragment XML_CDATA
	:	'<![CDATA['
		(  XML_WS
        |  (']' ~']') => ']'
        |  ~(']'| XML_WS)
        )*
		']]>'
	;

fragment XML_WS
    :	' '
    |	'\t'
    |	'\f'
    |	'\r'
    |	'\n'
    ;



SL			:	'<<'	;
SL_ASSIGN	:	'<<='	;
LE			:	'<='	;
LT			:	'<'	;


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
	:	'\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
	;

fragment OCTAL_ESC
	:   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
	|   '\\' ('0'..'7') ('0'..'7')
	|   '\\' ('0'..'7')
	;

fragment UNICODE_ESC
	:   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
	;
