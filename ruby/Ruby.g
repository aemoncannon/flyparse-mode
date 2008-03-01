grammar Rubyv3;

options {
	output=AST;
	language=Java;
    ASTLabelType=FlyparseTree;
}

tokens {
	// 'imaginary' tokens
	STATEMENT_LIST;
	STATEMENT;
	MODIFIER_LINE;
	IF;
	RPAREN_IN_METHOD_DEFINATION;
	BODY;
	//CALL;
	ARG;
	
	LEFT_SHIFT;
	HEREDOC_STRING;
	FLOAT;
	
	CONSTANT;
	FID;
	VARIABLE;
	CALL;
	
	
	//COMPSTMT;
	SYMBOL;
	BLOCK;
	MULTIPLE_ASSIGN;
	MULTIPLE_ASSIGN_WITH_EXTRA_COMMA;
	BLOCK_ARG;
	BLOCK_ARG_WITH_EXTRA_COMMA;
	MRHS;
	NESTED_LHS;
	SINGLETON_METHOD;
	STRING;
	REST_UNUSE;
	
	DIV;
	MOD;
}

@rulecatch {
catch (RecognitionException e) {
throw e;
}
}
@lexer::rulecatch {
catch (RecognitionException e) {
throw e;
}
}

@header {
package com.xruby.compiler.parser;
}
@lexer::header {
package com.xruby.compiler.parser;

import com.xruby.compiler.codedom.*;

}
//@lexer::options {
//  superClass=BaseLexer;
//}
@members {
  private Rubyv3Parser parent = null;
  private SymbolTableManager stm = new SymbolTableManager(null);
  private Rubyv3Lexer lexer;
  private BaseTokenStream tokenStream;
  /*public boolean just_seen_var() {
          Token token = input.LT(1);
          if(token != null) {
            return stm.isDefinedInCurrentScope(token.getText());
          }
          return false;
  }*/
  public boolean isDefinedVar(String text) {
        boolean result = false;
        if(parent != null) {
          result = parent.isDefinedVar(text);
        } else {
          return stm.isDefinedInCurrentScope(text);
        }
        if(result) {
          return true;
        } else {
          return false;
        }
        
  }
  public void addVariable(String s) {
        if(parent != null) {
           parent.addVariable(s);
           return;
        }
        stm.addVariable(s);
  }
  
  protected void enterScope() {
        stm.enterScope();
  }

	//protected void enterBlockScope() {
	//	lexer_.getSymbolTableManager().enterBlockScope();
	//}
	
  protected void leaveScope() {
       stm.leaveScope();
  }
		
  public Rubyv3Parser(TokenStream input, Rubyv3Parser parent) {
            super(input);
            ((Rubyv3Lexer) input.getTokenSource()).setParser(this);
            this.parent = parent;
            this.lexer = (Rubyv3Lexer)input.getTokenSource();
            this.tokenStream = (BaseTokenStream)input;
  }
  /*public void init() {
    ((Rubyv3Lexer) input.getTokenSource()).setParser(this);
  }*/
  
}

@lexer::members {

    static final int STR_FUNC_ESCAPE=0x01;
    static final int STR_FUNC_EXPAND=0x02;
    static final int STR_FUNC_REGEXP=0x04;
    static final int STR_FUNC_QWORDS=0x08;
    static final int STR_FUNC_SYMBOL=0x10;
    static final int STR_FUNC_INDENT=0x20;

    private final int str_squote = 0;
    private final int str_dquote = STR_FUNC_EXPAND;
    private final int str_xquote = STR_FUNC_EXPAND;
    private final int str_regexp = STR_FUNC_REGEXP | STR_FUNC_ESCAPE | STR_FUNC_EXPAND;
    private final int str_ssym   = STR_FUNC_SYMBOL;
    private final int str_dsym   = STR_FUNC_SYMBOL | STR_FUNC_EXPAND;
    
    public int lex_state = 0;
    public static final int IN_HEREDOC_STRING = 1;
        /** Override this method to change where error messages go */
        public void emitErrorMessage(String msg) {
		System.err.println(msg);
	}
	private Rubyv3Parser parser;
	public void setParser(Rubyv3Parser parser) {
        this.parser = parser;
        }
        public Rubyv3Parser getParser() {
          return this.parser;
        }
        
        private Expression expression;
        public void reset() {
		super.reset(); // reset all recognizer state variables
		expression = null;
	}
        public Token emit() {
	    MyToken t =
	        new MyToken(input, type, channel,
	                    tokenStartCharIndex, getCharIndex()-1);
	    t.setLine(tokenStartLine);
	    t.setText(text);
	    t.setCharPositionInLine(tokenStartCharPositionInLine);
	    t.expression = expression;
	    expression = null; //clear out expression
	    emit(t);
	    return t;
       }
       private static final boolean isIdentifierChar(int c) {
        return Character.isLetterOrDigit(c) || c == '_';
       }
        public static int nesting = 0;

	private int determineBegin(int begin) {
        int result = 0; //if collide with EOF, then we can use other value like -3,-7 
        if (begin == '[' || begin == '{' || begin == '(' || begin == '<') {
            result = begin;
        } 
        return result;
        }
        private int determineEnd(int begin) {
                int end = 0;
                if(begin == '[') {
                    end = ']';
                } else if(begin == '{') {
                    end = '}';
                } else if(begin == '(') {
                    end = ')';
                } else if(begin == '<'){
                    end = '>';
                } else {
                    end = begin;
                }
                return end;
        }
        
        //hand written lexer matcher
        public Expression mHEREDOC_CONTENT(boolean indent) {
            String delimiter = null;int func = 0;StringBuffer tokens = new StringBuffer();
            expression = null;
            int c= input.LA(1);
                if ( input.LA(1)=='\"'||input.LA(1)=='\''||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||(input.LA(1)>='_' && input.LA(1)<='z') ) {
                    input.consume();

                }
                else {
                    return expression; //not match heredoc
                }


                        int end;
                        if (c == '\'' || c == '"' || c == '`') {
                            if (c == '\'') {
                                func |= str_squote;
                            } else if (c == '"') {
                                func |= str_dquote;
                            } else {
                                func |= str_xquote;
                            }


                            end = c;
                            while ((c = input.LT(1)) != EOF && c != end) {
                                tokens.append((char)c);
                                input.consume();
                            }
                            if (c == EOF) {
                                throw new SyntaxException("unterminated here document identifier");
                            }
                            input.consume(); //consume the end character.
                        } else {
                            /*if (!isIdentifierChar(c)) {
                                throw new SyntaxException("shouldn't happen");
                            }*/

                            func |= str_dquote;
                            tokens.append((char)c);
                            while(true) {
                            c = input.LT(1);
                            if(c == Token.EOF || !isIdentifierChar(c)) {
                                break;
                            }
                            tokens.append((char)c);
                            input.consume();
                            }

                         } //we already consume the last character.

                       expression = new HeredocParser(this.parser, input, func, tokens.toString(), indent).parseString();
                       return expression;
        }
}

program
                :	terminal* |statement_list terminal*
		;

statement_list
		:	statement (terminal+ statement)* -> ^(STATEMENT_LIST statement*)
			;

/*terminal
		:	SEMI!
		|	LINE_BREAK!
		;*/
statement
	:	expression (modifier_line)*  -> ^(STATEMENT expression (modifier_line)*)              
	;

modifier_line
	:(IF_MODIFIER|UNLESS_MODIFIER|WHILE_MODIFIER|UNTIL_MODIFIER|RESCUE_MODIFIER)^ (line_break)* expression// -> ^(MODIFIER_LINE $modifier expression) 
		;
IF_MODIFIER     :  'if';
UNLESS_MODIFIER :  'unless';
WHILE_MODIFIER  :  'while';
UNTIL_MODIFIER  :  'until';
RESCUE_MODIFIER :  'resuce';

SEMI	:';'
	;


//OMIT_LINE_BREAK
//	:	LINE_BREAK* {skip();}
//	;
//emptyable_expression
//	:	expression|;
	
block_expression
	:	'begin' body 'end';
body	:	SEMI* |statement_list terminal*;  //nearly same as program
boolean_expression
	:	'false'|'nil'|'true';
if_expression
	:	'if'  e0=expression seperator
	        body0=body ('elsif' e1=expression seperator body1+=body)*
	        ('else' body2=body)?
	        'end' -> ^(IF $e0 $body0 $e1* $body1* $body2? )
	        //'if_e'
	        ;
seperator
	:LINE_BREAK+|':' LINE_BREAK* | LINE_BREAK* 'then' LINE_BREAK*;	
unless_expression
	:	'unless' boolean_expression ('then'|':'|line_break)
	        body 
	        ('else' body)?
	        'end';
terminal:	SEMI!|line_break;
line_break 
	:	 LINE_BREAK!;
LINE_BREAK
	:'\r'? '\n'
	;	
WS	:	(' ' | '\t') { skip(); }
	;


/*
 * operator expression precedence
 * lowest->
 *		and or
 *		not
 *		? :
 *		= += -= *= /= %= &= ^= |= <<= >>= **=
 *		.. ...
 *		||
 *		&&
 *		<=> ==  === !=  =~  !~
 *		>  >=  <  <=
 *		|	 ^
 *		&
 *		<<  >>
 *		+  -
 *		*  /  %
 *		-(unary)  +(unary)  !  ~
 *		**
 *		[]
 *		::
 * highest<-
 */

expression
	:	 'alias'^ fitem fitem|andorExpression|primaryExpression;
primaryExpression
	:	methodDefinition;
methodDefinition
	:	'def'^ (LINE_BREAK!)* (singleton dot_or_colon)? methodName {enterScope();} f_arglist (terminal!)*  bodyStatement? (terminal!)* 'end'! {leaveScope();};
singleton
	:	variable|'('! expression opt_nl ')'!;
opt_nl        : /* none */ | LINE_BREAK!
    ;
dot_or_colon
	:	DOT|COLON2;
methodName
	:	ID|CONSTANT|FID;  //todo:or constant
f_arglist
	:	'(' f_args  (LINE_BREAK)* ')' -> ^(ARG f_args)
	|       f_args terminal!;
f_args	:	f_norm_args | f_norm_args ','! f_opt_args (','! f_rest_arg)?| /*none*/
	|	f_norm_args ','! f_rest_arg
	|       f_opt_args (','! f_rest_arg)?
	|	f_rest_arg
        ;
f_norm_args
	:       //CONSTANT{throw new SyntaxException("formal argument cannot be a constant");}
	//|       INSTANCE_VARIABLE {throw new SyntaxException("formal argument cannot be an instance variable");}
	//|       CLASS_VARIABLE {throw new SyntaxException("formal argument cannot be an class variable");}
	//|       ID;
	ID {addVariable($ID.text);};
f_rest_arg
	:	'*'^ ID{addVariable($ID.text);} | '*' -> ^('*' REST_UNUSE);
f_opt_args
	:	ID '='^ arg{addVariable($ID.text);};	
	
bodyStatement
	:	statement_list -> ^(BODY statement_list);
	

fitem	:	fname;// | symbol;
fname	:	ID|CONSTANT|FID|op;

/*op            : tPIPE | tCARET | tAMPER2 | tCMP | tEQ | tEQQ | tMATCH | tGT
              | tGEQ | tLT | tLEQ | tLSHFT | tRSHFT | tPLUS  | tMINUS | tSTAR2
              | tSTAR | tDIVIDE | tPERCENT | tPOW | tTILDE | tUPLUS | tUMINUS
              | tAREF | tASET | tBACK_REF2
*/

op            : '|' | '^' | '&' | COMPARE | EQUAL | CASE_EQUAL | MATCH | GREATER_THAN
              | GREATER_OR_EQUAL | LESS_THAN | LESS_OR_EQUAL | LEFT_SHIFT | RIGHT_SHIFT | PLUS  | MINUS | STAR
              | DIV | MOD | POWER | '~' /*is TILDE*/
              | '[]' | '[]=' ;
andorExpression
		:	notExpression (
				(	'and'^		(LINE_BREAK!)*
				|	'or'^		(LINE_BREAK!)*
				)
				notExpression
			)*
		;
notExpression
		:	'not'^
			(LINE_BREAK!)*
			notExpression
		|	definedExpression
		;
definedExpression
	:	'defined' assignmentExpression
	|       assignmentExpression;

	/*|	ID '(' ')'
	|	ID args;
args	:	pure_args_one_or_more | '(' pure_args_one_or_more ')';
pure_args_one_or_more
	:	expression (',' expression)*;*/
	
assignmentExpression
	:	ternaryIfThenElseExpression
	        |  lhs (ASSIGN|MOD_ASSIGN|COMPLEMENT_ASSIGN|DIV_ASSIGN|MINUS_ASSIGN|PLUS_ASSIGN|BOR_ASSIGN|BAND_ASSIGN|LEFT_SHIFT_ASSIGN|RIGHT_SHIFT_ASSIGN|STAR_ASSIGN|LOGICAL_AND_ASSIGN|LOGICAL_OR_ASSIGN|POWER_ASSIGN)^ 
	           assignmentExpression {addVariable($lhs.text);};

ternaryIfThenElseExpression
		:	r=rangeExpression ( QUESTION^ rangeExpression ':'! rangeExpression |)
		;
//= += -= *= /= %= **= &= ^= |= <<= >>= &&= ||=
//.. ...
rangeExpression
		:	logicalOrExpression
		       (
				(	INCLUSIVE_RANGE^	(LINE_BREAK!)*
				|	EXCLUSIVE_RANGE^	(LINE_BREAK!)*
				)
				logicalOrExpression
			)*
		;

//||
logicalOrExpression
		:	logicalAndExpression
                        (
				LOGICAL_OR^		(LINE_BREAK!)*
				logicalAndExpression
			)*
		;

//&&
logicalAndExpression
		:	equalityExpression
                        (
				LOGICAL_AND^		(LINE_BREAK!)*
				equalityExpression
			)*
		;

//<=> ==  === !=  =~  !~
equalityExpression
		:	relationalExpression
                        (
				(	COMPARE^		(LINE_BREAK!)*
				|	EQUAL^			(LINE_BREAK!)*
				|	CASE_EQUAL^	(LINE_BREAK!)*
				|	NOT_EQUAL^		(LINE_BREAK!)*
				|	MATCH^			(LINE_BREAK!)*
				|	NOT_MATCH^		(LINE_BREAK!)*
				)
				relationalExpression
			)*
		;


//>  >=  <  <=
relationalExpression
		:	orExpression
                        (
				(	LESS_THAN^			(LINE_BREAK!)*
				|	GREATER_THAN^		(LINE_BREAK!)*
				|	LESS_OR_EQUAL^		(LINE_BREAK!)*
				|	GREATER_OR_EQUAL^	(LINE_BREAK!)*
				)
				orExpression
			)*
		;

//|  ^
orExpression
		:	andExpression
                        (
				(	BXOR^			(LINE_BREAK!)*
				|	BOR^			(LINE_BREAK!)*
				)
				andExpression
			)*
		;

//&
andExpression
		:	shiftExpression
                        (
				BAND^			(LINE_BREAK!)*
				shiftExpression
			)*
		;



//<<  >>
shiftExpression
		:	additiveExpression
                        (
				(	LEFT_SHIFT^		(LINE_BREAK!)*
				|	RIGHT_SHIFT^	(LINE_BREAK!)*
				)
				additiveExpression
			)*
		;



//+  -
additiveExpression
		:	multiplicativeExpression
                        (
				(	PLUS^				(LINE_BREAK!)*
				|	MINUS^				(LINE_BREAK!)*
				)
				multiplicativeExpression
			)*
		;

//*  /  %
multiplicativeExpression
		:	powerExpression
                        (
				(	STAR^			(LINE_BREAK!)*
				|	DIV^			(LINE_BREAK!)*
				|	MOD^			(LINE_BREAK!)*
				)
				powerExpression
			)*
		;


//**
powerExpression
		:	bnotExpression
                        (			
				POWER^			(LINE_BREAK!)*
				bnotExpression
			)*
		;

//!  ~
bnotExpression
		:	(	BNOT^			(LINE_BREAK!)*
			|	NOT^			(LINE_BREAK!)*
			)*
			command
		;
command
@after{System.out.println("add virtual Token EXPR_END");tokenStream.addVirtualToken($command.stop.getTokenIndex(), VirtualToken.EXPR_END);}
	:('expression0' | 'expression1' |literal|boolean_expression| block_expression|if_expression|unless_expression|atom[true] | '(' expression ')' ) (DOT^ method[false])*
	; //|       lhs SHIFT^ rhs ;	
atom[boolean topLevel]	:	methodExpression[topLevel];
methodExpression[boolean topLevel]
	:      variable|method[topLevel];
variable:	{isDefinedVar(input.LT(1).getText())}? ID -> ^(VARIABLE ID);
method[boolean topLevel]	:	{!isDefinedVar(input.LT(1).getText())}? ID -> ^(CALL ID)
        |       ID open_args -> ^(CALL ID open_args)
        ;
/*primary	:	literal;
	
command_call
	:	command1;
	//|       'return'^ call_args
	//|       'break'^ call_args
	//|       'next'^ call_args;
command1:	operation1 (command_args);
		
command_args
	:	open_args;*/
/* mandatory open_args*/
open_args options {backtrack=true;}
	:	'('! call_args ')'!
	|	'('! ')'!
	|	call_args 
        ;  //silence warnings :http://www.antlr.org:8888/browse/ANTLR-139

//paren_args
//	:	paren_args1
//	'('! arg ','! arg (','! arg)* /*call_args2*/ ')'!
//	;
//paren_args1
//	:	'(' arg ')';

call_args
	:	args  -> ^(ARG args);
	
args	:	arg (','! arg)*;
	
arg	:	definedExpression;
//call_args2
//	:	command1 | args;

operation1     : ID | CONSTANT | FID
    ;

operation2    : ID | CONSTANT | FID | op
    ;

operation3    : ID | FID | op
    ;
	
lhs	:	ID -> ^(VARIABLE ID); //todo: see this
rhs	:	expression;

//primary	:	literal| 'begin' program 'end'; //todo:more on this later

literal	:	INT|FLOAT|string|ARRAY|SYMBOL|REGEX;
INT
	:	'-'?
	        (OCTAL|HEX|BINARY|LEADING_MARK_DECIMAL
	        | ('0'|'1'..'9' ('_'? '0'..'9')* ) (/*none*/ | (EXP_PART|{if(input.LA(2) < '0' || input.LA(2) > '9') {$type=INT; this.type = INT; return;}} '.' LEADING0_NUMBER EXP_PART?) {$type = FLOAT;}) 
	        )//|ESCAPE_INT)
	;

ID	:	('a'..'z' | 'A'..'Z'{$type = CONSTANT;} | '_') (('a'..'z' | 'A'..'Z') | ('0'..'9'))*
	;
FID	:	ID ('?' | '!');
INSTANCE_VARIABLE
	:	'@' IDENTIFIER_CONSTANT_AND_KEYWORD;
CLASS_VARIABLE
	:	'@' INSTANCE_VARIABLE;

GLOBAL_VARIABLE 
		:	'$'	('-')?	IDENTIFIER_CONSTANT_AND_KEYWORD
		|	'$'	(options{greedy=true;}:'0'..'9')+
		|	'$'	('!'|'@'|'&'|'`'|'\''|'+'|'~'|'='|'/'|'\\'|','|';'|'.'|'<'|'>'|'*'|'$'|'?'|/*':'|*/'\"')
		;
fragment
IDENTIFIER_CONSTANT_AND_KEYWORD
		:	('a'..'z'|'A'..'Z'|'_')	('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
		;

//fragment
//FLOAT_WITH_DIRECT_EXPPART
//	: ( NON_LEADING0_NUMBER | '0') EXP_PART	; //{$type = FLOAT;};
//fragment
//FLOAT	:	'-'?( NON_LEADING0_NUMBER | '0') ('.' LEADING0_NUMBER EXP_PART?);
fragment
OCTAL	:	'0' '_'? ('0'..'7') ('_'? '0'..'7')*;
fragment
LEADING_MARK_DECIMAL	:	
('0d') ('0'..'9') ('_'? '0'..'9')* ;
fragment
HEX	: '0x' HEX_PART ('_'? HEX_PART)* ;
fragment
HEX_PART
	:	('0'..'9'|'a'..'f'|'A'..'F')
	;

fragment
BINARY	:	'0b'('0'..'1') ('_'? '0'..'1')*;

fragment
ESCAPE_INT
	:       /*QUESTION*/ (CONTROL_PART|META_PART)* ('\u0000' .. '\u0091' | '\u0093'..'\u0255' | ESCAPE_INT_PART)
	;
fragment
CONTROL_PART
	:	('\\C-'|'\\c')
	;
fragment
META_PART
	:	'\\M-'
	;

fragment
NON_LEADING0_NUMBER
	:('1'..'9') ('_'? '0'..'9')*;	
fragment
LEADING0_NUMBER
	:('0'..'9') ('_'? '0'..'9')*;
fragment	
EXP_PART:	('e' | 'E') '-'? LEADING0_NUMBER;

string	:	SINGLE_QUOTE_STRING|DOUBLE_QUOTE_STRING|HEREDOC_STRING;

SINGLE_QUOTE_STRING
	@init{int end=0; int nested=0;}:	'\'' SINGLE_STRING_CHAR* '\'' 
	| '%q' begin=. {System.out.println($begin); end=determineEnd($begin);begin=determineBegin($begin); } (tmp=.{System.out.println(tmp);
	                    if(tmp == EOF) {
	                      throw new SyntaxException("unterminated string meets end of file");
	                    } else if(tmp == '\\') {
	                      int c = input.LA(1);
	                      if(c == EOF) {
	                         throw new SyntaxException("unterminated string meets end of file");
	                      } else if (c == begin || c == end || c == '\\') {
	                         //tokens.add();
	                         input.consume();
	                      }
	                    }else if(tmp==begin) {
                                nested ++;
                            } else if(tmp==end)  {
                                
                                if(nested == 0) {
                                this.type=SINGLE_QUOTE_STRING;
                                return;
                                }
                                nested --;
                            }
                            })*;
fragment	
SINGLE_STRING_CHAR
  	:	'\\' . | ~ ('\\'|'\'') ;
fragment
DOUBLE_STRING_CHAR
	:	'\\' . | ~ ('\\'|'"');
DOUBLE_QUOTE_STRING
	@init{int end=0; int nested=0;}:	s=('"' {expression = new DoubleStringParser(this.parser, input, '"', 0).parseString();}  | '%Q' begin=. {end=determineEnd($begin);begin=determineBegin($begin); 
	expression = new DoubleStringParser(this.parser, input, end, begin).parseString(); } 
	); //{expression = new DoubleQuoteStringExpression(input.substring(tokenStartCharIndex, getCharIndex() - 1));}; //todo: is this some ref like $s.text here?
                            
LCURLY  : '{' {nesting++; System.out.println("meeting LCURLY with nesting:" + nesting);}
        ;
/** If we have a '}' with nesting level 0 then it must match the '{'
 *  (unseen by this grammar) that started an embedded Simple statement
 *  block within a javadoc comment.
 */
RCURLY  : '}'
          {
          if ( nesting<=0 ) {
                token=Token.EOF_TOKEN;
                System.out.println("exiting embedded simple with nesting:" + nesting);
          }
          else {
                nesting--;
          }
          }
        ;
fragment
ESCAPE_INT_PART //ESCAPE_INT_PART in ESCAPE_INT
	:	'\\' ('0'..'7' | '0'..'7' '0'..'7' | '0'..'7' '0'..'7' '0'..'7')
	|       '\\' 'x' (HEX_PART|HEX_PART HEX_PART) //validating semantic predicate seems does not work, just use enum directly, this is ugly.
	|       '\\' ~('0'..'7'|'x'|'c'|'M'|'C')
	;
                            
HEREDOC_BEGIN
	:	'<<'{if(Character.isWhitespace(input.LT(1))) {$type = LEFT_SHIFT;}};  //mofidy type to SHIFT in BaseTokenStream if previous token is var	

HEREDOC_INDENT_BEGIN
	:	'<<-'{if(Character.isWhitespace(input.LT(1))) {$type = LEFT_SHIFT;}}; 
//SHFIT   //set in HEREDOC_BEGIN
//	: '<<';

      
ARRAY	:	'[]';
hash	:	'{'^ assoc_list '}'!;
assoc_list
	:	assocs trailer /*| args trailer*/;
assocs	:	assoc ( ','! assoc)*;

assoc         : arg (ASSOC|',')! arg;



trailer!       : /* none */ | LINE_BREAK! | ','!;

REGEX	:	'/abc/';
SYMBOL	:	':abc';

ASSIGN                  :	'=';
PLUS_ASSIGN			:	'+='	;
MINUS_ASSIGN		:	'-='		;
STAR_ASSIGN			:	'*='		;
DIV_ASSIGN		:	'/='		;
MOD_ASSIGN		:	'%='	;
COMPLEMENT_ASSIGN   :   '~=';
POWER_ASSIGN		:	'**='	;
BAND_ASSIGN			:	'&='		;
BXOR_ASSIGN			:	'^='		;
BOR_ASSIGN			:	'|='		;
LEFT_SHIFT_ASSIGN	:	'<<='	;
RIGHT_SHIFT_ASSIGN	:	'>>='	;
LOGICAL_AND_ASSIGN	:	'&&='	;
LOGICAL_OR_ASSIGN	:	'||='	;
INCLUSIVE_RANGE     :	'..';
EXCLUSIVE_RANGE     :	'...';

ASSOC				:	'=>'		;
LOGICAL_AND			:	'&&'		;
LOGICAL_OR			:	'||'		;

QUESTION			:	'?'		;
LPAREN				:	'('     ;
RPAREN				:	')'		;
LBRACK				:	'['		;
RBRACK				:	']'		;
EMPTY_ARRAY		:	'[]'		;
//LCURLY_HASH			:	'{'		;
//RCURLY				:	'}'		;
COMMA				:	','		;
COLON				:	':'		;
DOT	:	'.';
COLON2				:	'::'	;

NOT					:	'!'		;
BNOT				:	'~'		;
DIV				:	'/'		;
PLUS				:	'+'		;
MINUS				:	'-'		;
MOD				:	'%'		;
STAR				:	'*'		;	//'f * g' can parsed as 'f(*g)' or '(f) * (g)'
LESS_THAN			:	'<'		;
GREATER_THAN		:	'>'		;
BXOR				:	'^'		;
BOR					:	'|'		;
BAND				:	'&'		;
POWER				:	'**'		;
COMPARE				:	'<=>'	;
GREATER_OR_EQUAL	:	'>='		;
LESS_OR_EQUAL		:	'<='		;
EQUAL				:	'=='		;
CASE_EQUAL			:	'==='	;
NOT_EQUAL			:	'!='		;
MATCH				:	'=~'		;
NOT_MATCH			:	'!~'		;
//LEFT_SHIFT		:	'<<'		;
RIGHT_SHIFT			:	'>>'		;

COMMENT
		:	'#'	ANYTHING_OTHER_THAN_LINE_FEED LINE_BREAK
			{
				skip();
			}
		;
fragment
ANYTHING_OTHER_THAN_LINE_FEED
		:	(~('\r'|'\n'))*
		;
		
//		ML_COMMENT
//: '/*' ( options {greedy=false;} : . )* '*/'
//;
