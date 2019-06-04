header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

class DecafParser extends Parser;
options
{
  importVocab = DecafScanner;
  k = 3;
  buildAST = true;
}

// Java glue code that makes error reporting easier.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  // Do our own reporting of errors so the parser can return a non-zero status
  // if any errors are detected.
  /** Reports if any errors were reported during parse. */
  private boolean error;

  @Override
  public void reportError (RecognitionException ex) {
    // Print the error via some kind of error reporting mechanism.
    error = true;
  }
  @Override
  public void reportError (String s) {
    // Print the error via some kind of error reporting mechanism.
    error = true;
  }
  public boolean getError () {
    return error;
  }

  // Selectively turns on debug mode.

  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws TokenStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws TokenStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}


program      : (callout_decl)* (field_decl)* (method_decl)* EOF;

callout_decl : TK_callout ID SEMICOLON;

field_decl   : type field_id (COMMA field_id)* SEMICOLON;
protected field_id : ID | ID LSQUARE INT_LITERAL RSQUARE;

method_decl  : (type | TK_void) ID LPAREN (method_param (COMMA method_param)*)? RPAREN block;
protected method_param : type ID;

block        : LCURLY (field_decl)* (statement)* RCURLY;

type         : TK_int | TK_boolean;

statement    : location assign_op expr SEMICOLON
             | method_call SEMICOLON
             | TK_if LPAREN expr RPAREN block (TK_else block)?
             | TK_for LPAREN ID EQ expr COMMA expr (COMMA INT_LITERAL)? RPAREN block
             | TK_while LPAREN expr RPAREN block
             | TK_return (expr)? SEMICOLON
             | TK_break SEMICOLON
             | TK_continue SEMICOLON;

assign_op    : EQ | PLUSEQ | MINUSEQ;

method_call  : method_name LPAREN (method_arg (COMMA method_arg)*)? RPAREN;
method_name  : ID;
method_arg   : expr | STRING_LITERAL;

location     : ID | ID LSQUARE expr RSQUARE;

expr         : expr_ternary;
expr_ternary : expr_or (QUES expr_ternary COLON expr_or)*;
expr_or      : expr_and (OR expr_and)*;
expr_and     : expr_eq (AND expr_eq)*;
expr_eq      : expr_rel (eq_op expr_rel)*;
expr_rel     : expr_arith1 (rel_op expr_arith1)*;
expr_arith1  : expr_arith2 (arith1_op expr_arith2)*;
expr_arith2  : expr_unop (arith2_op expr_unop)*;
expr_unop    : (BANG | MINUS)* expr_atom;
expr_atom    : LPAREN expr RPAREN
             | location | method_call | literal | AT ID;

protected literal   : INT_LITERAL | CHAR_LITERAL | TK_true | TK_false;
protected arith1_op : PLUS | MINUS;
protected arith2_op : TIMES | DIV | MOD;
protected rel_op    : LT | GT | LTE | GTE;
protected eq_op     : EQQ | NEQ;
