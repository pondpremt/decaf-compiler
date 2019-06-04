header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

{@SuppressWarnings("unchecked")}
class DecafScanner extends Lexer;
options
{
  k = 2;
}

tokens 
{
  "boolean";
  "break";
  "callout";
  "continue";
  "else";
  "false";
  "for";
  "while";
  "if";
  "int";
  "return";
  "len";
  "true";
  "void";
}

// Selectively turns on debug tracing mode.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws CharStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws CharStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}

SEMICOLON options { paraphrase = ";"; } : ';';
LSQUARE   options { paraphrase = "["; } : '[';
RSQUARE   options { paraphrase = "]"; } : ']';
LPAREN    options { paraphrase = "("; } : '(';
RPAREN    options { paraphrase = ")"; } : ')';
LCURLY    options { paraphrase = "{"; } : '{';
RCURLY    options { paraphrase = "}"; } : '}';
COMMA     options { paraphrase = ","; } : ',';
EQ        options { paraphrase = "="; } : '=';
PLUS      options { paraphrase = "+"; } : '+';
MINUS     options { paraphrase = "-"; } : '-';
TIMES     options { paraphrase = "*"; } : '*';
DIV       options { paraphrase = "/"; } : '/';
MOD       options { paraphrase = "%"; } : '%';
LT        options { paraphrase = "<"; } : '<';
GT        options { paraphrase = ">"; } : '>';
BANG      options { paraphrase = "!"; } : '!';
QUES      options { paraphrase = "?"; } : '?';
AT        options { paraphrase = "@"; } : '@';
COLON     options { paraphrase = ":"; } : ':';
PLUSEQ    options { paraphrase = "+="; } : "+=";
MINUSEQ   options { paraphrase = "-="; } : "-=";
LTE       options { paraphrase = "<="; } : "<=";
GTE       options { paraphrase = ">="; } : ">=";
EQQ       options { paraphrase = "=="; } : "==";
NEQ       options { paraphrase = "!="; } : "!=";
AND       options { paraphrase = "&&"; } : "&&";
OR        options { paraphrase = "||"; } : "||";

ID : ALPHA (ALPHANUM)*;
protected ALPHANUM : ALPHA | DIGIT;
protected ALPHA : ('a'..'z') | ('A'..'Z') | ('_');
protected DIGIT : ('0'..'9');
protected HEX_DIGIT : DIGIT | ('a'..'f') | ('A'..'F');
INT_LITERAL : DECIMAL_LITERAL | HEX_LITERAL;
protected DECIMAL_LITERAL : (DIGIT)+;
protected HEX_LITERAL : "0x" (HEX_DIGIT)+;
CHAR_LITERAL : '\'' (CHAR) '\'';
STRING_LITERAL : '"' (CHAR)* '"';


// Note that here, the {} syntax allows you to literally command the lexer
// to skip mark this token as skipped, or to advance to the next line
// by directly adding Java commands.
WS_ : (' ' | '\t' | '\n' {newline();}) {_ttype = Token.SKIP; };
SL_COMMENT : "//" (~'\n')* '\n' {_ttype = Token.SKIP; newline (); };

protected ESC :  '\\' ('n'|'t'|'\\'|'\"'|'\'');
protected CHAR : (' '|'!'|('#'..'&')|('('..'[')|(']'..'~')|ESC);
