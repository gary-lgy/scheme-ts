grammar Scheme;

program
   : sequence EOF
   ;

sequence
   : expression*
   ;

expression
   : '(' elements=expression* ')' # List
   | STRING # String
   | NUMBER # Number
   | BOOL # Bool
   | IDENTIFIER # Identifier
   ;

STRING
   : '"' ('\\' . | ~ ('\\' | '"'))* '"'
   ;

NUMBER
   : ('+' | '-')? (DIGIT)+ ('.' (DIGIT)+)?
   ;

BOOL
   : '#t'
   | '#f'
   ;

WHITESPACE
   : (' ' | '\n' | '\t' | '\r')+ -> skip
   ;

IDENTIFIER
   : IDENTIFIER_INITIAL IDENTIFIER_SUBSEQUENT*
   ;

fragment LETTER
   : ('a' .. 'z')
   | ('A' .. 'Z')
;

fragment DIGIT
   : ('0' .. '9')
   ;

fragment IDENTIFIER_INITIAL
   : LETTER
   | '+'
   | '-'
   | '*'
   | '/'
   ;

fragment IDENTIFIER_SUBSEQUENT
   : IDENTIFIER_INITIAL
   | DIGIT
;
