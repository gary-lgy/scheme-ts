grammar Scheme;

program
   : sequence EOF
   ;

sequence
   : expression*
   ;

expression
   : '(' elements=expression* ')' # List
   | '\'' expression # Quote
   | '`' expression # Quasiquote
   | ',' expression # Unquote
   | ',@' expression # UnquoteSplicing
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

IDENTIFIER
   : IDENTIFIER_INITIAL IDENTIFIER_SUBSEQUENT*
   | PECULIAR_IDENTIFIER
   ;

WHITESPACE
   : (' ' | '\n' | '\t' | '\r')+ -> skip
   ;

COMMENT
   : ';' ~ ( '\r' | '\n' )* -> skip
   ;

fragment LETTER
   : ('a' .. 'z')
;

fragment DIGIT
   : ('0' .. '9')
   ;

fragment IDENTIFIER_INITIAL
   : LETTER
   | '$'
   | '%'
   | '&'
   | '*'
   | '/'
   | ':'
   | '<'
   | '='
   | '>'
   | '?'
   | '^'
   | '_'
   ;

fragment IDENTIFIER_SUBSEQUENT
   : IDENTIFIER_INITIAL
   | DIGIT
   | '+'
   | '-'
   | '.'
   | '@'
   | '!'
;

fragment PECULIAR_IDENTIFIER
   : '+'
   | '-'
   | '...'
   ;
