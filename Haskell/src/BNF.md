<document> ::= <modifiers>* <EOF>

<modifiers> ::= <whitespace>* <header> <whitespace>*
            | <whitespace>* <blockquote> <whitespace>*
            | <ordered_list>
            | <unordered_list>
            | <whitespace>* <code_block> 
            | <whitespace>* <table> <whitespace>*
            | <image>
            | <footnote_reference>
            | <free_text>
            | <newline>

<header> ::= <atx_header>
           | <setext_header>

<atx_header> ::= <hashes> <whitespace>+ <text> <newline>

<setext_header> ::= <text> <newline> <underlineHeader> <newline>

<hashes> ::= "#" | "##" | "###" | "####" | "#####" | "######"

<underlineHeader> ::=  "==" ("=")*
                    |  "--" ("-")* 

<blockquote> ::= ">" <whitespace>* <text> (<newline> <blockquote>)*

<ordered_list> ::= <ordered_list_item_1> (<newline> <ordered_list_item>)*
<ordered_list_item_1> ::= "1" "." <whitespace>+ <text> (<ordered_sublist>)?
<ordered_list_item> ::= <number> "." <whitespace>+ <text> (<ordered_sublist>)?
<ordered_sublist> ::= <newline> <indentation> <ordered_list_item_1> (<newline> <indentation> <ordered_list_item>)*


<unordered_list> ::= <unordered_list_item> (<newline> <unordered_list_item>)*
<unordered_list_item> ::= "-" <whitespace>+ <text> (<unordered_sublist>)?
<unordered_sublist> ::= <newline> <indentation> <unordered_list_item> (<newline> <indentation> <unordered_list_item>)*
<indentation> ::= "    " <indentation>?  /* Allows nested sublists with increasing indentation */


<number> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" <digit>* 

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" 
           | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" 
           | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" 
           | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"

<special_char> ::= "!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" 
                 | "," | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" 
                 | "[" | "\\" | "]" | "^" | "_" | "`" | "{" | "|" | "}" | "~"

<character> ::= <letter> | <digit> | <special_char> | <whitespace>

<whitespace> ::= "\t" | "\r" | "\f" | "\v" | " "

<newline> ::= "\n"

<non_whitespace_char> ::= <letter> | <digit> | <special_char>

<code_content> ::= (<character> | <newline>)* 

<language> ::= <letter>+

<code_block> ::= "```" <whitespace>* <language>? <whitespace>* <newline> <code_content> <newline> "```" <newline>

/* Context-Free */
<table> ::= <table_header> <newline> <table_separator> <newline> <table_row>* 
<table_cell> ::= <whitespace>* <text> <whitespace>*
<table_header> ::= <whitespace>* "|" <table_cell> ("|" <table_cell>)* "|" <whitespace>*
<table_separator> ::= <whitespace>* "|" "---" ("-")* ("|" "---" ("-")*)* "|" <whitespace>*
<table_row> ::= <whitespace>* "|" <table_cell> ("|" <table_cell>)* "|" <whitespace>*



<image> ::= "!" "[" <text> "]" <whitespace>* "(" <url> <whitespace>+ <caption> ")"
<url> ::= <non_whitespace_char>+
<caption> ::= "\"" <text>* "\""


<footnote_reference> ::= "[" "^" <number> "]" ":" <whitespace>* <text> <newline>


<footnote> ::= "[" "^" <number> "]"

<free_text> ::= <text> <newline>?

<text> ::= (<bold> | <italic> | <strikeThrough> | <link> | <inline_code> | <plain_text>)+

/* Text Modifiers */
<bold> ::= "**" <text_content> "**"
<italic> ::= "_" <text_content> "_"
<strikeThrough> ::= "~~" <text_content> "~~"
<link> ::= "[" <text_content> "]" <whitespace>* "(" <url> ")"
<inline_code> ::= "`" <text_content> "`"

<plain_text> ::= <character>*
<text_content> ::= <plain_text> | <text_modifier>

<text_modifier> ::= <bold> | <italic> | <strikeThrough> | <inline_code> | <link> | <footnote>

