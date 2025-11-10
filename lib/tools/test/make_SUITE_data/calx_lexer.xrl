%% From: https://github.com/knutin/calx
Definitions.

Operator   = [+\-]
Number     = [0-9]+
Whitespace = [\000-\s]
LeftParen  = \(
RightParen = \)

Rules.

{Whitespace} : skip_token.
{Operator}   : {token, {operator, TokenLine, TokenChars}}.
{LeftParen}  : {token, {'(', TokenLine, TokenChars}}.
{RightParen} : {token, {')', TokenLine, TokenChars}}.
{Number}     : {token, {digit, TokenLine, TokenChars}}.


Erlang code.
