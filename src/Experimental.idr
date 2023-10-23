
module Experimental

--import Data.String.Parser
--import Data.String.Parser.Combinators
import Text.Parser
import Text.Lexer

symbolToken : Parser MathToken
symbolToken = Symbol <$> oneOf ['+', '-', '*', '/', '(', ')', '^', '[', ']', ',']


spaceToken : Parser MathToken
spaceToken = do
    some space
    pure Space


var2Token : Parser MathExpr
var2Token = Var2 <$> some (notChar '"') <* char '"' -- Assuming the variable is enclosed in quotes.


lit3Token : Parser MathExpr
lit3Token = Lit3 <$> double


term : Parser MathExpr
term = lit3Token
   <|> var2Token
   <|> parens sumToken -- Handle parenthesis


powToken : Parser MathExpr
powToken = chainl1 term (char '^' *> pure Pow2)

divToken : Parser MathExpr
divToken = chainl1 powToken (char '/' *> pure Div2)

mulToken : Parser MathExpr
mulToken = chainl1 divToken (char '*' *> pure Mul2)

subToken : Parser MathExpr
subToken = chainl1 mulToken (char '-' *> pure Sub2)

sumToken : Parser MathExpr
sumToken = chainl1 subToken (char '+' *> pure Add2)


vectorToken : Parser MathExpr
vectorToken = Vector2 <$> between (char '[') (char ']') (sepBy sumToken (char ','))


summationToken : Parser MathExpr
summationToken = do
    _ <- string "summation("
    e1 <- sumToken
    _ <- char ','
    e2 <- sumToken
    _ <- char ','
    e3 <- sumToken
    _ <- char ')'
    pure $ Summation2 e1 e2 e3


mathExprParser : Parser MathExpr
mathExprParser = choice [vectorToken, summationToken, sumToken] <* eof

parseExpr : String -> Either String MathExpr
parseExpr = parse mathExprParser
