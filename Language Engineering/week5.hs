import Yoda

--1.1

--(<*) :: Applicative f => f a -> f b -> f a
--p <* q = const <$> p <*> q

--some = (:) <$> p <*> many p

--(a) Example : parseMaybe (string "hello" <* eof) "hello"
--(a.5) Example : parseMaybe (string "hellodan") "hellodan"
--(b) Example : parseMaybe (many(string ("is")) "isisisisis") --return list of "is" * 5 (0 or more)

--(c) Example : parseMaybe (some(string ("is") term) --only if once instance at least
--(d) Example : parseMaybe (oneOf(string "hello") "h")
--(e) Example : parseMaybe (noneOf(string "hello") "w")
--(f) Example : parseMaybe (try (string "hello") <|> (string "help") "help")

whitespace :: Parser ()
whitespace = many (oneOf " \t\n\r") *> pure ()

-- USE string which matches chars to chars baby... ohhhh yeah B-)

tok :: String -> Parser String
tok
 ts = whitespace *> (string ts) <* whitespace

--whitespace :: Parser ()
--string ts :: Parser String

number :: Parser Int
number = read <$> some (oneOf "0123456789")

-- data Robot = Move Int Robot
--            | Right Robot
--            | Left Robot
--            | Stop
--            deriving(Show, Eq)

data Robot = Move Int Robot
           | Rright Robot
           | Lleft Robot
           | Stop
           deriving(Show, Eq)

str :: String
str = "forward 4 rotate right rotate left forward 20 stop"

str1 :: String
str1 = "forward 4"

-- parseRobotForward :: Parser Robot
-- parseRobotForward = Move <$ tok "forward" <*> number

--Parser (Robot -> Robot)
-- Parser (Robot)
-- = Parser (Robot)

-- parseRobotRight :: Parser Robot
-- parseRobotRight = Rright <$ (tok "rotate" *> tok "right")
--
-- parseRobotLeft :: Parser Robot
-- parseRobotLeft = Lleft <$ (tok "rotate" *> tok "left")
--
-- parseRobotStop :: Parser Robot
-- parseRobotStop = Stop <$ (tok "stop")

syntax :: Parser ()
syntax = many (oneOf ";:,\n") *> pure ()

1.3


parseRobot :: Parser Robot
parseRobot = Move <$ tok "forward" <*> number <*> parseRobot
          <|> Rright <$ (tok "rotate" *> tok "right") <*> parseRobot
          <|> Lleft <$ (tok "rotate" *> tok "left") <*> parseRobot
          <|> Stop <$ (tok "stop")

--   :t (Move <$ tok "forward") = Parser (Int -> Robot)
-- <$ :: a -> Parser b -> Parser a
-- So we're running this as: Robot -> Parser String -> Parser Robot
-- For it to be type Robot, Move needs to take in an Int as the def: data Robot = Move Int
-- This lets us use <*> as it takes a Parser (Int -> Robot) and a Parser (Int), applies the function
-- in the first Parser to the contents of the second to get us Parser Robot ta da Hi

-- newtype Parser a = Parser (String -> [(String, a)])
