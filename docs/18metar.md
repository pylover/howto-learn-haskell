% Case Study: Parsing Metar Reports
% Jim Royer
% Wed Apr  3 23:04:38 EDT 2019

---
lang: en
...

# Acknowledgement #

The following is based on

* [*Parser Combinators: Parsing for Haskell Beginners*, from the **Two Wrongs** Blog, by kqr, published 2016-01-04]( https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html )

* [The METAR Wikipedia article](https://en.wikipedia.org/wiki/METAR)

<img align=center src="./airplane.jpg" width="80%"/>

# METAR

* METAR is a standard format for reporting weather information.
* Widely used by pilots and meteorologists
* *Example (from Wikipedia)* <pre>LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N
R22/P1500U +SN BKN022 OVC050 M04/M07 Q1020 NOSIG 8849//91=</pre>
	* **LBBG** is the ICAO airport code for Burgas Airport.
	* **041600Z** indicates the time of the observation. It is the day
      of the month (04) followed by the time of day (1600 Zulu time,
      which equals 4:00 pm Greenwich Mean Time or 6:00 pm local
      time). 
	* **12012MPS** indicates the wind direction is from 120°
      (east-southeast) at a speed of 12 m/s (23 knots; 27 mph; 44
      km/h). Speed measurements can be in knots (abbreviated KT) or
      meters per second (abbreviated MPS).
    * ...
    * See <http://metars.com> for more examples with translations.

# The ReadP parsing library

See <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/Cabal-2.4.0.1/Distribution-Compat-ReadP.html>

* <tt>get</tt> is equivalent to Hutton's <tt>item</tt>

* <tt>(<++)</tt> is left-biased choice

* <tt>(+++)</tt> is symmetric choice (We'll use <tt>(<|>)</tt>)

* <tt>pfail</tt> is the equivalent to Hutton's <tt>failure</tt>

* &hellip;

# Beginning samples

~~~~ {.haskell}
import Text.ParserCombinators.ReadP

isVowel :: Char -> Bool
isVowel char = char `elem` "aeiouAEIOU"

vowel :: ReadP Char
vowel = satisfy isVowel

parse = readP_to_S 
~~~~

~~~~ 
*Metar> parse vowel "one"
[('o',"ne")]

*Metar> parse vowel "two"
[]
~~~~

# ReadP gives you *all* the parses.

~~~~ {.haskell}
atLeastOneVowel :: ReadP [Char]
atLeastOneVowel = many1 vowel
~~~~

~~~~ 
*Metar> parse atLeastOneVowel "aouibcdef"
[("a","ouibcdef")
,("ao","uibcdef")
,("aou","ibcdef")
,("aoui","bcdef")
]
~~~~

# Back to Metar: Airports

* The first word is the airport name.
* It consistes of one or more uppercase letters.
* It is terminated by a space.
* So, a first try:

~~~~ {.haskell}
airport0 :: ReadP String
airport0 = do
    code <- many1 (satisfy isUpper)
    return code    
~~~~

~~~~
*Metar> sample1
"KSYR 031454Z 10006KT 6SM -RA ..."

*Metar> parse airport9 sample1
[("K","SYR 031454Z 10006KT 6SM -RA ...")
,("KS","YR 031454Z 10006KT 6SM -RA ...")
,("KSY","R 031454Z 10006KT 6SM -RA ...")
,("KSYR"," 031454Z 10006KT 6SM -RA ...")]
~~~~


# Airports, Second try

* The first word is the airport name.
* It consistes of one or more uppercase letters.
* It is terminated by a space.
* We use the trailing space to disambiguate.


~~~~ {.haskell}
airport :: ReadP String
airport = do
    code <- many1 (satisfy isUpper)
    satisfy (== ' ')
    return code         
~~~~

~~~~
*Metar> sample1
"KSYR 031454Z 10006KT 6SM -RA ..."
*Metar> parse airport sample1
[("KSYR","031454Z 10006KT 6SM -RA ...")]
~~~~

# Time stamps, Try 1

* The time and date stamp is of the form
<pre>&LT;day-of-month&GT;&LT;hours&GT;&LT;minutes&GTZ </pre>

* *Example:* <tt>031454Z </tt>

* Each of the day-of-month, hours, and minutes fields consist of two
  digits.

* The Z is followed by a space.
  
~~~~ {.haskell}
digit :: ReadP Char
digit = satisfy isDigit

timestamp1 :: ReadP (String, String, String)
timestamp1 = do
    day    <- count 2 digit
    hour   <- count 2 digit
    minute <- count 2 digit
    string "Z "
    return (day, hour, minute)     
~~~~

~~~~
*Metar> parse timestamp1 "031454Z ..."
[(("03","14","54"),"...")]
~~~~

* Problem: We want the day, hour, minutes as <tt>Int</tt>s.

# Time stamps, Try 2

* We want the day, hour, minutes as <tt>Int</tt>s.
  
~~~~ {.haskell}
timestamp2 :: ReadP (Int, Int, Int)
timestamp2 = do
    day    <- count 2 digit
    hour   <- count 2 digit
    minute <- count 2 digit
    string "Z "
    return (read day, read hour, read minute)     
~~~~

~~~~
*Metar> parse timestamp2 "031454Z ..."
[((3,14,54),"...")]
~~~~

* Problem: We want to check that the day, hour, minutes are sensible
values and fail if they are not.

* So it makes sense to convert time to <tt>Int</tt> earlier.

# Time stamps, Try 2<tt>'</tt>

* Problem: We want to check that the day, hour, minutes are sensible
values and fail if they are not.

* So it makes sense to convert time to <tt>Int</tt> earlier.

~~~~ {.haskell}
timestamp2' :: ReadP (Int, Int, Int)
timestamp2' = do
    day    <- fmap read (count 2 digit)
    hour   <- fmap read (count 2 digit)
    minute <- fmap read (count 2 digit)
    string "Z "
    return (day, hour, minute)
~~~~

* While we are at it, lets define a separate function for reading
digits and returning numbers

# Time stamps, Try 2<tt>''</tt>

~~~~ {.haskell}
numbers :: Int -> ReadP Int
numbers digits = fmap read (count digits digit)

numbers' :: Int -> ReadP Int
numbers' digits = do
    parse <- count digits digit
    return (read parse)

timestamp2'' :: ReadP (Int, Int, Int)
timestamp2'' = do
    day    <- numbers 2
    hour   <- numbers 2
    minute <- numbers 2
    string "Z "
    return (day, hour, minute)
~~~~	

* Now lets add in the validity check.

# Time stamps, Final version

~~~~ {.haskell}
timestamp :: ReadP (Int, Int, Int)
timestamp = do
    day    <- numbers 2
    hour   <- numbers 2
    minute <- numbers 2
    string "Z "
    if day < 1 || day > 31 || hour > 23 || minute > 59
       then pfail
       else return (day, hour, minute)           
~~~~

# Wind information, The simple version

Wind Information (without gusts) consists of

* A three digit number (the degree of the direction)

* A two or three digit number (the speed)

* Either <tt>"KT"</tt> (for knots) or <tt>MPS</tt> (for meters per
second) and then a space.

* We use <tt>&LT;|&GT;</tt> (from Control.Applicative) for choice.

~~~~ {.haskell}
windInfo1 = do
    direction <- numbers 3
    speed     <- numbers 2 <|> numbers 3
    unit      <- string "KT" <|> string "MPS"
    string " "
    return (direction, speed, unit)
~~~~

# Wind information, The simple version improved

* Lets pick MPS as the standard speed unit 

~~~~ {.haskell}
toMPS :: String -> Int -> Int
toMPS "KT" speed  = speed `div` 2
toMPS "MPS" speed = speed

windInfo1' :: ReadP (Int, Int)
windInfo1' = do
    direction <- numbers 3
    speed     <- numbers 2 <|> numbers 3
    unit      <- string "KT" <|> string "MPS"
    string " "
    return (direction, toMPS unit speed)
~~~~

# Wind information, Dealing with gusts

* If there are wind gusts, the wind information can be of the form
<pre>18027G31KT</pre>
I.e., wind from the south of 27 knots with gusts of 31 knots

* We can try:

~~~~ {.haskell}
gustParser :: ReadP Int
gustParser = do
    satisfy (== 'G')
    numbers 2 <|> numbers 3                    

windInfo2 = do
    direction <- numbers 3
    speed     <- numbers 2 <|> numbers 3
    gusts     <- gustParser
    unit      <- string "KT" <|> string "MPS"
    string " "
    return ( direction
	       , (toMPS unit speed)
	       , (toMPS unit gusts)
           )
~~~~

* But this will fail for any report without gusts


# Dealing with options


~~~~ {.haskell}
option :: a -> ReadP a -> ReadP a
option v p = -- try p, but if it fails, return v
~~~~ 

~~~~
*Metar> parse (option '?' vowel) "abc"
[('?',"abc"),('a',"bc")]

*Metar> parse (option '?' vowel) "xyz"
[('?',"xyz")]
~~~~

* For gust, what do we want as our optional value?

* <tt>0</tt> is a poor choice

* We use <tt>Maybe</tt> types. I.e.,
  * <tt>Nothing</tt> &nbsp;&equiv;&nbsp; no gusts
  * <tt>Just 12</tt>  &nbsp;&equiv;&nbsp; gust up to 12

~~~~ {.haskell}
windInfo3 :: ReadP (Int, Int, Maybe Int)
windInfo3 = do
    direction <- numbers 3
    speed     <- numbers 2 <|> numbers 3
    gusts     <- option Nothing (fmap Just gustParser)
    unit      <- string "KT" <|> string "MPS"
    string " "
    return (direction
           , (toMPS unit speed)
           , (fmap (toMPS unit) gusts))
~~~~

# Let us tidy up what we have, 1

Let us introduce a few data types for packaging info.

~~~~ {.haskell}
data WindInfo = WindInfo { dir   :: Int
                         , speed :: Int
                         , gusts :: Maybe Int
                         }
                deriving Show    

data Report =   Report   { station :: String
                         , time    :: (Int,Int,Int)
                         , wind    :: WindInfo
                         }
                deriving Show
~~~~

# Let us tidy up what we have, 2

Now let us revise WinInfo and write a general report function. 

~~~~ {.haskell}
windInfo :: ReadP WindInfo
windInfo = do
    direction <- numbers 3
    speed     <- numbers 2 <|> numbers 3
    gusts     <- option Nothing (fmap Just gustParser)
    unit      <- string "KT" <|> string "MPS"
    string " "
    return (WindInfo
              direction
              (toMPS unit speed)
              (fmap (toMPS unit) gusts))
                    
metar :: ReadP Report
metar = do
    code <- airport
    time <- timestamp
    wind <- windInfo
    return (Report code time wind)
~~~~

# Etc.

* This is typical of the sort of thing you do for log files.
* But there are more challenging things, ...

# Colophon #

<!-- Foo bar -->

These pages were produced from a [Markdown][] source,
[18metar.md](18metar.md), using
[pandoc][] (with [pandoc.css][]) via
```
pandoc -s --webtex --css pandoc.css -t slidy 18metar.md -o 18metar.html
```

[pandoc]: http://www.pandoc.org "The Pandoc homepage"

[Markdown]: https://en.wikipedia.org/wiki/Markdown "markdown on Wikipedia"

[pandoc.css]: https://gist.github.com/killercup/5917178 
