\section{Introduction}

\begin{code}
{-# LANGUAGE CPP #-}
module
 NiceSymbols
  ( mathBold, mathSansBold, flags
  , bold, underline
  , black, red, green, yellow, blue, magenta, cyan, white
  , lsq, rsq, ldq, rdq
  , _ll, _gg
  , _alpha, _beta, _theta, _iota, _mu, _pi
  , _epsilon, _tau, _Sigma, _omega, _lambda, _Lambda
  , _top, _bot, _sqcap, _sqcup, _sqsubseteq, _sqsupseteq
  , _lhd, _rhd
  , _true , _false , _lnot, _land, _lor, _implies, _equiv
  , _forall, _exists
  , _emptyset, _cup, _cap, _setminus
  , _in, _subseteq, _supseteq, _varnothing
  , _neq, _notin, _nexists, _nsubseteq
  , _langle, _rangle
  , _parallel, _Cap
  , _infty, _star
  , _bullet, _fun, _pfun, _ffun, _maplet, _times
  , _triangleq, _vdash
  , _qed, _redQ
  , _overline
  , _supStr, _supNum, _subStr, _subNum
  , _mathcal, cmathcal, _mathbb, cmathbb
  , dia_line, dia_3dots, dia_lhook, dia_rhook
  , dia_larrow, dia_rarrow, dia_lrarrow
  , niceSymMap, findSym
  , help
  , widthHack
  )
where
import Data.Char
import Numeric
import Data.Map (Map)
import qualified Data.Map as M
--import qualified Data.Text as T     -- Don't help,
--import qualified Data.Text.IO as T  --  still require widthHack !

versionNS = "0.5.2"
\end{code}


We define some nice symbols using unicode, for non-Windows usage,
along with dull ASCII equivalents for Windows users.

We assume UTF-8 throughout.

Given a LaTeX symbol macro like \verb"\xyz"
we define a Haskell variable \verb"_xyz"
to be a string that gives either a Unicode/UTF-8 glyph for that symbol,
or an approximation using ``ASCII-art''.

\newpage
\section{Platform Independent Code}

\subsection{Follow each character by \dots}

\begin{code}
follow "" _ = ""
follow (c:cs) a = c:a:follow cs a
\end{code}


\subsection{Alphabet conversions}

How to convert ASCII `a' to `z' into different fontstyles, in UTF-8
\\(See \verb"http://qaz.wtf/u/convert.cgi?text=az").

\begin{tabular}{|l|r|r|}
  \hline
  Style          & Code for 'A' & Code for `a'
  \\\hline
  ASCII          & 65           & 97
  \\\hline
  Math Sans Bold & 120276       & 120302
  \\\hline
\end{tabular}
~\\
\begin{code}
styleShift code_A code_a c
 | isUpper c  =  chr (ord c + upperShift)
 | isLower c  =  chr (ord c + lowerShift)
 | otherwise  =  c
 where
   upperShift = code_A - ord 'A'
   lowerShift = code_a - ord 'a'

mathBold     = map $ styleShift 119808 119834
mathSansBold = map $ styleShift 120276 120302
flags        = map $ styleShift 127462 127462
test = putStrLn . map (styleShift 119886 119886)
\end{code}

\newpage
\section{Weight Conversions}

ANSI escape sequences out here
\begin{code}
eSGR n = "\ESC["++show n++"m"

resetSGR  = eSGR 0
boldSGR   = eSGR 1
undlSGR   = eSGR 4
ovlSGR    = eSGR 9
fontSGR i = eSGR (i+10)

colorSGR  i = eSGR (i+30)
bcolorSGR i = eSGR (i+40)
\end{code}

\subsection{Weight Conversion for Unix/OS X}

\begin{code}
#ifndef mingw32_HOST_OS
\end{code}

\begin{code}
bold str      = boldSGR    ++ str ++ resetSGR
overline str  = ovlSGR     ++ str ++ resetSGR
underline str = undlSGR    ++ str ++ resetSGR
color i str   = colorSGR i ++ str ++ resetSGR
[black,red,green,yellow,blue,magenta,cyan,white] = map color [0..7]
\end{code}

\begin{code}
#endif
\end{code}

\subsection{Weight ``Conversion'' for Windows}

\begin{code}
#ifdef mingw32_HOST_OS
\end{code}

\begin{code}
bold str      = '*':str++"*"
underline str = '_':str++"_"
overline str  = '^':str++"^"
color i str   = colorSGR i ++ str ++ resetSGR
[black,red,green,yellow,blue,magenta,cyan,white] = map color [0..7]
\end{code}

\begin{code}
#endif
\end{code}


\newpage
\section{Nice Symbols}
\subsection{Nice Symbols for OS X/Unix}

\begin{code}
#ifndef mingw32_HOST_OS
\end{code}

\begin{code}
lsq = "\x2018"
rsq = "\x2019"
ldq = "\x201c"
rdq = "\x201d"

_ll = "\x00ab"
_gg = "\x00bb"

_alpha = "\x03b1"
_beta = "\x03b2"
_theta = "\x03b8"
_iota = "\x03b9"
_mu = "\x03bc"
_pi = "\x03c0"
_epsilon = "\x03f5"
_tau = "\x03c4"
_Sigma = "\x2211" -- "\x03a3" ?
_omega = "\x03c9"
_lambda = "\x03bb"
_Lambda = "\x039b"

_top = "\x22a4"
_bot = "\x22a5"
_sqcap = "\x2293"
_sqcup = "\x2294"
_sqsubseteq = "\x2291"
_sqsupseteq = "\x2292"
_lhd = "\x25c1"
_rhd = "\x25b7"

_true = bold "true"
_false = bold "false"
_lnot = "\x00ac"
_land = "\x2227"
_lor = "\x2228"
_implies = "\x27f9"
_equiv = "\x2261"

_forall = "\x2200"
_exists = "\x2203"

_emptyset = "\x00d8"
_cup = "\x222a"
_cap = "\x2229"
_setminus = "\x2216"
_in = "\x2208"
_subseteq = "\x2286"
_supseteq = "\x2287"
_varnothing = "\x2205"

_neq = "\x2260"
_notin = "\x2209"
_nexists = "\x2204"
_nsubseteq = "\x2288"

_langle = "\x27e8"
_rangle = "\x27e9"

_parallel = "\x2016"
_Cap = "\x22d2"

_infty = "\x221e"
_star = "\x2605"

_bullet = "\x2022"
_fun = "\x27f6"
_pfun = "\x21f8"
_ffun = "\x21fb"
_maplet = "\x27fc"
_times = "\x2a09"

_triangleq = "\x225c"

_vdash = "\x22a2"

_qed  = "\x220e"
_redQ = "\x2753"
\end{code}

\newpage
\begin{code}
_overline str = "\ESC[9m"++follow str '\x35e'++"\ESC[0m"

_supChar '1' = '\185'
_supChar '2' = '\178'
_supChar '3' = '\179'

_supChar 'A' = '\7468'
_supChar 'B' = '\7470'
_supChar 'D' = '\7472'
_supChar 'E' = '\7473'

_supChar 'a' = '\7491'
_supChar 'b' = '\7495'
_supChar 'c' = '\7580'
_supChar 'd' = '\7496'
_supChar 'e' = '\7497'
_supChar 'f' = '\7584'
_supChar 'g' = '\7501'
_supChar 'h' = '\688'
_supChar 'i' = '\8305'
_supChar 'j' = '\690'
_supChar 'k' = '\7503'
_supChar 'l' = '\737'
_supChar 'm' = '\7504'
_supChar 'n' = '\8319'
_supChar 'o' = '\7506'
_supChar 'p' = '\7510'
-- no q !
_supChar 'r' = '\691'
_supChar 's' = '\738'
_supChar 't' = '\7511'
_supChar 'u' = '\7512'
_supChar 'v' = '\7515'
_supChar 'w' = '\695' -- also '\7514'
_supChar 'x' = '\739'
_supChar 'y' = '\696'
_supChar 'z' = '\7611'

_supChar '+' = '\8314'
_supChar '-' = '\8315'
_supChar '(' = '\8317'
_supChar ')' = '\8318'

_supChar ',' = ','
_supChar '*' = '*'
_supChar '\x221e' = '\x221e'  -- infty
_supChar '\120596' = '\x1d5a'  -- omega
_supChar '\9733' = '*'    -- star

_supChar c
  | isDigit c = chr (ord c - ord '0' + 0x2070)
  | isSpace c = c
  | otherwise = '\175'

_supStr s = map _supChar s
_supNum n = _supStr $ show n

_subChar 'a' = '\x2090'
_subChar 'e' = '\x2091'
_subChar 'o' = '\x2092'
_subChar 'x' = '\x2093'
_subChar 'h' = '\x2095'
_subChar 'k' = '\x2096'
_subChar 'l' = '\x2097'
_subChar 'm' = '\x2098'
_subChar 'n' = '\x2099'
_subChar 'p' = '\x209a'
_subChar 's' = '\x209b'
_subChar 't' = '\x209c'

_subChar c
  | isDigit c = chr (ord c - ord '0' + 0x2080)
  | isSpace c = c
  | otherwise = '_'

_subStr s = map _subChar s
_subNum n = _subStr $ show n


_mathcal = map cmathcal

-- cmathcal 'B' = '\x212c' -- not great!
cmathcal 'E' = '\x2130'
-- cmathcal 'F' = '\x2131'
-- cmathcal 'H' = '\x210b'
-- cmathcal 'I' = '\x2110'
-- cmathcal 'L' = '\x2112'
-- cmathcal 'M' = '\x2133'
-- cmathcal 'R' = '\x211b'
cmathcal c
 | isUpper c  =  chr (ord c - ord 'A' + 0x1d4d0)
 | otherwise  =  c

_mathbb = map cmathbb

cmathbb 'C' = '\x2102'
cmathbb 'H' = '\x210d'
cmathbb 'N' = '\x2115'
cmathbb 'P' = '\x2119'
cmathbb 'Q' = '\x211a'
cmathbb 'R' = '\x211d'
cmathbb 'Z' = '\x2124'
cmathbb c
 | isUpper c  =  chr (ord c - ord 'A' + 0x1d538)
 | isLower c  =  chr (ord c - ord 'a' + 0x1d552)
 | isDigit c  =  chr (ord c - ord '0' + 0x1d7d8)
 | otherwise  =  c
\end{code}

\begin{code}
#endif
\end{code}

\newpage
\subsection{``Nice'' Symbols for Windows }

\begin{code}
#ifdef mingw32_HOST_OS
\end{code}

\begin{code}
lsq = "'"
rsq = "'"
ldq = "\""
rdq = "\""

_ll = "<<"
_gg = ">>"

_alpha = "alf"
_beta = "beta"
_theta = "theta"
_iota = "iota"
_mu = "mu"
_pi = "pi"
_epsilon = "eps"
_tau = "tau"
_Sigma = "Sigma"
_omega = "omega"
_lambda = "lambda"
_Lambda = "Lambda"

_top = "T"
_bot = "_|_"
_sqcap = "|~|"
_sqcup = "|_|"
_sqsubseteq = "|="
_sqsupseteq = "=|"
_lhd = "<|"
_rhd = "|>"

_true = "true"  -- bold true
_false = "false" -- bold false
_lnot = "~"
_land = "/\\"
_lor = "\\/"
_implies = "==>"
_equiv = "=="

_forall = "forall"
_exists = "exists"

_emptyset = "{}"
_cup = "U"
_cap = "I"
_setminus = "\\"
_in = "in"
_subseteq = "subset"
_supseteq = "supset"
_varnothing = "()"

_neq = "neq"
_notin = "notin"
_nexists = "nexists"
_nsubseteq = "nsubseteq"

_langle = "<"
_rangle = ">"

_parallel = "||"
_Cap = "II"

_infty = "inf"
_star = "*"

_bullet = "@"
_fun = "->"
_pfun = "-+>"
_ffun = "-++>"
_maplet = "|->"
_times = "x"

_triangleq = "^="

_vdash = "|-"

_qed = "[*]"
_redQ = "??"

_overline str = "ovl("++str++")"

_supStr = ('^':)
_supNum n = _supStr $ show n

_subStr = ('_':)
_subNum n = _subStr $ show n

cmathcal c = c
_mathcal str = str

cmathbb c = c
_mathbb str = str
\end{code}


\begin{code}
#endif
\end{code}


\newpage
\section{Diacritics}

\subsection{Diacritics for Unix/OS X}

\begin{code}
#ifndef mingw32_HOST_OS
\end{code}

\begin{code}
dia_line c =  c:"\773"
dia_3dots c = c:"\x20db"
dia_lhook c = c:"\x20d0"
dia_rhook c = c:"\x20d1"
dia_larrow c = c:"\x20d6"
dia_rarrow c = c:"\x20d7"
dia_lrarrow c = c:"\x20e1"
\end{code}

\begin{code}
#endif
\end{code}

\subsection{Diacritics for Windows}

\begin{code}
#ifdef mingw32_HOST_OS
\end{code}

We don't even try right now \dots
\begin{code}
dia_line c = [c]
dia_3dots c = [c]
dia_lhook c = [c]
dia_rhook c = [c]
dia_larrow c = [c]
dia_rarrow c = [c]
dia_lrarrow c = [c]
\end{code}

\begin{code}
#endif
\end{code}

\newpage
\section{Width Hack}

\subsection{Width Hack for OS X/Unix}

\begin{code}
widthHack :: Int -> String -> String

tst str = putStrLn $ unlines [ str, widthHack 1 str, widthHack 2 str ]
tstop op = tst (" A"++op++"B ; A "++op++" B .")

widthHelp = sequence_ (fmap (tstop . snd) niceSyms)
\end{code}

\begin{code}
#ifndef mingw32_HOST_OS

widthHack s cs = whack (replicate s ' ') cs

whack _ [] = []
whack ss (c:cs)
 | c `elem` badWidths  =  c:ss++cs'
 | otherwise           =  c:cs'
 where
   cs' = whack ss cs

badWidths = _implies ++ _star ++ _fun ++ _pfun ++ _ffun ++ _maplet
#endif
\end{code}

\subsection{Width Hack for Windows}

\begin{code}
#ifdef mingw32_HOST_OS

widthHack _ = id
#endif
\end{code}


\newpage
\section{Mainline}


Basically a catalog of our nice symbols that is easy to display in GHCi
\begin{code}
niceSyms
 = [ ("lsq",lsq)
   , ("rsq",rsq)
   , ("ldq",ldq)
   , ("rdq",rdq)
   , ("_ll", _ll)
   , ("_gg", _gg)
   , ("_pi", _pi)
   , ("_epsilon", _epsilon)
   , ("_tau", _tau)
   , ("_Sigma", _Sigma)
   , ("_omega", _omega)
   , ("_lambda", _lambda)
   , ("_Lambda", _Lambda)
   , ("_top", _top)
   , ("_bot", _bot)
   , ("_sqcap", _sqcap)
   , ("_sqcup", _sqcup)
   , ("_sqsubseteq", _sqsubseteq)
   , ("_sqsupseteq", _sqsupseteq)
   , ("_true", _true)
   , ("_false", _false)
   , ("_lnot", _lnot)
   , ("_land", _land)
   , ("_lor", _lor)
   , ("_implies", _implies)
   , ("_equiv", _equiv)
   , ("_exists", _exists)
   , ("_forall", _forall)
   , ("_emptyset", _emptyset)
   , ("_cup", _cup)
   , ("_cap", _cap)
   , ("_setminus", _setminus)
   , ("_in", _in)
   , ("_subseteq", _subseteq)
   , ("_supseteq", _supseteq)
   , ("_varnothing", _varnothing)
   , ("_neq", _neq)
   , ("_notin", _notin)
   , ("_nexists", _nexists)
   , ("_nsubseteq", _nsubseteq)
   , ("_langle", _langle)
   , ("_rangle", _rangle)
   , ("_parallel", _parallel)
   , ("_Cap", _Cap)
   , ("_infty", _infty)
   , ("_star", _star)
   , ("_bullet", _bullet)
   , ("_fun", _fun)
   , ("_pfun", _pfun)
   , ("_ffun", _ffun)
   , ("_maplet", _maplet)
   , ("_times", _times)
   , ("_triangleq", _triangleq)
   , ("_vdash", _vdash)
   , ("_qed", _qed)
   , ("_redQ", _redQ)
   ]

niceSymMap :: Map String String
niceSymMap = M.fromList niceSyms

findSym :: String -> Maybe String
findSym str = M.lookup str niceSymMap

niceDias
 = [ ("C", "C ")
   , ("dia_line(C)", dia_line 'C')
   , ("dia_3dots(C)", dia_3dots 'C')
   , ("dia_lhook(C)", dia_lhook 'C')
   , ("dia_rhook(C)", dia_rhook 'C')
   , ("dia_rarrow(C)", dia_rarrow 'C')
   , ("dia_larrow(C)", dia_larrow 'C')
   , ("dia_lrarrow(C)", dia_lrarrow 'C')
   ]

aLower = ['a'..'z']
aUpper = ['A'..'Z']
niceFuns
 = [ ("bold(string)", bold "string" )
   , ("underline(string)", underline "string" )
   -- , ("overline(string)", overline "string" )
   -- , ("_overline(string)", _overline "string")
   , ("black(string)", black "string" )
   , ("red(string)", red "string" )
   , ("green(string)", green "string" )
   , ("yellow(string)", yellow "string" )
   , ("blue(string)", blue "string" )
   , ("magenta(string)", magenta "string" )
   , ("cyan(string)", cyan "string" )
   , ("white(string)", white "string" )
   , ("red(bold(underline(string)))", red $ bold $ underline "string" )
   , ("_mathcal('A'..'Z')", _mathcal aUpper )
   , ("_mathbb('A'..'9')", _mathbb (aUpper++"abyz0189") )
   , ("red(_mathbb('A'..'9'))", red $ _mathbb (aUpper++"abyz0189") )
   , ("bold(_mathbb('A'..'9'))", bold $ _mathbb (aUpper++"abyz0189") )
   , ("underline(_mathbb('A'..'9'))", underline $ _mathbb (aUpper++"abyz0189") )
   , ( "A _supStr(\"a..z\")", 'A':_supStr( ['a'..'z']))
   , ( "A _supStr(\"A..Z\")", 'A':_supStr( ['A'..'Z']))
   , ("x _supNum(9876543210)", 'x':_supNum 9876543210)
   , ( "A _subStr(\"a..z\")", 'A':_subStr( ['a'..'z']))
   , ( "A _subStr(\"A..Z\")", 'A':_subStr( ['A'..'Z']))
   , ("x _subNum(9876543210)", 'x':_subNum 9876543210)
   ]

niceRender w (_nm, nm@[uc])
 = _nm ++ (replicate (w-length _nm) ' ') ++ "  " ++ nm ++ "   " ++ hexRender uc
niceRender w (_nm, nm)
 = _nm ++ (replicate (w-length _nm) ' ') ++ "  " ++ nm

hex i = showHex i ""

hexRender uc = hexPad $ hex $ ord uc

hexPad hstr
 | len <= 4  =  pad4 len hstr
 | len <= 8  =  pad4 (len-4) hleft ++ ' ':hright
 where
   len = length hstr
   pad4 l str = (replicate (4-l) '0') ++ str
   (hleft,hright) = splitAt (len-4) hstr
\end{code}

Use \verb"help" in GHCi to see the available strings and functions.
\begin{code}
help
 = do putStrLn ("Nice Symbols v"++versionNS++" listing:")
      putStrLn $ unlines $ map (niceRender maxw1) niceSyms
      putStrLn ("Nice Diacritics v"++versionNS++" listing:")
      putStrLn $ unlines $ map (niceRender maxw2) niceDias
      putStrLn ("Nice Functions v"++versionNS++" listing:")
      putStrLn $ unlines $ map (niceRender maxw3) niceFuns
 where
   maxw1 = maximum $ map (length . fst) niceSyms
   maxw2 = maximum $ map (length . fst) niceDias
   maxw3 = maximum $ map (length . fst) niceFuns
\end{code}
