\section{Introduction}

\begin{code}
{-# LANGUAGE CPP #-}
module
 NiceSymbols
  ( mathBold, mathSansBold, flags
  , bold, overline
  , _ll, _gg
  , _alpha, _beta, _theta, _iota, _mu, _pi
  , _epsilon, _tau, _Sigma, _omega, _lambda, _Lambda
  , _top, _bot, _sqcap, _sqcup, _sqsubseteq, _lhd, _rhd
  , _true , _false , _lnot, _land, _lor, _implies, _equiv
  , _forall, _exists
  , _emptyset, _cup, _cap, _setminus
  , _in, _subseteq, _varnothing
  , _langle, _rangle
  , _parallel, _Cap
  , _infty, _star
  , _bullet, _fun, _pfun, _ffun, _maplet, _times
  , _triangleq
  , _overline
  , _supStr, _supNum
  , _mathcal, cmathcal, _mathbb, cmathbb
  , help
  )
where
import Data.Char
import Numeric

versionNS = "0.4.2"
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

\subsection{Weight Conversion for Unix/OS X}

\begin{code}
#ifndef mingw32_HOST_OS

eSGR n = "\ESC["++show n++"m"

resetSGR = eSGR 0
boldSGR  = eSGR 1
ovlSGR   = eSGR 9

bold str = boldSGR ++ str ++ resetSGR
overline str = ovlSGR ++ str ++ resetSGR
#endif
\end{code}

\subsection{Weight ``Conversion'' for Windows}

\begin{code}
#ifdef mingw32_HOST_OS

bold str = '*':str++"*"
overline str = '^':str++"^"
#endif
\end{code}


\newpage
\section{Nice Symbols for OS X/Unix}

\begin{code}
#ifndef mingw32_HOST_OS

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
_lhd = "\x25c1"
_rhd = "\x25b7"

_true = bold "true"
_false = bold "false"
_lnot = "\x00ac"
_land = "\x2227"
_lor = "\x2228"
_implies = "\x21d2"
_equiv = "\x2261"

_forall = "\x2200"
_exists = "\x2203"

_emptyset = "\x00d8"
_cup = "\x222a"
_cap = "\x2229"
_setminus = "\x2216"
_in = "\x2208"
_subseteq = "\x2286"
_varnothing = "\x2205"

_langle = "\x27e8"
_rangle = "\x27e9"

_parallel = "\x2016"
_Cap = "\x22d2"

_infty = "\x221e"
_star = "\x2605"

_bullet = "\x2022"
_fun = "\x2192"
_pfun = "\x21f8"
_ffun = "\x21fb"
_maplet = "\x21a6"
_times = "\x2a09"

_triangleq = "\x225c"
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

#endif
\end{code}


\newpage
\section{``Nice'' Symbols for Windows }

\begin{code}
#ifdef mingw32_HOST_OS

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
_varnothing = "()"

_langle = "<"
_rangle = ">

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

_overline str = "ovl("++str++")"

_supStr = ('^':)
_supNum n = _supStr $ show n

cmathcal c = c
_mathcal str = str

cmathbb c = c
_mathbb str = str
#endif
\end{code}


\section{Mainline}


Basically a catalog of our nice symbols that is easy to display in GHCi
\begin{code}
niceSyms
 = [ ("_ll", _ll)
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
   , ("_varnothing", _varnothing)
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
   ]

aLower = ['a'..'z']
aUpper = ['A'..'Z']
niceFuns
 = [ ("bold(string)", bold "string" )
   , ("overline(string)", overline "string" )
   , ("_overline(string)", _overline "string")
   , ("_mathcal('A'..'Z')", _mathcal aUpper )
   , ("_mathbb('A'..'9')", _mathbb (aUpper++"abyz0189") )
   , ( "_supStr(\"abcdijklmnABCDIJKLMN\")"
      , _supStr( "abcdijklmnABCDIJKLMN"))
   , ("_supNum(9876543210)", _supNum 9876543210)
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
      putStrLn ("Nice Functions v"++versionNS++" listing:")
      putStrLn $ unlines $ map (niceRender maxw2) niceFuns
 where
   maxw1 = maximum $ map (length . fst) niceSyms
   maxw2 = maximum $ map (length . fst) niceFuns
\end{code}
