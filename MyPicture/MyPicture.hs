--mk12ss@outlook.com 
--char picture
--做过一些手动测试
module MyPicture where
import Data.Char
import Data.List

type Picture = [[Char]]
type Dictionary = [Picture]

characters::Dictionary
characters = [["     AA     ","    A  A    ","   AAAAAA   ","  A      A  "," A        A "]
             ,[" BBBBBBBB   "," B      BB  "," BBBBBBBB   "," B      BB  "," BBBBBBBB   "]
             ,["   CCCCCC   ","  CC        "," CC         ","  CC        ","   CCCCCC   "]
             ,[" DDDDDDDD   "," D      DD  "," D       DD "," D      DD  "," DDDDDDDD   "]
             ,[" EEEEEEEE   "," E          "," EEEEEEEE   "," E          "," EEEEEEEE   "]
             ,[" FFFFFFFF   "," F          "," FFFFFFFF   "," F          "," F          "]
             ,["   GGGGGG   ","  GG        "," GG     GG  ","  GG    GG  ","   GGGGGG   "]
             ,[" H       H  "," H       H  "," HHHHHHHHH  "," H       H  "," H       H  "]
             ,["    IIII    ","     II     ","     II     ","     II     ","    IIII    "]
             ,["    JJJJ    ","     JJ     ","     JJ     ","  J  JJ     ","   JJJ      "]
             ,[" K     KK   "," K   KK     "," K KK       "," K   KK     "," K     KK   "]
             ,[" LL         "," LL         "," LL         "," LL         "," LLLLLLLLL  "]
             ,[" MM       MM"," M M     M M"," M  M   M  M"," M   M M   M"," M    M    M"]
             ,[" NNN    N   "," N  N   N   "," N   N  N   "," N    N N   "," N     NN   "]
             ,["   OOOOOO   ","  OO    OO  "," OO      OO ","  OO    OO  ","   OOOOOO   "]
             ,["   PPPPPP   ","   P    PP  ","   PPPPPP   ","   PP       ","   PP       "]
             ,["   QQQQQQ   ","  QQ    QQ  "," QQ      QQ ","  QQ   QQQ  ","   QQQQQQ QQ"]
             ,["   RRRRRR   ","   R    RR  ","   RRRRRR   ","   R   R     ","   R     R "]
             ,["    SSSSS   ","  SS        ","     SSS    ","         SS ","    SSSSSS  "]
             ,["  TTTTTTTT  ","     TT     ","     TT     ","     TT     ","     TT     "]
             ,["  UU     UU ","  UU     UU ","  UU     UU ","  UU     UU ","   UUUUUUU  "]
             ,[" V        V ","  V      V  ","   V    V   ","    V  V    ","     VV     "]
             ,[" W   WW   W "," W   Ww   W "," W  W  W  W "," W W    W W ","  W      W  "]
             ,["  X       X ","    X   X   ","     XX     ","    X   X   ","  X       X "]
             ,["  Y      Y  ","   Y    Y   ","     YY     ","     YY     ","     YY     "]
             ,["  ZZZZZZZZ  ","       ZZ   ","     ZZ     ","    ZZ      ","    ZZZZZZZ "]]

numbers::Dictionary
numbers = [["   000000   ","  00    00  "," 00      00 ","  00    00  ","   000000   "]
          ,["     11     ","    111     ","     11     ","     11     ","    1111    "]
          ,["    22222   ","        22  ","     222    ","   22       ","   2222222  "]
          ,["    33333   ","        33  ","     333    ","        33  ","    33333   "]
          ,["       444  ","     4   4  ","    4444444 ","         4  ","         4  "]
          ,["    555555  ","    55      ","    55555   ","        55  ","    55555   "]
          ,["    666666  ","        66  ","    666666  ","    66  66  ","    666666  "]
          ,["   7777777  ","        7   ","       7    ","      7     ","     7      "]
          ,["    888888  ","    88  88  ","    888888  ","    88  88  ","    888888  "]
          ,["     9999   ","    99  99  ","     99999  ","        9   ","       9    "]]

sayit :: String -> IO ()
sayit = putStr . say

say :: String -> String
say x = picToString (sayPic x empty) []
      where empty = ["","","","",""]

--convert string to picture by conbert every single characters of the string into pictures
--, then combine these pictures into one piece side by side
sayPic::String->Picture->Picture
sayPic [] result = result
sayPic (x:xs) result = sayPic xs (sideBySide result (charToPic x)) 

--combine 2 pictures into one side by side
sideBySide :: Picture -> Picture -> Picture
sideBySide = zipWith (++)

--turn a single character into a picture
--simply find the equivalent picture in the dictionary by the index of the character of the alphabet
--because the dictionaries are well ordered
charToPic::Char->Picture
charToPic ch = if (nch >= nA && nch <= nZ) then characters!!(nch-nA)
               else numbers!!(nch-(ord '0'))
               where nch = ord (upper ch)
                     nA = ord 'A'
                     nZ = ord 'Z'

--turn the characters capital
upper::Char->Char
upper a = if (nch >= na && nch <= nz) then chr (nch-32)
          else a
            where nch = ord a
                  na = ord 'a'
                  nz = ord 'z'

--convert the picture into string so that it can be output by "putStr"
picToString::Picture->String->String
picToString [] result = result
picToString (x:xs) result = picToString xs (result ++ x ++ "\n")