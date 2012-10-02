module Main where

-- maps ordinary latin characters to fullwidth equivalents
-- takes no parameters, goes from stdin to stdio
-- build with: ghc --make Fullwidth

import qualified Data.IntMap as I
import Data.Maybe (maybe)

fullW = I.fromList . map (\(s:t:[]) -> (fromEnum s, fromEnum t)) $ [
	"!！",
	"\"＂",
	"#＃",
	"$＄",
	"%％",
	"&＆",
	"'＇",
	"(（",
	")）",
	"*＊",
	"+＋",
	",，",
	"-－",
	".．",
	"/／",
	"0０",
	"1１",
	"2２",
	"3３",
	"4４",
	"5５",
	"6６",
	"7７",
	"8８",
	"9９",
	":：",
	";；",
	"<＜",
	"=＝",
	">＞",
	"?？",
	"@＠",
	"AＡ",
	"BＢ",
	"CＣ",
	"DＤ",
	"EＥ",
	"FＦ",
	"GＧ",
	"HＨ",
	"IＩ",
	"JＪ",
	"KＫ",
	"LＬ",
	"MＭ",
	"NＮ",
	"OＯ",
	"PＰ",
	"QＱ",
	"RＲ",
	"SＳ",
	"TＴ",
	"UＵ",
	"VＶ",
	"WＷ",
	"XＸ",
	"YＹ",
	"ZＺ",
	"[［",
	"\\＼",
	"]］",
	"^＾",
	"_＿",
	"`｀",
	"aａ",
	"bｂ",
	"cｃ",
	"dｄ",
	"eｅ",
	"fｆ",
	"gｇ",
	"hｈ",
	"iｉ",
	"jｊ",
	"kｋ",
	"lｌ",
	"mｍ",
	"nｎ",
	"oｏ",
	"pｐ",
	"qｑ",
	"rｒ",
	"sｓ",
	"tｔ",
	"uｕ",
	"vｖ",
	"wｗ",
	"xｘ",
	"yｙ",
	"zｚ",
	"{｛",
	"|｜",
	"}｝",
	"~～",
        "¢￠",
        "£￡",
        "¬￢",
        "¯￣",
        "¦￤",
        "¥￥",
        "₩￦"]

main = interact (map fullLook)

fullLook c = maybe c toEnum . flip I.lookup fullW . fromEnum $ c
