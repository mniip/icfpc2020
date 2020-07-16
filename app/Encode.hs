import Data.Char
import Modem

main = writePCM "/dev/stdout" . modulate . map (not . isSpace) =<< getContents
