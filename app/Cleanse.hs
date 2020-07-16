import Modem

main = writePCM "/dev/stdout" . smooth . map abs . cleanse =<< readWAV "/dev/stdin"
