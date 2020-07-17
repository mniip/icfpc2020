import Modem

main = writePCM "/dev/stdout" . map abs . cleanse =<< readWAV "/dev/stdin"
