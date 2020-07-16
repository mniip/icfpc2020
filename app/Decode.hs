import Modem

main = (wrap =<< guessWrap) =<< demodulate <$> readWAV "/dev/stdin"
