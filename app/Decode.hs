import Modem
import Blocks

main = display . wrap =<< demodulate <$> readWAV "/dev/stdin"
