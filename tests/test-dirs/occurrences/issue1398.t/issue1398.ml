let (let++) x f = f x
let (and++) a b = (a, b) ;;
let ops = (let++), (and++) ;;
let++ x = 3 and++ y = 4 in x + y
