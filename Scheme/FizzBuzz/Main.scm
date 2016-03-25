(define (fizz-buzz i)
    (cond
        ((= 0 (remainder i 15)) "FizzBuzz")
        ((= 0 (remainder i 5)) "Buzz")
        ((= 0 (remainder i 3)) "Fizz")
        (else (number->string i))))

(define (main args)
    (let loop((i 1))
        (if (> i 100)
            0
            (begin
                (display (fizz-buzz i))
                (newline)
                (loop (+ i 1))))))
