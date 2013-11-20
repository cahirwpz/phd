; fac(n : SingleInteger) : SingleInteger ==
;   if n < qconvert(2)$SingleInteger
;     then n
;     else n * fac(n - 1)

(SDEFUN |ZZZ;fac| ((|n| |SingleInteger|) ($ |SingleInteger|))
        (COND ((|less_SI| |n| 2) |n|)
              ('T (|mul_SI| |n| (|ZZZ;fac| (|sub_SI| |n| 1) $))))) 

; fac_acc(n : SingleInteger, acc : SingleInteger) : SingleInteger ==
;   if n < qconvert(2)$SingleInteger
;     then acc
;     else fac_acc(n - 1, acc * n)

(SDEFUN |ZZZ;fac_acc|
        ((|n| |SingleInteger|) (|acc| |SingleInteger|) ($ |SingleInteger|))
        (COND ((|less_SI| |n| 2) |acc|)
              ('T (|ZZZ;fac_acc| (|sub_SI| |n| 1) (|mul_SI| |acc| |n|) $)))) 

; fib(n : SingleInteger) : SingleInteger ==
;   if n < 2
;     then n
;     else fib(n - 1) + fib(n - 2)

(SDEFUN |ZZZ;fib| ((|n| |SingleInteger|) ($ |SingleInteger|))
        (COND ((|less_SI| |n| 2) |n|)
              ('T
               (|add_SI| (|ZZZ;fib| (|sub_SI| |n| 1) $)
                         (|ZZZ;fib| (|sub_SI| |n| 2) $))))) 

; gcd(a : SingleInteger, b : SingleInteger) : SingleInteger ==
;   while not(a = b) repeat
;     if b < a
;       then a := a - b
;       else b := b - a
;   a

(SDEFUN |ZZZ;gcd|
        ((|a| |SingleInteger|) (|b| |SingleInteger|) ($ |SingleInteger|))
        (SEQ
         (SEQ G190
              (COND
               ((NULL (COND ((|eql_SI| |a| |b|) 'NIL) ('T 'T))) (GO G191)))
              (SEQ
               (EXIT
                (COND
                 ((|less_SI| |b| |a|)
                  (LETT |a| (|sub_SI| |a| |b|) . #1=(|ZZZ;gcd|)))
                 ('T (LETT |b| (|sub_SI| |b| |a|) . #1#)))))
              NIL (GO G190) G191 (EXIT NIL))
         (EXIT |a|))) 

; sqrt(n : DoubleFloat) : DoubleFloat ==
;   n < (0.0 :: DoubleFloat) =>
;     error "sqrt on negative number not defined"
;
;   err := (0.000001 :: DoubleFloat)
;
;   old_n := n
;   new_n := (old_n + n / old_n) * (0.5 :: DoubleFloat)
;
;   while err < abs((new_n - old_n) / new_n) repeat
;     old_n := new_n
;     new_n := (old_n + n / old_n) / (2.0 :: DoubleFloat)
;
;   new_n 

(SDEFUN |ZZZ;sqrt| ((|n| . #1=(|DoubleFloat|)) ($ |DoubleFloat|))
        (SPROG
         ((|new_n| (|DoubleFloat|)) (|old_n| #1#) (|err| (|DoubleFloat|)))
         (SEQ
          (COND
           ((|less_DF| |n| 0.0)
            0.0) ;(|error| "sqrt on negative number not defined"))
           ('T
            (SEQ
             (LETT |err| 0.000001 . #2=(|ZZZ;sqrt|))
             (LETT |old_n| |n| . #2#)
             (LETT |new_n|
                   (|mul_DF| (|add_DF| |old_n| (|div_DF| |n| |old_n|))
                             0.5)
                   . #2#)
             (SEQ G190
                  (COND
                   ((NULL
                     (|less_DF| |err|
                                (|abs_DF|
                                 (|div_DF| (|sub_DF| |new_n| |old_n|)
                                           |new_n|))))
                    (GO G191)))
                  (SEQ (LETT |old_n| |new_n| . #2#)
                       (EXIT
                        (LETT |new_n|
                              (|div_DF|
                               (|add_DF| |old_n| (|div_DF| |n| |old_n|))
                               2.0)
                              . #2#)))
                  NIL (GO G190) G191 (EXIT NIL))
             (EXIT |new_n|))))))) 

(SDEFUN |main| (($ |Void|))
       (PROGN
         ; wykryć złą ilość argumentów!
         (|print_SI| (|ZZZ;fac| 8 NIL))
         (|print_SI| (|ZZZ;fac_acc| 10 1 NIL))
         (|print_SI| (|ZZZ;fib| 10 NIL))
         (|print_SI| (|ZZZ;gcd| 144 328 NIL))
         (|print_DF| (|ZZZ;sqrt| 2.0 NIL))))
