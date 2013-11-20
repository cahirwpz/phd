; czy na poziomie spada da się oddzielić NULL(Boolean) od NULL(List) ?
; EXIT == RETURN-FROM SEQ

(SDEFUN |FOOLIST;leni;LSi;1|
        ((|l| |List| (|SingleInteger|)) ($ |SingleInteger|))
        (SPROG ((|res| (|SingleInteger|)))
               (SEQ (LETT |res| 0 . #1=(|FOOLIST;leni;LSi;1|))
                    (SEQ G190
                         (COND
                          ((NULL (COND ((NULL |l|) 'NIL) ('T 'T))) (GO G191)))
                         (SEQ (LETT |res| (|add_SI| |res| 1) . #1#)
                              (EXIT (LETT |l| (CDR |l|) . #1#)))
                         NIL (GO G190) G191 (EXIT NIL))
                    (EXIT |res|)))) 

(SDEFUN |FOOLIST;lenr_aux|
        ((|l| |List| (|SingleInteger|)) (|s| |SingleInteger|)
         ($ |SingleInteger|))
        (COND ((NULL |l|) |s|)
              ('T (|FOOLIST;lenr_aux| (CDR |l|) (|add_SI| |s| 1) $)))) 

(SDEFUN |FOOLIST;lenr;LSi;3|
        ((|l| |List| (|SingleInteger|)) ($ |SingleInteger|))
        (|FOOLIST;lenr_aux| |l| 0 $)) 

(SDEFUN |FOOLIST;sumi;LSi;4|
        ((|l| |List| (|SingleInteger|)) ($ |SingleInteger|))
        (SPROG ((|res| (|SingleInteger|)))
               (SEQ (LETT |res| 0 . #1=(|FOOLIST;sumi;LSi;4|))
                    (SEQ G190
                         (COND
                          ((NULL (COND ((NULL |l|) 'NIL) ('T 'T))) (GO G191)))
                         (SEQ
                          (LETT |res| (|add_SI| |res| (|SPADfirst| |l|)) . #1#)
                          (EXIT (LETT |l| (CDR |l|) . #1#)))
                         NIL (GO G190) G191 (EXIT NIL))
                    (EXIT |res|)))) 

(SDEFUN |FOOLIST;sumr_aux|
        ((|l| |List| (|SingleInteger|)) (|s| |SingleInteger|)
         ($ |SingleInteger|))
        (COND ((NULL |l|) |s|)
              ('T
               (|FOOLIST;sumr_aux| (CDR |l|) (|add_SI| |s| (|SPADfirst| |l|))
                $)))) 

(SDEFUN |FOOLIST;sumr;LSi;6|
        ((|l| |List| (|SingleInteger|)) ($ |SingleInteger|))
        (|FOOLIST;sumr_aux| |l| 0 $)) 

(SDEFUN |main| (($ |Void|))
       (SPROG ((|l| (|List| (|SingleInteger|))))
             (SETQ |l| (CONS 5 (CONS 15 (CONS 2 (CONS 20 NIL)))))
             (|print_SI| (|FOOLIST;sumr;LSi;6| |l| NIL))
             (|print_SI| (|FOOLIST;lenr;LSi;3| |l| NIL))))
