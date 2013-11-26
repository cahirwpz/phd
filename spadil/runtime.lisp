(SDECLFUN |error| ((|msg| (|CString|))) (|Exit|))

; SingleInteger
(SDECLFUN |box_SI| ((|raw_si| (|SingleInteger|))) (|Any|))
(SDECLFUN |unbox_SI| ((|box_si| (|Any|))) (|SingleInteger|))
(SDECLFUN |print_SI| ((|raw_si| (|SingleInteger|))) (|Void|))

; DoubleFloat
(SDECLFUN |box_DF| ((|raw_df| (|DoubleFloat|))) (|Any|))
(SDECLFUN |unbox_DF| ((|box_df| (|Any|))) (|DoubleFloat|))
(SDECLFUN |print_DF| ((|raw_df| (|DoubleFloat|))) (|Void|))

; Cons
(SDECLFUN CONS ((|hd| (|Any|)) (|tl| (|Any|))) (|Cons|))
(SDECLFUN CAR ((|l| (|Cons|))) (|Any|))
(SDECLFUN CDR ((|l| (|Cons|))) (|Any|))
(SDECLFUN NULL ((|l| (|Cons|))) (|Boolean|))
(SDECLFUN ATOM ((|l| (|Cons|))) (|Boolean|))
(SDECLFUN NREVERSE ((|l| (|Cons|))) (|Cons|))
(SDECLFUN |SPADfirst| ((|l| (|Cons|))) (|Any|))

; Vector 1D
(SDECLFUN MAKEARR1 
          ((|size| (|SingleInteger|)) (|init| (|SingleInteger|))) (|Vector|))
(SDECLFUN QREFELT
          ((|vector| (|Vector|)) (|index| (|SingleInteger|))) (|Any|))
(SDECLFUN QVREF
          ((|vector| (|Vector|))) (|CArray|))
(SDECLFUN QVSIZE
          ((|vector| (|Vector|))) (|SingleInteger|))
