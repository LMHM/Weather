REM *** Program f”r flera dagars statistik
REM *** Namn: 24DAG_3a.BAS
REM *** Med RUN efter uppdatering tid p† PC:n
REM *** Med Run vid fel
REM *** Med korrektion f”r skott†r m.m.
REM Efter „ndring till <=0 f”r vintertemp



1   CLS
    UppTim = 17: UppMin = 12

    ON ERROR GOTO Omstart
    COLOR 11
    LOCATE 1, 11
    PRINT "TempMin     TempMax     VindMax      TryckMin     TryckMax  Nbd"
    COLOR 3
    LOCATE 2, 10
    PRINT " "; CHR$(248); "C  tid     "; CHR$(248); "C";
    PRINT ; "  tid    m/s  tid     hPa   tid    hpa   tid   mm"
REM    COLOR 3: LOCATE 2, 2: PRINT "Datum"; : LOCATE 2, 75: PRINT "Datum";
    COLOR 11: LOCATE 1, 2: PRINT "Datum"; : LOCATE 1, 75: PRINT "Datum";


    colv = 10: colt = 8
    OPEN "path20.txt" FOR INPUT AS #1
    INPUT #1, path$
    CLOSE #1
    

10   sek = VAL(RIGHT$(TIME$, 2))
REM LOCATE 10, 10: PRINT TIME$
    q$ = INKEY$: IF q$ = "X" OR q$ = "x" THEN CLOSE : END
REM    IF varv = 0 THEN 20
    IF sek = 50 AND klarflag = 0 THEN OPEN "klar20" FOR OUTPUT AS #4: CLOSE #4: klarflag = 1
    IF sek <> 50 THEN klarflag = 0
    IF sek <> 45 THEN flag = 0
    IF sek = 45 AND flag = 0 THEN 20
REM    IF sek <> 45 THEN flag = 0
    GOTO 10
    
20    flag = 1
REM 20 LOCATE 12, 10: INPUT "", q$
    aar$ = RIGHT$(DATE$, 2): aman$ = LEFT$(DATE$, 2): adag$ = MID$(DATE$, 4, 2)
    ar = VAL(aar$): man = VAL(aman$): dag = VAL(adag$)
    ndag = dag: nman = man: nar = ar
    
    IF nar < 10 THEN nar$ = "0" + RIGHT$(STR$(nar), 1) ELSE nar$ = RIGHT$(STR$(nar), 2)
    IF nman < 10 THEN nman$ = "0" + RIGHT$(STR$(nman), 1) ELSE nman$ = RIGHT$(STR$(nman), 2)
    IF ndag < 10 THEN ndag$ = "0" + RIGHT$(STR$(ndag), 1) ELSE ndag$ = RIGHT$(STR$(dag), 2)
    datum$ = nar$ + nman$ + ndag$
    AktDatum$ = datum$
REM    PRINT nar; nman; ndag; datum$
    
    x = 0
    GOSUB MTsum
    GOSUB Inlas
   
    uppDmin = VAL(MID$(TIME$, 4, 2))
    uppDtim = VAL(LEFT$(TIME$, 2))
    
REM    IF uppDtim = 19 AND uppDmin > 50 THEN tidflag = 0
REM    IF uppDtim = 19 AND uppDmin = 52 THEN RUN
    IF ndag = gdag THEN 10
    IF uppDtim = UppTim AND uppDmin = UppMin THEN 544

    GOTO 30
  
      
   
544 REM IF tidflag = 1 THEN GOTO 30
    time1$ = TIME$
REM    SHELL "net time \\nas02-lmhm /set /yes >nul"
    SHELL "net time \\lamet-nas /set /yes >nul"
    OPEN "uppdatPC.txt" FOR APPEND AS #4
    PRINT #4, datum$; " "; TIME$; "  "; time1$
    CLOSE #4
REM    sek11 = VAL(RIGHT$(TIME$, 2))
    
545 sek12 = VAL(RIGHT$(TIME$, 2)): IF sek12 = 45 THEN 545
REM    tidflag = 1
    CLS : RUN

       

30  IF ndag = gdag THEN 10
    gdag = ndag



112 FOR x = 1 TO 22
    ndag = ndag - 1
    IF ndag = 0 THEN nman = nman - 1: GOSUB NyMan
REM IF ndag = 0 THEN GOSUB NyMan
REM    IF nman = -1 THEN nar = ar - 1
REM IF nar = -1 THEN GOSUB Nyar
   
    IF nar < 10 THEN nar$ = "0" + RIGHT$(STR$(nar), 1) ELSE nar$ = RIGHT$(STR$(nar), 2)
    IF nman < 10 THEN nman$ = "0" + RIGHT$(STR$(nman), 1) ELSE nman$ = RIGHT$(STR$(nman), 2)
    IF ndag < 10 THEN ndag$ = "0" + RIGHT$(STR$(ndag), 1) ELSE ndag$ = RIGHT$(STR$(ndag), 2)
    datum$ = nar$ + nman$ + ndag$

REM     PRINT nar; nman; ndag; datum$
     GOSUB Inlas
    NEXT x

REM    LOCATE 10, 10: PRINT TIME$

    GOTO 10

NyMan:
    IF nman = 0 THEN nman = 12: ndag = 31: nar = ar - 1
    IF nman = 11 THEN ndag = 30
    IF nman = 10 THEN ndag = 31
    IF nman = 9 THEN ndag = 30
    IF nman = 8 THEN ndag = 31
    IF nman = 7 THEN ndag = 31
    IF nman = 6 THEN ndag = 30
    IF nman = 5 THEN ndag = 31
    IF nman = 4 THEN ndag = 30
    IF nman = 3 THEN ndag = 31
    IF nman = 2 THEN ndag = 28: IF INT(ar / 4) = ar / 4 THEN ndag = 29
    IF nman = 1 THEN ndag = 31
RETURN



Inlas:
    v$ = path$ + "DS" + datum$ + ".txt"
    n$ = path$ + "Dn" + datum$ + ".txt"
    MT$ = "MT" + datum$ + ".txt"

    OPEN v$ FOR INPUT ACCESS READ SHARED AS #1
    INPUT #1, vmax, t$
    tid$ = RIGHT$(t$, 5)
    LOCATE x + 3, 33: COLOR colv: PRINT USING "##.#"; vmax;
    LOCATE x + 3, 38: COLOR colt: PRINT tid$;
    INPUT #1, np, t$
    
    tid$ = RIGHT$(t$, 5)
    LOCATE x + 3, 45: COLOR colv: PRINT USING "####.#"; np;
    LOCATE x + 3, 52: COLOR colt: PRINT tid$;
    INPUT #1, xp, t$
    tid$ = RIGHT$(t$, 5)
    LOCATE x + 3, 58: COLOR colv: PRINT USING "####.#"; xp;
    LOCATE x + 3, 65: COLOR colt: PRINT tid$;
    INPUT #1, nt, t$
    tid$ = RIGHT$(t$, 5)
    LOCATE x + 3, 8: COLOR colv: PRINT USING "###.#"; nt;
    LOCATE x + 3, 14: COLOR colt: PRINT tid$;
    INPUT #1, xt, t$
    tid$ = RIGHT$(t$, 5)
    LOCATE x + 3, 20: COLOR colv: PRINT USING "###.#"; xt;
    LOCATE x + 3, 26: COLOR colt: PRINT tid$;
    CLOSE #1
    OPEN n$ FOR INPUT ACCESS READ SHARED AS #1
    INPUT #1, nbd
    LOCATE x + 3, 71: COLOR colv: PRINT USING "###"; nbd;
    IF nbd < .5 THEN LOCATE x + 3, 71: COLOR colv: PRINT " <1";
    IF nbd = 0 THEN LOCATE x + 3, 71: COLOR colv: PRINT "  -";

    CLOSE #1
    d$ = MID$(datum$, 3, 2) + "-" + RIGHT$(datum$, 2)
REM    IF x = 0 THEN d$ = "Idag"
REM    IF x = 1 THEN d$ = "Ig†r"

    COLOR 15
    IF x = 0 THEN 111

    OPEN MT$ FOR INPUT AS #4
    INPUT #4, MT
    CLOSE #4
    colx = 14
    IF MT <= 0 THEN colx = 11
    IF MT >= 10 THEN colx = 12
    COLOR colx

111 LOCATE x + 3, 2: PRINT d$; : LOCATE x + 3, 75: PRINT d$;
    IF x = 22 THEN varv = 1
REM    OPEN "klar20" FOR OUTPUT AS #4: CLOSE #4
    RETURN




Omstart:
    OPEN "20omst.txt" FOR APPEND AS #2
    PRINT #2, DATE$; TIME$
    CLOSE #2
    RUN


MTsum:
    
    VDin$ = path$ + "vdata.txt"
    OPEN VDin$ FOR INPUT ACCESS READ SHARED AS #4
    FOR j = 1 TO 10
    INPUT #4, jj
    NEXT j
    INPUT #4, t1i
    CLOSE #4

    MTsum$ = "mtsum.txt"
    OPEN MTsum$ FOR INPUT AS #3
    INPUT #3, Tsum, Trakn, LagratDatum$
    CLOSE #3

    IF AktDatum$ <> LagratDatum$ THEN 607

    Tsum = Tsum + t1i: Trakn = Trakn + 1

    OPEN MTsum$ FOR OUTPUT AS #3
    PRINT #3, Tsum, Trakn, LagratDatum$
    CLOSE #3
    RETURN

607 mtutfil$ = "MT" + LagratDatum$ + "txt"
    OPEN mtutfil$ FOR OUTPUT AS #2
    PRINT #2, USING "###.#"; Tsum / Trakn
    CLOSE #2
    Tsum = 0: Trakn = 0

    Tsum = Tsum + t1i: Trakn = Trakn + 1

    OPEN MTsum$ FOR OUTPUT AS #3
    PRINT #3, Tsum, Trakn, AktDatum$
    CLOSE #3
    RETURN



