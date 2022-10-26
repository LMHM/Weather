REM **********************************
REM *                                *
REM * Program f”r att visa v„derinfo *
REM * via TeamViewer                 *
REM * Uppstartsdatum och tid         *
REM * tas fr†n arkivfil              *
REM * Datum <10 med en siffra        *
REM *                                *
REM * Namn: TnSum_54.BAS             *
REM * Som 53 men kraftigare f„rg p†  *
REM * fjol†rets v„rden               *
REM *                                *
REM * Minst LMM_F472 k”rs i Milos200 *
REM *                                *
REM * Program: Bertil Larsson        *
REM * Uppdaterat: 2009-01-24         *
REM *                                *
REM * QBASIC 4.5                     *
REM *                                *
REM **********************************




    CLS
REM    SCREEN 12



    DIM nacc(12)
    DIM gacc(12)

    ON ERROR GOTO Omstart

    summapp$ = "v:\milog\" 'f”r andra datorer och V: „r d† e: p† VIDAR   
REM    summapp$ = "e:\milog\" 'mapp som inneh†ller as,mn.... m.m.

    infil$ = summapp$ + "uppdat.txt"
    OPEN infil$ FOR INPUT ACCESS READ SHARED AS #4
    INPUT #4, datum$
    INPUT #4, tid$
    CLOSE #4
    upptid$ = LEFT$(tid$, 5)
    gdatum$ = datum$


   
    COLOR 1
    LOCATE 24, 2: PRINT "LaMet2009";
    LOCATE 24, 19: COLOR 3: PRINT "-- LJUNGBYHEDS MILITŽRHISTORISKA MUSEUM --";
   
    COLOR 11
    LOCATE 2, 11
    PRINT "TempMin  TempMax   VindMax    TryckMin   TryckMax   Nbd  Acc"
    COLOR 3
    LOCATE 3, 11
    PRINT " "; CHR$(248); "C dat   "; CHR$(248); "C";
    PRINT ; " dat   m/s dat     hPa dat    hpa dat    mm   mm"



REM ============= H„r b”rjar loop =============


10  aktmin$ = MID$(TIME$, 4, 2)   'fr†n tid i datorn
    sek = VAL(RIGHT$(TIME$, 2))

    in$ = INKEY$
    IF in$ = "x" THEN CLOSE : END
    IF in$ = "X" THEN CLOSE : END

    tsek = 12 * INT(sek / 12)
    IF tsek <> sek THEN GOTO 10

REM    LOCATE 20, 75: PRINT sek

    IF varv = 0 THEN 20              'uppdatering direkt vid start
REM    IF sek <> 32 THEN uppdatflag = 0
REM    IF uppdatflag = 1 THEN 10
REM    IF sek = 32 THEN 20 ELSE 10

21  OPEN "uppdat.txt" FOR INPUT ACCESS READ SHARED AS #4
    INPUT #4, datum$
    INPUT #4, tid$
    CLOSE #4
    IF gtid$ = tid$ THEN GOTO 10
    gtid$ = tid$


20  infil$ = summapp$ + "uppdat.txt"
    OPEN infil$ FOR INPUT ACCESS READ SHARED AS #4
    INPUT #4, datum$
    INPUT #4, tid$
    CLOSE #4
    uppdatflag = 1: varv = 1
    upptid$ = LEFT$(tid$, 5)
  

REM    IF datum$ <> gdatum$ THEN RUN


REM ============ Utskrift av m†nadsv„rden f”r i †r ==============

   
    arv = 14: art = 8
    ar = VAL(LEFT$(datum$, 2))
    man = VAL(MID$(datum$, 4, 2))

    xman = man

    FOR x = 1 TO 12
    nacc(x) = 0
    gacc(x) = 0
    NEXT x

19  FOR x = 1 TO xman
    ar1 = ar
    COLOR arv
   
    IF ar1 < 10 THEN ar1$ = "0" + RIGHT$(STR$(ar1), 1) ELSE ar1$ = RIGHT$(STR$(ar1), 2)
    IF x < 10 THEN man$ = "0" + RIGHT$(STR$(x), 1) ELSE man$ = RIGHT$(STR$(x), 2)
    parman$ = ar1$ + "-" + man$
    IF x = 1 THEN parman$ = "Jan " + ar1$
    IF x = 2 THEN parman$ = "Feb " + ar1$
    IF x = 3 THEN parman$ = "Mar " + ar1$
    IF x = 4 THEN parman$ = "Apr " + ar1$
    IF x = 5 THEN parman$ = "Maj " + ar1$
    IF x = 6 THEN parman$ = "Jun " + ar1$
    IF x = 7 THEN parman$ = "Jul " + ar1$
    IF x = 8 THEN parman$ = "Aug " + ar1$
    IF x = 9 THEN parman$ = "Sep " + ar1$
    IF x = 10 THEN parman$ = "Okt " + ar1$
    IF x = 11 THEN parman$ = "Nov " + ar1$
    IF x = 12 THEN parman$ = "Dec " + ar1$

    filarman$ = ar1$ + man$
    LOCATE x + 4, 2: PRINT parman$
    infil$ = summapp$ + "ms" + filarman$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, mxHast, mHastT$, mnTryck, mnTryckT$, mxTryck, mxTryckT$
    INPUT #4, mnTemp, mnTempT$, mxTemp, mxTempT$
    CLOSE #4
    infil$ = summapp$ + "mN" + filarman$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, mannbd
    CLOSE #4

    nacc(x) = nacc(x - 1) + mannbd

    LOCATE x + 4, 10
    COLOR arv
    PRINT USING "###.#"; mnTemp;
    COLOR art: x$ = MID$(mnTempT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR art: PRINT " "; MID$(mnTempT$, 7, 2);

    COLOR arv: PRINT USING "####.#"; mxTemp;
    COLOR art: x$ = MID$(mxTempT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR art: PRINT " "; MID$(mxTempT$, 7, 2);

    COLOR arv: PRINT USING "#####.#"; mxHast;
    COLOR art: x$ = MID$(mHastT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR art: PRINT " "; MID$(mHastT$, 7, 2);

    COLOR arv: PRINT USING "#######.#"; mnTryck;
    COLOR art: x$ = MID$(mnTryckT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;


REM    COLOR art: PRINT " "; MID$(mnTryckT$, 7, 2);
    COLOR arv: PRINT USING "######.#"; mxTryck;
    COLOR art: x$ = MID$(mxTryckT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;
   
REM    COLOR art: PRINT " "; MID$(mxTryckT$, 7, 2);
    COLOR arv: PRINT USING "######"; mannbd;
    PRINT USING "#####"; nacc(x);
    COLOR arv: PRINT "   "; parman$;    'LEFT$(parman$, 3);




REM    LOCATE 22, 40: INPUT "", q$
27    NEXT x


REM ================ Utskrift av m†nadsv„rden f”r i fjol ==============

    garv = 10: gart = 8
    arg = ar - 1
29  FOR x = 1 TO 12
    ar1 = arg
    COLOR garv
  
    IF ar1 < 10 THEN ar1$ = "0" + RIGHT$(STR$(ar1), 1) ELSE ar1$ = RIGHT$(STR$(ar1), 2)
    IF x < 10 THEN man$ = "0" + RIGHT$(STR$(x), 1) ELSE man$ = RIGHT$(STR$(x), 2)
    parman$ = ar1$ + "-" + man$
    IF x = 1 THEN parman$ = "Jan " + ar1$
    IF x = 2 THEN parman$ = "Feb " + ar1$
    IF x = 3 THEN parman$ = "Mar " + ar1$
    IF x = 4 THEN parman$ = "Apr " + ar1$
    IF x = 5 THEN parman$ = "Maj " + ar1$
    IF x = 6 THEN parman$ = "Jun " + ar1$
    IF x = 7 THEN parman$ = "Jul " + ar1$
    IF x = 8 THEN parman$ = "Aug " + ar1$
    IF x = 9 THEN parman$ = "Sep " + ar1$
    IF x = 10 THEN parman$ = "Okt " + ar1$
    IF x = 11 THEN parman$ = "Nov " + ar1$
    IF x = 12 THEN parman$ = "Dec " + ar1$

    filarman$ = ar1$ + man$
    infil$ = summapp$ + "ms" + filarman$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, mxHast, mHastT$, mnTryck, mnTryckT$, mxTryck, mxTryckT$
    INPUT #4, mnTemp, mnTempT$, mxTemp, mxTempT$
    CLOSE #4
    infil$ = summapp$ + "mN" + filarman$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, mannbd
    CLOSE #4

    gacc(x) = gacc(x - 1) + mannbd

    IF x < xman THEN GOTO 37

   
    LOCATE x + 5, 2: PRINT parman$


    LOCATE x + 5, 10
    COLOR garv
    PRINT USING "###.#"; mnTemp;
    COLOR gart: x$ = MID$(mnTempT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR gart: PRINT " "; MID$(mnTempT$, 7, 2);
    COLOR garv: PRINT USING "####.#"; mxTemp;
    COLOR gart: x$ = MID$(mxTempT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR gart: PRINT " "; MID$(mxTempT$, 7, 2);
    COLOR garv: PRINT USING "#####.#"; mxHast;
    COLOR gart: x$ = MID$(mHastT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR gart: PRINT " "; MID$(mHastT$, 7, 2);
    COLOR garv: PRINT USING "#######.#"; mnTryck;
    COLOR gart: x$ = MID$(mnTryckT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR gart: PRINT " "; MID$(mnTryckT$, 7, 2);
    COLOR garv: PRINT USING "######.#"; mxTryck;
    COLOR gart: x$ = MID$(mxTryckT$, 7, 2)
    IF VAL(x$) < 10 THEN PRINT "  "; RIGHT$(x$, 1);  ELSE PRINT " "; x$;

REM    COLOR gart: PRINT " "; MID$(mxTryckT$, 7, 2);
    COLOR garv: PRINT USING "######"; mannbd;
    PRINT USING "#####"; gacc(x);
    COLOR garv: PRINT "   "; parman$;

                   

37    NEXT x

REM     LOCATE 22, 40: INPUT "", q$


REM ============= Utskrift av v„rden f”r ig†r ===============
    igarv = 14: igart = 8
    ar$ = LEFT$(datum$, 2): man$ = MID$(datum$, 4, 2)
    dag$ = RIGHT$(datum$, 2)
    ar = VAL(ar$): man = VAL(man$): dag = VAL(dag$)
    gdag = dag - 1: gar = ar: gman = man
    IF gdag = 0 AND man = 1 THEN gar = ar - 1: gman = 12: gdag = 31
    IF gdag = 0 AND man = 2 THEN gman = 1: gdag = 31
    IF gdag = 0 AND man = 3 THEN gman = 2: gdag = 28: IF ar / 4 = INT(ar / 4) THEN gdag = 29
    IF gdag = 0 AND man = 4 THEN gman = 3: gdag = 31
    IF gdag = 0 AND man = 5 THEN gman = 4: gdag = 30
    IF gdag = 0 AND man = 6 THEN gman = 5: gdag = 31
    IF gdag = 0 AND man = 7 THEN gman = 6: gdag = 30
    IF gdag = 0 AND man = 8 THEN gman = 7: gdag = 31
    IF gdag = 0 AND man = 9 THEN gman = 8: gdag = 31
    IF gdag = 0 AND man = 10 THEN gman = 9: gdag = 30
    IF gdag = 0 AND man = 11 THEN gman = 10: gdag = 31
    IF gdag = 0 AND man = 12 THEN gman = 11: gdag = 30
    IF gar < 10 THEN gar$ = "0" + RIGHT$(STR$(gar), 1) ELSE gar$ = RIGHT$(STR$(gar), 2)
    IF gman < 10 THEN gman$ = "0" + RIGHT$(STR$(gman), 1) ELSE gman$ = RIGHT$(STR$(gman), 2)
    IF gdag < 10 THEN gdag$ = "0" + RIGHT$(STR$(gdag), 1) ELSE gdag$ = RIGHT$(STR$(gdag), 2)
   
    infil$ = summapp$ + "ds" + gar$ + gman$ + gdag$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, dxHast, dHastT$, dnTryck, dnTryckT$, dxTryck, dxTryckT$
    INPUT #4, dnTemp, dnTempT$, dxTemp, dxTempT$
    CLOSE #4
    infil$ = summapp$ + "dn" + gar$ + gman$ + gdag$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, gdagnbd
    CLOSE #4

    COLOR igarv
    LOCATE 19, 4: PRINT "Ig†r"
    LOCATE 19, 10: PRINT USING "###.#"; dnTemp;
    LOCATE 19, 19: PRINT USING "###.#"; dxTemp;
    LOCATE 19, 30: PRINT USING "##.#"; dxHast;
    LOCATE 19, 40: PRINT USING "####.#"; dnTryck
    LOCATE 19, 51: PRINT USING "####.#"; dxTryck
    LOCATE 19, 58
REM IF gdagnbd > 0 AND gdagnbd < .5 THEN PRINT "  <1" ELSE PRINT USING "####"; gdagnbd
    PRINT USING "######.#"; gdagnbd;
    PRINT "        Ig†r";

    COLOR igart
    LOCATE 20, 6: PRINT "Kl"
    LOCATE 20, 10: PRINT RIGHT$(dnTempT$, 5)
    LOCATE 20, 19: PRINT RIGHT$(dxTempT$, 5)
    LOCATE 20, 29: PRINT RIGHT$(dHastT$, 5)
    LOCATE 20, 41: PRINT RIGHT$(dnTryckT$, 5)
    LOCATE 20, 52: PRINT RIGHT$(dxTryckT$, 5)
   




REM ======== Utskrift av v„rden f”r idag i 'klimatruta' =========

    idagv = 15: idagt = 8
    infil$ = summapp$ + "ds" + ar$ + man$ + dag$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, dxHast, dHastT$, dnTryck, dnTryckT$, dxTryck, dxTryckT$
    INPUT #4, dnTemp, dnTempT$, dxTemp, dxTempT$
    CLOSE #4
    infil$ = summapp$ + "dn" + ar$ + man$ + dag$ + ".txt"
    OPEN infil$ FOR INPUT AS #4
    INPUT #4, dagnbd
    CLOSE #4

    COLOR idagv
    LOCATE 21, 4: PRINT "Idag"
    LOCATE 21, 10: PRINT USING "###.#"; dnTemp;
    LOCATE 21, 19: PRINT USING "###.#"; dxTemp;
    LOCATE 21, 30: PRINT USING "##.#"; dxHast;
    LOCATE 21, 40: PRINT USING "####.#"; dnTryck
    LOCATE 21, 51: PRINT USING "####.#"; dxTryck
    LOCATE 21, 59
REM    IF dagnbd > 0 AND dagnbd < .5 THEN PRINT "  <1" ELSE PRINT USING "####"; dagnbd
    PRINT USING "#####.#"; dagnbd;
    PRINT "        Idag";

    COLOR idagt
    LOCATE 22, 6: PRINT "Kl"
    LOCATE 22, 10: PRINT RIGHT$(dnTempT$, 5)
    LOCATE 22, 19: PRINT RIGHT$(dxTempT$, 5)
    LOCATE 22, 29: PRINT RIGHT$(dHastT$, 5)
    LOCATE 22, 41: PRINT RIGHT$(dnTryckT$, 5)
    LOCATE 22, 52: PRINT RIGHT$(dxTryckT$, 5)

    LOCATE 23, 2:
    COLOR 8
    PRINT "------------------------------------------------------------------------------"
    LOCATE 18, 2:
    COLOR 8
    PRINT "------------------------------------------------------------------------------"

    gdag$ = dag$
REM    INPUT "", q$

    COLOR 6
    datum1$ = MID$(DATE$, 4, 2)
    man$ = LEFT$(DATE$, 2)
    IF man$ = "01" THEN man1$ = "Jan"
    IF man$ = "02" THEN man1$ = "Feb"
    IF man$ = "03" THEN man1$ = "Mar"
    IF man$ = "04" THEN man1$ = "Apr"
    IF man$ = "05" THEN man1$ = "Maj"
    IF man$ = "06" THEN man1$ = "Jun"
    IF man$ = "07" THEN man1$ = "Jul"
    IF man$ = "08" THEN man1$ = "Aug"
    IF man$ = "09" THEN man1$ = "Sep"
    IF man$ = "10" THEN man1$ = "Okt"
    IF man$ = "11" THEN man1$ = "Nov"
    IF man$ = "12" THEN man1$ = "Dec"

    LOCATE 24, 68: PRINT datum1$; " "; man1$; " "; upptid$;  'LEFT$(tid$, 5);

REM    LOCATE 24, 69: PRINT RIGHT$(datum$, 5); " "; upptid$;
    IF gdatum$ <> datum$ THEN RUN
    uppdatmin$ = MID$(TIME$, 4, 2)
    OPEN "klar" FOR OUTPUT AS #1
    CLOSE #1

    GOTO 10


Omstart:
    otime$ = TIME$
    OPEN "OmstSumm.txt" FOR APPEND AS #3
    PRINT #3, "TVSumm "; DATE$; " "; TIME$; "  err="; ERR; "  errl="; ERL
    CLOSE #3
151 IF TIME$ = otime$ THEN GOTO 151
    RUN

