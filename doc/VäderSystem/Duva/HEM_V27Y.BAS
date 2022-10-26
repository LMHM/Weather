
REM **********************************
REM *                                *
REM *  Ritar grafer i Vectra-maskin  *
REM *  f”r bild till hemsida         *
REM *  Mellanlagring lokalt f”r graf *
REM *                                *
REM *  Med dynamisk uppdatering      *
REM *  av tryck/temp                 *
REM *  med koll vindhast=32767       *
REM *  Gul f„rg graf n„r sikt<1km    *
REM *  Avrundning sikt som i QAM     *
REM *                                *
REM *  Ingen k”ldfaktor om vindhast= *
REM *  32767                         *
REM *  Flera alternativ vid Nbd-probl*
REM *                                *
REM *  5-graders uppl. tempgraf      *
REM *  5-hPa uppl. tryckgraf         *
REM *  Max 400 p† nbd-intens         *
REM *                                *
REM *  Dynamisk uppdatering av LV    *
REM *  Dynamisk uppdatring av KvnKorr*
REM *                                *
REM *  Programnamn  : Hem_V27y.BAS   *
REM *  Senast „ndrad: 22-01-17       *
REM *                                *
REM *  Programkod   : QBASIC 4.5     *
REM *  K„llkod      : Bertil Larsson *
REM *                                *
REM **********************************

	DEFDBL J        'f”r juliansk dag med m†nga decimaler
	DIM logv(20)
	CLS     'start „ven vid uppdatering var 3:e timme
	RelPlottFlag = 0

REM    rhkorr = -6     'ev. korrigering av rel fukt
REM    kvnkorr = -22   'ev. korrigering av kortv†g ner
	z9 = 3.14159 / 180 'Omvandling grader/radianer

REM    OPEN "kvnkorr.txt" FOR INPUT AS #1
REM    INPUT #1, kvnkorr
REM    CLOSE #1

	ramcol = 2      'f„rg p† ram diagramruta
	colbakgrund = 1 'f„rg p† bakgrung diagramruta
	colgrid = 3     'f„rg p† gridlinjer
	coltext = 2     'f„rg p† ledtext v„der

	colexp = 7      'f„rg p† f”rklaring (inom [..] p† bild)
	colval = 15     'f„rg p† v„rden i tabell
	vf = 15         'f„rg p† plottv„rden
	vx = 11         'f„rg p† vindmax
	vlvn = 12       'f„rg p† LV-srt†lning
	tidcol = 11     'f„rg p† tider
	coluppd = 7     'f„rg p† uppdatering av skalor
	colakt = 11     'f„rg p† aktuell skala


	JulianMinut = 1 / 60 / 24       '=1 min i dagformat med decimal

	uppDmin = 30    'vid minut = 30 uppdateras PC och MILOS-klockor

	 summapp$ = "C:\"  ' f”r DOSBox och URD
REM    summapp$ = "d:\lmm_vstn\" 'f”r Dakota

   
	SCREEN 12






REM ============== Best„mning/utskrift av tidsaxel i grafer ==================
	ON ERROR GOTO Omstart
	tidfil$ = summapp$ + "uppdat.txt"
	OPEN tidfil$ FOR INPUT ACCESS READ SHARED AS #1
	INPUT #1, datum2$
	INPUT #1, tid$
	CLOSE #1
	date1$ = RIGHT$(datum2$, 5) + "-20" + LEFT$(datum2$, 2)
	timex$ = tid$


	tim = VAL(LEFT$(timex$, 2))  ' datorns interntid; s„tts mot server 1g/tim


   
	COLOR tidcol
	FOR k = 0 TO 1
	IF k = 1 THEN r = 17
	LOCATE r + 1, 1
	c1 = 3 * INT(tim / 3) + 3: IF c1 > 23 THEN c1 = c1 - 24
	c3 = 0: REM IF c1 / 3 <> INT(c1 / 3) THEN c3 = 1
		FOR c = c1 TO 21 STEP 3
		c1$ = STR$(c)
		IF c < 10 THEN c$ = "0" + RIGHT$(c1$, 1) ELSE c$ = RIGHT$(c1$, 2)
		IF c = c1 THEN PRINT " ";  ELSE PRINT "   ";
		PRINT c$;
		NEXT c
   
		FOR c = c3 TO c1 STEP 3
		c1$ = STR$(c)
		IF c < 10 THEN c$ = "0" + RIGHT$(c1$, 1) ELSE c$ = RIGHT$(c1$, 2)
		PRINT "   "; c$;
		NEXT c
	NEXT k
REM ---- c1 blir b”rjan (och slutet) p† tidsskalan ------
	
REM    LOCATE 29, 43: PRINT "UTC";


REM ============= Rita diagramrutor V„der med gridn„t ======================

	xmin = 17: x0 = xmin         'v„nsterpos rutor;x0 f”r uppdat pr fr. -96
	xdel = 13.3: b1 = xdel       'l„ngd p† en timme;xdel fr†n -96
	xantal = 24                  '24 timmar p† xaxel
	xmax = xmin + xdel * xantal  'pos h”ger sida rutor
	ymin1 = 17: y0 = ymin1       'f”rsta rutans ”verdel;y0 fr†n -96
	ydel = 6.7                   '"tick" per ruta
	yantal = 4                   'antal ticks;bh=rutans h”jd fr†n -96
	ytotpix = ydel * yantal: bh = ytotpix 'totalh”jd ruta;bh fr†n -96
	yflytt = 32: ho = yflytt     'flytt f”r varje ny ruta;ho fr†n -96

	FOR varv = 0 TO 7             'totalt 8 rutor
	ymin = ymin1 + yflytt * varv    '”verdel varje ruta
	ymax = ymin + ydel * yantal     'nederdel varje ruta
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), colbakgrund, BF

		FOR y = 1 TO 3              'horisontella linjer i rutor
		ypos = ymin + ydel * y
		LINE (xmin, ypos)-(xmax, ypos), colgrid, , &HAAAA
		NEXT y

		FOR x = 1 TO 23             'vertikala linjer i rutor
		col = colgrid
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col, , &HAAAA
		NEXT x

		FOR x = 3 TO 21 STEP 3             'vertikala linjer i rutor
		col = colgrid
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col
		NEXT x


		LINE (xmin, ymin)-(xmax, ymax), colgrid, B
	NEXT varv




REM ======= Utskrift av ledtext f”r alfanumerisk presentation ===========
   
	   
	OPEN summapp$ + "tempmax" FOR INPUT AS #4
	INPUT #4, tempmax
	CLOSE #4

	OPEN summapp$ + "tryckmax" FOR INPUT AS #4
	INPUT #4, tryckmax
	CLOSE #4

	OPEN summapp$ + "LVmax" FOR INPUT AS #4
	INPUT #4, LVmax
	CLOSE #4

	xp = 45      'Position (kolumn) b”rjan text
	COLOR 1
	
REM    LINE (553, 12)-(635, 48), 1, B
REM    LINE (554, 49)-(636, 49), 1
REM    LINE (555, 50)-(637, 50), 1
REM    LINE (636, 13)-(636, 49), 1
REM    LINE (637, 14)-(637, 50), 1
   
	LINE (546, 12)-(635, 48), 6, B
	LINE (547, 49)-(636, 49), 6
	LINE (548, 50)-(637, 50), 6
	LINE (636, 13)-(636, 49), 6
	LINE (637, 14)-(637, 50), 6

REM    LOCATE 2, xp + 26: PRINT "LMM-V„der";
REM    LOCATE 3, xp + 26: PRINT "LaMet2007";

	COLOR coltext: LOCATE 2, xp: PRINT "Vindriktning";
	LOCATE 2, xp + 18: PRINT "gr": REM PRINT CHR$(248);
	COLOR colexp: LOCATE 3, xp: PRINT "[0-360]"
	COLOR coltext: LOCATE 4, xp: PRINT "Vindhast Med      m/s"
	COLOR colexp: LOCATE 5, xp: PRINT "[0-20]";
	COLOR coltext: PRINT "   Max      m/s"; : COLOR colexp: PRINT " [2min]"

	COLOR coltext: LOCATE 6, xp: PRINT "Tryck(QFF)        hPa      hPa/3tim"
	COLOR colexp: LOCATE 7, xp
	IF tryckmax = 980 THEN PRINT "[960-980] ";
	IF tryckmax = 985 THEN PRINT "[965-985] ";
	IF tryckmax = 990 THEN PRINT "[970-990] ";
	IF tryckmax = 995 THEN PRINT "[975-995] ";
	IF tryckmax = 1000 THEN PRINT "[980-1000] ";
	IF tryckmax = 1005 THEN PRINT "[985-1005] ";
	IF tryckmax = 1010 THEN PRINT "[990-1010] ";
	IF tryckmax = 1015 THEN PRINT "[995-1015] ";
	IF tryckmax = 1020 THEN PRINT "[1000-1020]";
	IF tryckmax = 1025 THEN PRINT "[1005-1025]";
	IF tryckmax = 1030 THEN PRINT "[1010-1030]";
	IF tryckmax = 1035 THEN PRINT "[1015-1035]";
	IF tryckmax = 1040 THEN PRINT "[1020-1040] ";
	IF tryckmax = 1045 THEN PRINT "[1025-1045] ";
	IF tryckmax = 1050 THEN PRINT "[1030-1050] ";


	COLOR coltext: LOCATE 7, xp + 29: PRINT "Mark"

	COLOR coltext: LOCATE 8, xp: PRINT "Temperatur       "; : PRINT CHR$(248); "C";
	LOCATE 8, xp + 33: PRINT CHR$(248); "C";

	COLOR colexp: LOCATE 9, xp
	IF tempmax = -20 THEN PRINT "[-40--20]"
	IF tempmax = -15 THEN PRINT "[-35--15]"
	IF tempmax = -10 THEN PRINT "[-30--10]"
	IF tempmax = -5 THEN PRINT "[-25--5] "
	IF tempmax = 0 THEN PRINT "[-20-0]  "
	IF tempmax = 5 THEN PRINT "[-15-+5] "
	IF tempmax = 10 THEN PRINT "[-10-+10] "
	IF tempmax = 15 THEN PRINT "[-5-+15]  "
	IF tempmax = 20 THEN PRINT "[0-+20] "
	IF tempmax = 25 THEN PRINT "[+5-+25]  "
	IF tempmax = 30 THEN PRINT "[+10-+30] "
	IF tempmax = 35 THEN PRINT "[+15-+35] "
	IF tempmax = 40 THEN PRINT "[+20-+40]"

	COLOR 4: LOCATE 17, xp + 23
	IF LVmax = 600 THEN PRINT "[500-600]"
	IF LVmax = 575 THEN PRINT "[475-575]"
	IF LVmax = 550 THEN PRINT "[450-550]"
	IF LVmax = 525 THEN PRINT "[425-525]"
	IF LVmax = 500 THEN PRINT "[400-500]"
	IF LVmax = 475 THEN PRINT "[375-475]"
	IF LVmax = 450 THEN PRINT "[350-450]"
	IF LVmax = 425 THEN PRINT "[325-425]"
	IF LVmax = 400 THEN PRINT "[300-400]"
	IF LVmax = 375 THEN PRINT "[275-375]"
	IF LVmax = 350 THEN PRINT "[250-350]"
	IF LVmax = 325 THEN PRINT "[225-325]"
	IF LVmax = 300 THEN PRINT "[200-300]"
	IF LVmax = 275 THEN PRINT "[175-275]"
	IF LVmax = 250 THEN PRINT "[150-250]"



	COLOR coltext: LOCATE 10, xp: PRINT "Rel.Fuktighet     %"
	COLOR colexp: LOCATE 11, xp: PRINT "[20-100]"
	COLOR coltext: LOCATE 11, xp + 10: PRINT "Dp     "; : PRINT CHR$(248); "C"
	COLOR coltext: LOCATE 12, xp: PRINT "Nederb.intens     p"
	COLOR colexp: LOCATE 13, xp: PRINT "[0-400]"
	COLOR coltext: LOCATE 14, xp: PRINT "Sikt              km"
	COLOR colexp: LOCATE 15, xp: PRINT "[0-20km]"
	COLOR coltext: LOCATE 16, xp: PRINT "Instr†ln. KV      W/m"; CHR$(253); ""
	COLOR coltext: LOCATE 17, xp: PRINT "Instr†ln. LV      W/m"; CHR$(253); ""
	COLOR colexp: LOCATE 16, xp + 23: PRINT "[0-1000]"
REM    COLOR 6: LOCATE 18, xp: PRINT "[200-600]"

REM *** Nederb”rdsm„tare OK=0, U/S=1, Intens st”rd av spindelv„v = 2 ***

	OPEN "nbdmat.txt" FOR INPUT AS #4
	INPUT #4, nbdmat
	IF nbdmat = 99 THEN LINE INPUT #4, U$
	CLOSE #4

	COLOR 5: LOCATE 18, xp

	IF nbdmat = 0 THEN PRINT "                               "
	IF nbdmat = 1 THEN PRINT "Nederb”rdsm„ngd os„ker         "
	IF nbdmat = 2 THEN PRINT "Nbd.intens. st”rd av spindelv„v"
	IF nbdmat = 3 THEN PRINT "Nederb”rdsm„tare ur funktion   "
	IF nbdmat = 4 THEN PRINT "Nederb”rdsm„ngd os„ker. LV U/S "
	IF nbdmat = 5 THEN PRINT "Nederb”rdsm„ngd fel. LV U/S    "
	IF nbdmat = 6 THEN PRINT "Nederb”rdsm„ngd fel.           "
	IF nbdmat = 99 THEN PRINT U$
	

	COLOR 2: LOCATE 10, xp + 24: PRINT "Nederb”rd"
	LOCATE 11, xp + 22: PRINT " 3tim      mm"
	LOCATE 12, xp + 22: PRINT " 6tim      mm"
	LOCATE 13, xp + 22: PRINT "12tim      mm"
	LOCATE 14, xp + 22: PRINT "24tim      mm"
	LINE (522, 140)-(634, 224), 2, B
	LINE (523, 225)-(635, 225), 2
	LINE (635, 141)-(635, 225), 2
	LINE (524, 226)-(636, 226), 2
	LINE (636, 142)-(636, 226), 2

	COLOR 6
REM    LINE (548, 237)-(634, 288), 6, B ' Ruta f”r Uppdateradinfo
REM    LINE (549, 289)-(635, 289), 6
REM    LINE (550, 290)-(636, 290), 6
REM    LINE (635, 238)-(635, 289), 6
REM    LINE (636, 239)-(636, 290), 6
REM LINE (377, 272)-(635, 288), 6, B





REM ===== Rubrik i rutor f”r grafer ====

	
	COLOR coltext
	LOCATE 2, 5: PRINT "Riktn"
	LOCATE 4, 5: PRINT "Hast"
	LOCATE 6, 5: PRINT "Tryck"
	LOCATE 8, 5: PRINT "Temp"
	LOCATE 10, 5: PRINT "RH"
	LOCATE 12, 5: PRINT "Nbd"
	LOCATE 14, 5: PRINT "Sikt"

	COLOR 15
	LOCATE 16, 5: PRINT "KV"
	COLOR 12
	LOCATE 16, 7: PRINT "LV"


	FOR varv = 0 TO 7              'totalt 8 rutor
	ymin = ymin1 + yflytt * varv    '”verdel varje ruta
	ymax = ymin + ydel * yantal     'nederdel varje ruta
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), ramcol, B
	NEXT varv




REM ====== Uppdatering av filer f”r Plott av "Gamla" data =========
REM ------ Datum och tid till filer tas t.v. fr†n lokal datorn ----





	man$ = LEFT$(date1$, 2): dag$ = MID$(date1$, 4, 2): ar$ = RIGHT$(date1$, 2)
	NyFilDatum$ = ar$ + man$ + dag$ + ".txt"
	ar = VAL(ar$): man = VAL(man$): dag = VAL(dag$)
	gdag = dag - 1
	gar = ar: gman = man

REM ------ Ber„kning av g†rdagens datum -----------
	IF gdag > 0 THEN 44         '=inget m†nadsskifte
	ON man GOTO 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30
0   gar = ar - 1: gman = 12: gdag = 31: GOTO 44
28  gman = man - 1: gdag = 28: IF INT(ar / 4) * 4 = ar THEN gdag = 29
	GOTO 44
31  gman = man - 1: gdag = 31: GOTO 44
30  gman = man - 1: gdag = 30: GOTO 44
44  gar$ = STR$(gar)
	gman$ = STR$(gman)
	gdag$ = STR$(gdag)
	IF gar < 10 THEN gar$ = "0" + RIGHT$(gar$, 1) ELSE gar$ = RIGHT$(gar$, 2)
	IF gman < 10 THEN gman$ = "0" + RIGHT$(gman$, 1) ELSE gman$ = RIGHT$(gman$, 2)
	IF gdag < 10 THEN gdag$ = "0" + RIGHT$(gdag$, 1) ELSE gdag$ = RIGHT$(gdag$, 2)
   
	GFilDatum$ = gar$ + gman$ + gdag$ + ".txt"  'f”reg†ende dag


REM ========== Plotting av 'gamla' data =========================
   
	GOSUB GammalMilog
	

REM *******************************************************************
REM ****** Start av loop f”r l„sning av 'senaste'-datafiler ***********
REM *******************************************************************

REM -- Tid fr†n datorn; rem uppdatering mot server vid 'minuppd' 1ggr/tim -
   
	RelPlottFlag = 1
REM 10  REM

REM ============= H„r b”rjar loop =============


10  aktmin$ = MID$(TIME$, 4, 2)   'fr†n tid i datorn
	sek = VAL(RIGHT$(TIME$, 2))
	
REM    LOCATE 12, 10: PRINT sek
REM    COLOR 1: LOCATE 24, 1: PRINT " ";
REM    LOCATE 10, 10: PRINT TIME$
REM    COLOR 2: LOCATE 10, 10: PRINT sek: INPUT "", q$
	in$ = INKEY$
	IF in$ = "x" THEN CLOSE : END
	IF in$ = "X" THEN CLOSE : END
	IF varvx = 0 THEN 698   'Uppdatering direkt vid start
	tsek = 12 * INT(sek / 12)
	IF tsek <> sek THEN GOTO 10

REM    IF sek <> 30 THEN uppdatflag = 0    'uppdatering vid sek 30
REM    IF uppdatflag = 1 THEN 10           'bara en uppdatering vid sek=30
REM    IF sek = 30 THEN 698 ELSE 10         'uppdatering  sek=30

	ON ERROR GOTO Omstart

	OPEN "uppdat.txt" FOR INPUT ACCESS READ SHARED AS #1
	INPUT #1, datum$
	INPUT #1, tid$
	CLOSE #1
	IF tid$ = gtid$ THEN 10



REM    IF uppdatflag = 0 THEN 699

REM    GOSUB LasningMiLog
698    GOSUB LasningVdata
	   varvx = 1

699 sek = VAL(RIGHT$(tid$, 2))
	min = VAL(MID$(tid$, 4, 2))
	tim = VAL(LEFT$(tid$, 2))  'koll om ny uppdatering av tidsskala
	c4 = 3 * INT(tim / 3)       '
	IF c1 = c4 THEN RUN        'c1=tidsskalans b”rjan (slut)
	gtid$ = tid$



510 TryckUppd = 0
	IF qff < 960 OR qff > 1050 THEN 520
	IF qff > tryckmax THEN tryckmax = tryckmax + 5: TryckUppd = 1
	IF qff < tryckmax - 20 THEN tryckmax = tryckmax - 5: TryckUppd = 1
	IF TryckUppd = 1 THEN GOSUB UppdateraTryck
	IF TryckUppd = 0 THEN 520 ELSE 510

520 TempUppd = 0
	IF ABS(t1i) > 50 THEN 530
	IF t1i > tempmax THEN tempmax = tempmax + 5: TempUppd = 1
	IF t1i < tempmax - 20 THEN tempmax = tempmax - 5: TempUppd = 1
	IF TempUppd = 1 THEN GOSUB UppdateraTemp
	IF TempUppd = 0 THEN 530 ELSE 520

530 LVUppd = 0
	IF lvn < 0 OR lvn > 1000 THEN 15
	IF lvn > LVmax THEN LVmax = LVmax + 25: LVUppd = 1
	IF lvn < LVmax - 100 THEN LVmax = LVmax - 25: LVUppd = 1

	IF LVUppd = 1 THEN GOSUB UppdateraKVLV

	IF LVUppd = 0 THEN 15 ELSE 530


			
REM ======== Bearbetning/Mellanlagring av MILOG-data ============

15  REM 'CLOSE : OPEN "c:\senaste\senmilog" FOR INPUT ACCESS READ SHARED AS #1

	reltim = tim
	tim1$ = STR$(tim)
	IF tim < 10 THEN tim$ = "0" + RIGHT$(tim1$, 1) ELSE tim$ = RIGHT$(tim1$, 2)
	min1$ = STR$(min)
	IF min < 10 THEN min$ = "0" + RIGHT$(min1$, 1) ELSE min$ = RIGHT$(min1$, 2)
	milogtid$ = tim$ + ":" + min$ + ":00"   'f”r  "Latest Update"





	
	COLOR colval

	GOSUB Vindriktning
REM    LOCATE 2, xp + 14
REM    IF da2 < 361 THEN PRINT USING "###"; da2 ELSE PRINT "---"
	LOCATE 4, xp + 13
   
	IF fa2 < 100 THEN PRINT USING "##.#"; fa2 ELSE PRINT " ---"
	LOCATE 5, xp + 13: COLOR vx
	IF fx2 < 100 THEN PRINT USING "##.#"; fx2 ELSE PRINT " ---"
	LOCATE 6, xp + 11: COLOR colval
	IF qff < 2000 THEN PRINT USING "####.#"; qff ELSE PRINT "   ---"
	LOCATE 6, xp + 21
	IF p3 < 100 THEN PRINT USING "+##.#"; p3 ELSE PRINT "  ---"
	GOTO 688
	IF p3 > 100 THEN 688
	IF p3 >= 10 THEN LOCATE 6, xp + 21: PRINT "+"; : GOTO 688
	IF p3 > 0 THEN LOCATE 6, xp + 22: PRINT "+"; : GOTO 688

688 LOCATE 8, xp + 12
	IF ABS(t1i) < 100 THEN PRINT USING "###.#"; t1i ELSE PRINT "  ---"
	LOCATE 8, xp + 28
	IF ABS(t3i) < 100 THEN PRINT USING "###.#"; t3i ELSE PRINT "  ---"
REM    LOCATE 8, xp + 30
REM    IF ABS(t2i) < 100 THEN PRINT USING "###.#"; t2i ELSE PRINT "  ---"
	LOCATE 7, xp + 21: PRINT "      ";
	LOCATE 8, xp + 20: PRINT "       ";
	IF ABS(wc) >= 100 THEN 258
	IF fa2 < 2 OR fa2 > 50 THEN 258
	IF t1i > 8 THEN 258
	LOCATE 7, xp + 21: COLOR 11: PRINT "K”ldf.";
	LOCATE 8, xp + 20: PRINT USING "###.#"; wc;
	COLOR coltext:  PRINT CHR$(248); "C";
	COLOR colval
   
	
   
258 LOCATE 10, xp + 14
REM rh= 32767
	IF rh > 102 OR rh < 0 THEN PRINT "---";  ELSE PRINT USING "###"; rh;
	LOCATE 11, xp + 12
	dpdiff = t1i - td
REM dpdiff = 40
	IF dpdiff < 25 AND td < 40 THEN PRINT USING "###.#"; td ELSE PRINT "  ---";
	LOCATE 12, xp + 13
	IF d3 < 10000 THEN PRINT USING "####"; d3 ELSE PRINT " ---"
REM -- d3 = antal partiklar; fr†n Friedrich r„knare --
	 
		
	LOCATE 16, xp + 13
	IF kvn < 0 THEN kvn = 0
	IF kvn < 1100 THEN PRINT USING "####"; kvn ELSE PRINT " ---"
	LOCATE 17, xp + 13
	IF lvn < 0 THEN lvn = 0
	IF lvn < 1000 THEN PRINT USING "####"; lvn ELSE PRINT " ---"

	
REM    sikt3 = (sikt1 + sikt2) / 2: sikt4 = sikt3
REM IF sikt3 <= 20 THEN 197
REM sikt3 = 5 * INT((sikt3 + 2.5) / 5) 'avrundning till 5km ”ver 20 km
197 REM

	LOCATE 14, xp + 13
		COLOR colval
		vis$ = " km"
		IF sikt3 >= 10 THEN vis1 = 5 * INT((sikt3 + 2.5) / 5)
		IF sikt3 < 10 THEN vis1 = INT(sikt3 + .5)
		IF sikt3 < 5 THEN vis1 = 100 * CINT(10 * sikt3): vis$ = " m "
		IF sikt3 < 1 THEN vis2 = 1000 * sikt3 + 25: vis1 = 50 * INT(vis2 / 50)
		IF sikt3 < 100 THEN PRINT USING "####"; vis1;  ELSE PRINT " ---";
		COLOR coltext: PRINT vis$;

REM    IF sikt3 > 100 THEN LOCATE 14, xp + 13: PRINT "  ---"; : GOTO 198
REM    IF sikt3 = 0 THEN PRINT "  ---"; : GOTO 198
REM    IF sikt3 < 5 THEN PRINT USING "###.#"; sikt3 ELSE PRINT USING "#####"; sikt3;
198 REM

REM    ******** Utskrift av nederb”rdsm„ngd ********
	COLOR colval
	LOCATE 11, xp + 28: PRINT USING "##.#"; nbd3
	LOCATE 12, xp + 28: PRINT USING "##.#"; nbd6
	LOCATE 13, xp + 28: PRINT USING "##.#"; nbd12
	LOCATE 14, xp + 28: PRINT USING "##.#"; nbd24

	LOCATE 13, xp + 9: PRINT "           ";
	IF nbdintens < 4 THEN GOTO 123
	IF t1i < 1 THEN GOTO 123
752 COLOR 11: LOCATE 13, xp + 9: PRINT "/tim     mm";
	COLOR 11: LOCATE 13, xp + 14: PRINT USING "###"; nbdintens;
	COLOR colval
	
   

123 GOSUB PlottMilog

	   
REM ------- Mellanlagring f”r att kunna generera "gammal" grafik --------
	IF uppdatflag = 0 THEN 100
	ar$ = RIGHT$(date1$, 2): man$ = LEFT$(date1$, 2): dag$ = MID$(date1$, 4, 2)
	milogfil$ = summapp$ + "mellan\mm" + ar$ + man$ + dag$ + ".txt"
	OPEN milogfil$ FOR APPEND AS #4
	PRINT #4, tim; min; fa2; da2; fx2; qff; p3; rh; t1i; td;
	PRINT #4, kvn; lvn; d3; sikt1; sikt2
	CLOSE #4
	mlagratmin = min

100 REM

	GOSUB Klockor

	COLOR 6: REM LOCATE 19, xp + 4: PRINT "Uppdaterad:"
REM    datum$ = RIGHT$(date1$, 2) + "-" + LEFT$(date1$, 5)
datum1$ = "20" + datum$
	LOCATE 2, xp + 25: PRINT datum1$;
	LOCATE 3, xp + 26: PRINT tid$;
REM    LINE (377, 272)-(635, 288), 6, B
	netdatum$ = datum$
	nettimex$ = timex$

	GOSUB SummaUppdat
	GOSUB NetUppdat
	
276 REM

	numin = VAL(MID$(timex$, 4, 2))
	nutim = VAL(LEFT$(timex$, 2))
	IF nutim = 0 AND numin = uppDmin THEN 179
	IF nutim = 6 AND numin = uppDmin THEN 179
	IF nutim = 2 AND numin = 0 THEN 179
	IF nutim = 3 AND numin = 0 THEN 179
	IF nutim = 12 AND numin = uppDmin THEN 179
	IF nutim = 18 AND numin = uppDmin THEN 179
	GOTO 278


179 IF MilosUppdFlag = 1 THEN 278
	GOSUB UppdateraPC
278 REM
	IF numin <> uppDmin THEN MilosUppdFlag = 0


REM    GOSUB VWS
REM    GOSUB DagNbd
	
	OPEN "klar" FOR OUTPUT AS #1
	CLOSE #1


	GOTO 10    'startar ny l„speriod






REM *********************************************************************
REM ********** L„sning/Plottning av 'gamla' data - SUBRUTINER ***********
REM *********************************************************************




REM ========== L„sning och Plott av gamla MILOG-data fr†n fil(er) =========

GammalMilog:
	
	ON ERROR GOTO Error801
	IF c1 = 0 THEN 801              'om tidaxel b”rjar 00
	GMilogFil$ = summapp$ + "mellan\mm" + GFilDatum$
	OPEN GMilogFil$ FOR INPUT AS #4
800 IF EOF(4) THEN CLOSE #4: GOTO 801
	INPUT #4, tim, min, fa2, da2, fx2, qff, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	sikt3 = (sikt1 + sikt2) / 2
	IF tim < c1 THEN 800
	GOSUB PlottMilog
	GOTO 800
 
801 ON ERROR GOTO Error803

	NyMilogFil$ = summapp$ + "mellan\mm" + NyFilDatum$
	OPEN NyMilogFil$ FOR INPUT AS #4
802 IF EOF(4) THEN CLOSE #4: GOTO 803
	INPUT #4, tim, min, fa2, da2, fx2, qff, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	sikt3 = (sikt1 + sikt2) / 2
REM    LOCATE 10, 10: PRINT tim; min: INPUT "", q$
	GOSUB PlottMilog

	GOTO 802

	

803 RETURN




	   


REM ******************** PLOTTNING - SUBRUTINER *********************

REM ============= Plottning av MILOG-data  (gamla/nya) ==============
REM ----- d=diagramruta-1 uppifr†n r„knat; g„ller alla grafet ------

PlottMilog:

	dtid = tim + min / 60
	xdt = dtid - c1: IF xdt < 0 THEN xdt = 24 + xdt
	ptid = x0 + xdt * b1          'plats p† tidaxeln i pixel

	IF da2 > 360 THEN 701    'vindriktning
	d = 0: ypos = y0 + d * ho + bh * (360 - da2) / 360: PSET (ptid, ypos), vf

701 IF fa2 > 100 THEN 702    'hast medel
	qq = fa2
	IF qq > 20 THEN qq = 20
	d = 1: ypos = y0 + d * ho + bh * (20 - qq) / 20: PSET (ptid, ypos), vf

702 IF fx2 > 100 THEN 703    'hast max
	qq = fx2
	IF qq > 20 THEN qq = 20
	d = 1: ypos = y0 + d * ho + bh * (20 - qq) / 20: PSET (ptid, ypos), vx

703 IF p > 2000 THEN 704     'tryck
	qq = qff
	IF qq < tryckmax - 20 THEN qq = tryckmax - 20
	IF qq > tryckmax THEN qq = tryckmax
	d = 2: ypos = y0 + d * ho + bh * (tryckmax - qq) / 20: PSET (ptid, ypos), vf

704 IF t1i > 100 THEN 705      'temp;ex saknas  (32767)
	qq = t1i
	IF qq > tempmax THEN qq = tempmax
	IF qq < tempmax - 20 THEN qq = tempmax - 20
	d = 3: ypos = y0 + d * ho + bh * (tempmax - qq) / 20: PSET (ptid, ypos), vf

705 IF rh > 110 THEN 706    'rel fukt
	qq = rh
	IF qq < 20 THEN 706
	IF qq > 105 THEN 706
	IF qq > 100 AND qq < 105 THEN qq = 100
	d = 4: ypos = y0 + d * ho + bh * (100 - qq) / 80: PSET (ptid, ypos), vf

706 IF sikt3 > 100 THEN GOTO 710
	IF sikt3 > 20 THEN sikt3 = 20
	IF sikt3 > 100 OR sikt3 = 0 THEN 710
	IF sikt3 < 1 THEN vff = 14 ELSE vff = vf
	d = 6: ypos = y0 + d * ho + bh * (20 - sikt3) / 20:  PSET (ptid, ypos), vff
REM    ypos1 = y0 + d * ho: LINE (ptid, ypos1)-(ptid, ypos), vf  'i linjalen

710 IF kvn > 1000 THEN 711      'kortv†g
	REM IF kvn < 0 THEN 711
	IF kvn < 0 THEN kvn = 0
	qq = kvn
	IF qq > 1000 THEN qq = 1000
	d = 7: ypos = y0 + d * ho + bh * (1000 - qq) / 1000: PSET (ptid, ypos), vf

711 IF lvn > 1000 THEN 712      'l†ngv†g
	IF lvn < 0 THEN 712
	qq = lvn
	IF qq > LVmax THEN qq = LVmax
	IF qq < LVmax - 100 THEN qq = LVmax - 100
	d = 7: ypos = y0 + d * ho + bh * (LVmax - qq) / 100: PSET (ptid, ypos), vlvn

712 nbd = d3     'kan „ndras beroende p† vilken digitalkanal p† TheFri-logger
	IF nbd > 1000 THEN 713
	IF nbd > 400 THEN nbd = 400
	IF nbd < 7 THEN 713
	d = 5
	ypos = y0 + d * ho + bh * (400 - nbd) / 400: REM PSET (ptid, ypos), vf
	ypos1 = y0 + d * ho + bh: LINE (ptid, ypos1)-(ptid, ypos), vf 'i linjalen

713 RETURN





REM *************** Felhanteringsrutiner ***********************
   

Error171:
	RESUME 171

Error172:
	RESUME 172

ERROR276:
	RESUME 276

ERROR278:
	RESUME 278


ERROR333:
	RESUME 333

ERROR334:
	RESUME 334

Error471:
	RESUME 471

Error472:
   
	RESUME 472

Error571:
	RESUME 571

Error572:
  
	RESUME 572


Error499:
	RESUME 499

ERROR502:
	RESUME 502

Error801:
	RESUME 801

Error803:
	RESUME 803







UppdateraTid:        'kan bli aktuellt igen om server med bra klocka(GPS)






REM *** Ber„kning av juliansk tid  - dag med decimaler ***

julian:
	jar = VAL(RIGHT$(date1$, 2))
	jman = VAL(LEFT$(date1$, 2))
	jdag = VAL(MID$(date1$, 4, 2))
	ardag = 365: dagadd = 0
	IF INT(jar / 4) * 4 = jar THEN ardag = 366: dagadd = 1  'skott†r
	dagtot = (jar - 2) * ardag
	IF jman = 1 THEN juldag = dagtot + jdag
	IF jman = 2 THEN juldag = dagtot + jdag + 31
	IF jman = 3 THEN juldag = dagtot + jdag + 59 + dagadd
	IF jman = 4 THEN juldag = dagtot + jdag + 90 + dagadd
	IF jman = 5 THEN juldag = dagtot + jdag + 120 + dagadd
	IF jman = 6 THEN juldag = dagtot + jdag + 151 + dagadd
	IF jman = 7 THEN juldag = dagtot + jdag + 181 + dagadd
	IF jman = 8 THEN juldag = dagtot + jdag + 212 + dagadd
	IF jman = 9 THEN juldag = dagtot + jdag + 243 + dagadd
	IF jman = 10 THEN juldag = dagtot + jdag + 273 + dagadd
	IF jman = 11 THEN juldag = dagtot + jdag + 303 + dagadd
	IF jman = 12 THEN juldag = dagtot + jdag + 334 + dagadd
	jtim = VAL(LEFT$(timex$, 2))
	jmin = VAL(MID$(timex$, 4, 2))
	jsek = VAL(RIGHT$(timex$, 2))
	jtidtottim = jtim + jmin / 60 + jsek / 3600
	juliandag = juldag + jtidtottim / 24

	RETURN



UppdateraTemp:

	OPEN "tempmax" FOR OUTPUT AS #4
	PRINT #4, tempmax
	CLOSE #4
	varv = 3            'totalt 13 rutor
	ymin = ymin1 + yflytt * varv    '”verdel varje ruta
	ymax = ymin + ydel * yantal     'nederdel varje ruta
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), colbakgrund, BF

		FOR y = 1 TO 3              'horisontella linjer i rutor
		ypos = ymin + ydel * y
		LINE (xmin, ypos)-(xmax, ypos), colgrid, , &HAAAA
		NEXT y

		FOR x = 1 TO 23             'vertikala linjer i rutor
		col = colgrid
REM        IF INT(x / 3) = x / 3 THEN col = 7  'var tredje timme lite kraftigare
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col, , &HAAAA
		NEXT x

		FOR x = 3 TO 21 STEP 3             'vertikala linjer i rutor
		col = colgrid
REM        IF INT(x / 3) = x / 3 THEN col = 7  'var tredje timme lite kraftigare
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col
		NEXT x
	  
		LINE (xmin, ymin)-(xmax, ymax), colgrid, B
   

	COLOR coltext
	LOCATE 8, 5: PRINT "Temp"
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), ramcol, B

REM    COLOR coluppd: LOCATE 9, 1: PRINT "QWER";
	 COLOR colexp: LOCATE 9, xp
	IF tempmax = -20 THEN PRINT "[-40--20]"
	IF tempmax = -15 THEN PRINT "[-35--15]"
	IF tempmax = -10 THEN PRINT "[-30--10]"
	IF tempmax = -5 THEN PRINT "[-25--5] "
	IF tempmax = 0 THEN PRINT "[-20-0]  "
	IF tempmax = 5 THEN PRINT "[-15-+5] "
	IF tempmax = 10 THEN PRINT "[-10-+10] "
	IF tempmax = 15 THEN PRINT "[-5-+15]  "
	IF tempmax = 20 THEN PRINT "[0-+20] "
	IF tempmax = 25 THEN PRINT "[+5-+25]  "
	IF tempmax = 30 THEN PRINT "[+10-+30] "
	IF tempmax = 35 THEN PRINT "[+15-+35] "
	IF tempmax = 40 THEN PRINT "[+20-+40]"
   
   
	ON ERROR GOTO Error171
	IF c1 = 0 THEN GOTO 171
	GMilogFil$ = summapp$ + "mellan\mm" + GFilDatum$
	OPEN GMilogFil$ FOR INPUT AS #4
170 IF EOF(4) THEN CLOSE #4: GOTO 171
	INPUT #4, tim, min, fa2, da2, fx2, p, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	IF tim < c1 THEN 170
	GOSUB PlottTemp
	GOTO 170
   

171 NyMilogFil$ = summapp$ + "mellan\mm" + NyFilDatum$
	ON ERROR GOTO Error172
	OPEN NyMilogFil$ FOR INPUT AS #4
172 IF EOF(4) THEN CLOSE #4: RETURN
	INPUT #4, tim, min, fa2, da2, fx2, p, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	GOSUB PlottTemp
	GOTO 172
  
PlottTemp:
	dtid = tim + min / 60
	xdt = dtid - c1: IF xdt < 0 THEN xdt = 24 + xdt
	ptid = x0 + xdt * b1

173 IF t1i > 50 THEN 174      'Temp fr†n Milos
	qq = t1i
	IF qq > tempmax THEN qq = tempmax
	IF qq < tempmax - 20 THEN qq = tempmax - 20
	d = 3: ypos = y0 + d * ho + bh * (tempmax - qq) / 20: PSET (ptid, ypos), vf
 
174 RETURN

UppdateraTryck:

	OPEN "tryckmax" FOR OUTPUT AS #4
	PRINT #4, tryckmax
	CLOSE #4
	varv = 2            'totalt 13 rutor
	ymin = ymin1 + yflytt * varv    '”verdel varje ruta
	ymax = ymin + ydel * yantal     'nederdel varje ruta
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), colbakgrund, BF

		FOR y = 1 TO 3              'horisontella linjer i rutor
		ypos = ymin + ydel * y
		LINE (xmin, ypos)-(xmax, ypos), colgrid, , &HAAAA
		NEXT y

		FOR x = 1 TO 23             'vertikala linjer i rutor
		col = colgrid
REM        IF INT(x / 3) = x / 3 THEN col = 7  'var tredje timme lite kraftigare
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col, , &HAAAA
		NEXT x

		FOR x = 3 TO 21 STEP 3             'vertikala linjer i rutor
		col = colgrid
REM        IF INT(x / 3) = x / 3 THEN col = 7  'var tredje timme lite kraftigare
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col
		NEXT x
	 
		LINE (xmin, ymin)-(xmax, ymax), colgrid, B
  

	COLOR coltext
	LOCATE 6, 5: PRINT "Tryck"
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), ramcol, B
	COLOR colexp: LOCATE 7, xp
	IF tryckmax = 980 THEN PRINT "[960-980] ";
	IF tryckmax = 985 THEN PRINT "[965-985] ";
	IF tryckmax = 990 THEN PRINT "[970-990] ";
	IF tryckmax = 995 THEN PRINT "[975-995] ";
	IF tryckmax = 1000 THEN PRINT "[980-1000] ";
	IF tryckmax = 1005 THEN PRINT "[985-1005] ";
	IF tryckmax = 1010 THEN PRINT "[990-1010] ";
	IF tryckmax = 1015 THEN PRINT "[995-1015] ";
	IF tryckmax = 1020 THEN PRINT "[1000-1020]";
	IF tryckmax = 1025 THEN PRINT "[1005-1025]";
	IF tryckmax = 1030 THEN PRINT "[1010-1030]";
	IF tryckmax = 1035 THEN PRINT "[1015-1035]";
	IF tryckmax = 1040 THEN PRINT "[1020-1040] ";
	IF tryckmax = 1045 THEN PRINT "[1025-1045] ";
	IF tryckmax = 1050 THEN PRINT "[1030-1050] ";
   

	ON ERROR GOTO Error471
	IF c1 = 0 THEN GOTO 471
	GMilogFil$ = summapp$ + "mellan\mm" + GFilDatum$
	OPEN GMilogFil$ FOR INPUT AS #4
470 IF EOF(4) THEN CLOSE #4: GOTO 471
	INPUT #4, tim, min, fa2, da2, fx2, p, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	IF tim < c1 THEN 470
	GOSUB PlottTryck
	GOTO 470

471 NyMilogFil$ = summapp$ + "mellan\mm" + NyFilDatum$
	ON ERROR GOTO Error472
	OPEN NyMilogFil$ FOR INPUT AS #4
472 IF EOF(4) THEN CLOSE #4: RETURN
	INPUT #4, tim, min, fa2, da2, fx2, p, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	GOSUB PlottTryck
	GOTO 472

PlottTryck:
	dtid = tim + min / 60
	xdt = dtid - c1: IF xdt < 0 THEN xdt = 24 + xdt
	ptid = x0 + xdt * b1

473 IF p > 1050 THEN 474      'Temp frr†n Milos/Logger
	qq = p
	IF qq > tryckmax THEN qq = tryckmax
	IF qq < tryckmax - 20 THEN qq = tryckmax - 20
	d = 2: ypos = y0 + d * ho + bh * (tryckmax - qq) / 20: PSET (ptid, ypos), vf

474 RETURN



UppdateraKVLV:

	OPEN "LVmax" FOR OUTPUT AS #4
	PRINT #4, LVmax
	CLOSE #4
	GOTO 671

	COLOR 15
	LOCATE 16, xp + 13
	IF kvn < 0 THEN kvn = 0
	IF kvn < 1100 THEN PRINT USING "####"; kvn ELSE PRINT " ---"
	LOCATE 17, xp + 13
	IF lvn < 0 THEN lvn = 0
	IF lvn < 1000 THEN PRINT USING "####"; lvn ELSE PRINT " ---"


671    varv = 7            'totalt 13 rutor
	ymin = ymin1 + yflytt * varv    '”verdel varje ruta
	ymax = ymin + ydel * yantal     'nederdel varje ruta
	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), colbakgrund, BF

		FOR y = 1 TO 3              'horisontella linjer i rutor
		ypos = ymin + ydel * y
		LINE (xmin, ypos)-(xmax, ypos), colgrid, , &HAAAA
		NEXT y

		FOR x = 1 TO 23             'vertikala linjer i rutor
		col = colgrid
REM        IF INT(x / 3) = x / 3 THEN col = 7  'var tredje timme lite kraftigare
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col, , &HAAAA
		NEXT x

		FOR x = 3 TO 21 STEP 3             'vertikala linjer i rutor
		col = colgrid
REM        IF INT(x / 3) = x / 3 THEN col = 7  'var tredje timme lite kraftigare
		xpos = xmin + xdel * x
		LINE (xpos, ymin)-(xpos, ymax), col
		NEXT x
	
		LINE (xmin, ymin)-(xmax, ymax), colgrid, B
 

	COLOR 15
	LOCATE 16, 5: PRINT "KV"
	COLOR 12
	LOCATE 16, 7: PRINT "LV"

	LINE (xmin - 1, ymin - 1)-(xmax + 1, ymax + 1), ramcol, B

	COLOR 4: LOCATE 17, xp + 23
	IF LVmax = 600 THEN PRINT "[500-600]"
	IF LVmax = 575 THEN PRINT "[475-575]"
	IF LVmax = 550 THEN PRINT "[450-550]"
	IF LVmax = 525 THEN PRINT "[425-525]"
	IF LVmax = 500 THEN PRINT "[400-500]"
	IF LVmax = 475 THEN PRINT "[375-475]"
	IF LVmax = 450 THEN PRINT "[350-450]"
	IF LVmax = 425 THEN PRINT "[325-425]"
	IF LVmax = 400 THEN PRINT "[300-400]"
	IF LVmax = 375 THEN PRINT "[275-375]"
	IF LVmax = 350 THEN PRINT "[250-350]"
	IF LVmax = 325 THEN PRINT "[225-325]"
	IF LVmax = 300 THEN PRINT "[200-300]"
	IF LVmax = 275 THEN PRINT "[175-275]"
	IF LVmax = 250 THEN PRINT "[150-250]"

  

	ON ERROR GOTO Error571
	IF c1 = 0 THEN GOTO 571
	GMilogFil$ = summapp$ + "mellan\mm" + GFilDatum$
	OPEN GMilogFil$ FOR INPUT AS #4
570 IF EOF(4) THEN CLOSE #4: GOTO 571
	INPUT #4, tim, min, fa2, da2, fx2, p, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	IF tim < c1 THEN 570
	GOSUB PlottKVLV
	GOTO 570

  

571 NyMilogFil$ = summapp$ + "mellan\mm" + NyFilDatum$
	ON ERROR GOTO Error572
	OPEN NyMilogFil$ FOR INPUT AS #4
572 IF EOF(4) THEN CLOSE #4: RETURN
	INPUT #4, tim, min, fa2, da2, fx2, p, p3, rh, t1i, td
	INPUT #4, kvn, lvn, d3, sikt1, sikt2
	GOSUB PlottKVLV
	GOTO 572

PlottKVLV:
	dtid = tim + min / 60
	xdt = dtid - c1: IF xdt < 0 THEN xdt = 24 + xdt
	ptid = x0 + xdt * b1

770 IF kvn > 1000 THEN 771      'kortv†g
	IF kvn < 0 THEN 771
	qq = kvn
	IF qq > 1000 THEN qq = 1000
	d = 7: ypos = y0 + d * ho + bh * (1000 - qq) / 1000: PSET (ptid, ypos), vf

771 IF lvn > 1000 THEN 774      'l†ngv†g
	IF lvn < 0 THEN 774
	qq = lvn
	IF qq > LVmax THEN qq = LVmax
	IF qq < LVmax - 100 THEN qq = LVmax - 100
	d = 7: ypos = y0 + d * ho + bh * (LVmax - qq) / 100: PSET (ptid, ypos), vlvn
   
774 RETURN


REM **********************************************************************
REM ******************* Rita Klockor i nedre halvan av bild **************


Klockor:

REM    GOSUB BerMaxMin
	LINE (0, 295)-(640, 295), 8
	LINE (0, 296)-(640, 461), 0, BF 'sl„cker yta med 'Klockor'
REM    LOCATE 10, 10: INPUT "", q$

	stepx = 1
	vriktn = da2
	rktnmin = dm10: rktnmax = dx10
	vhast = fa2
	IF fx10 < 1000 THEN Max10 = fx10

	tryck = qff
	temp = t1i: REM -2.8

	

	GOSUB RitaVindRos1
	GOSUB RitaHastSkala
	GOSUB RitaTryckSkala
	GOSUB RitaTempSkala1
	GOSUB maxmin

	RETURN




REM ================== Rita vindros f”r vindriktning ===========

RitaVindRos1:
	vrxm = 75
	vrym = 390
	vrr = 52
   
	FOR x = 1 TO 3 STEP stepx            'ritar periferi
	CIRCLE (vrxm, vrym), vrr + x, 2
	NEXT x
   

	rmin = vrr - 1: rmax = vrr + 7      'l„ngd p† ticks

	FOR x = 10 TO 360 STEP 10           'ritar ticks
	xr = SIN(x * z9): yr = COS(x * z9)
	COLOR 2
	IF x / 30 = INT(x / 30) THEN COLOR 15 'vita ticks f”r var 30:e grad
	LINE (vrxm + xr * rmin, vrym - yr * rmin)-(vrxm + xr * rmax, vrym - yr * rmax)
	NEXT x

	rmin = vrr - 20: rmax = vrr       'l„ngd p† mix/max vindriktn
	
	xvind = SIN(rktnmin * z9): yvind = COS(rktnmin * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 9'ritar vindhast

	xvind = SIN(rktnmax * z9): yvind = COS(rktnmax * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 9'ritar vindhast


	xvind = SIN(vriktn * z9): yvind = COS(vriktn * z9)      'ritar vindriktning
	LINE (vrxm, vrym)-(vrxm + xvind * vrr, vrym - yvind * vrr)
	COLOR 15
	vrktn$ = RIGHT$(STR$(vriktn), 3)
	IF vriktn < 10 THEN vrktn$ = "00" + RIGHT$(vrktn$, 1) 'f”r numerisk utskrift
	IF vriktn < 100 THEN vrktn$ = "0" + RIGHT$(vrktn$, 2)
	LOCATE 25, 9: PRINT vrktn$
	LOCATE 20, 5: COLOR 10: PRINT "Vindriktning"
	COLOR 7
	LOCATE 21, 10: PRINT "N";
	LOCATE 29, 10: PRINT "S";
	LOCATE 25, 2: PRINT "W";
	LOCATE 25, 18: PRINT "E";
	LOCATE 22, 3: PRINT "NW";
	LOCATE 28, 3: PRINT "SW";
	LOCATE 22, 16: PRINT "NE";
	LOCATE 28, 16: PRINT "SE";
	COLOR 15
	RETURN


REM ================== Rita vindros f”r vindhastighet ===========

RitaHastSkala:
	vrxm = 223
	vrym = 400
	vrr = 55

REM Vindhast 0= vinkel 240 gr, vindhast 30=vinkel 330, vindhast 15=90 gr
REM 30 m/s motsvarar 240 gr skilland-> 1m/s = 8 gr
REM vinkeln blir allts† 240 - vindhast*8


REM ************ Ritar cirkelb†ge 330-210 gr

  
	FOR x = 1 TO 3 STEP stepx
	CIRCLE (vrxm, vrym), vrr + x, 2, 330 * z9, 210 * z9
	NEXT x

	FOR x = 1 TO 4
	CIRCLE (vrxm, vrym), x, 15
	NEXT x

	rmin = vrr - 20: rmax = vrr
	bhast = 240 + Max10 * 8
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 9'ritar vindhast
   

	rmin = vrr - 10: rmax = vrr
	bhast = 240 + hmax * 8
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 14'ritar vindhast

	IF vhast > 100 THEN 77
	bhast = 240 + vhast * 8
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm, vrym)-(vrxm + xvind * vrr, vrym - yvind * vrr), 15'ritar vindhast
	
77  rmin = vrr - 1: rmax = vrr + 7

	FOR x = -120 TO 120 STEP 8
	xr = SIN(x * z9): yr = COS(x * z9)
	COLOR 2
	IF x / 10 = INT(x / 10) THEN COLOR 15
	LINE (vrxm + xr * rmin, vrym - yr * rmin)-(vrxm + xr * rmax, vrym - yr * rmax)
	NEXT x

	LOCATE 20, 23: COLOR 10: PRINT "Vindhastighet"
	COLOR 7
	LOCATE 21, 28: PRINT "15";
	LOCATE 22, 22: PRINT "10"
	LOCATE 22, 34: PRINT "20"
	LOCATE 25, 20: PRINT "5"
	LOCATE 25, 37: PRINT "25"
	LOCATE 28, 21: PRINT "0"
	LOCATE 28, 36: PRINT "30"
	COLOR 7
	LOCATE 28, 28: PRINT "m/s"
	COLOR 15: LOCATE 27, 27
	IF vhast < 100 THEN PRINT USING "##.#"; vhast ELSE PRINT " ---"
	LOCATE 29, 13: COLOR 9: PRINT "10min    Max: "; : PRINT USING "##.#"; Max10;
	COLOR 15

	RETURN

REM ================== Rita Tryckskala ===========

RitaTryckSkala:
	vrxm = 393
	vrym = 400
	vrr = 55


REM ************ Ritar cirkelb†ge 330-210 gr

REM Tryck fr†n 960-1040 hPa
REM Vinkeln blir d† 240/80 per hPa =3 gr per hPa
 
	FOR x = 1 TO 3 STEP stepx
	CIRCLE (vrxm, vrym), vrr + x, 11, 330 * z9, 210 * z9
	NEXT x

	FOR x = 1 TO 4
	CIRCLE (vrxm, vrym), x, 15
	NEXT x


	rmin = vrr - 10: rmax = vrr
	bhast = 240 + (pmax - 960) * 3
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 14'ritar vindhast

	rmin = vrr - 10: rmax = vrr
	bhast = 240 + (pmin - 960) * 3
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 14'ritar vindhast


	rktn = 240 + (tryck - 960) * 3
	xvind = SIN(rktn * z9): yvind = COS(rktn * z9)
	LINE (vrxm, vrym)-(vrxm + xvind * vrr, vrym - yvind * vrr)


	rmin = vrr - 1: rmax = vrr + 7

	FOR x = -120 TO 120 STEP 15
	xr = SIN(x * z9): yr = COS(x * z9)
	COLOR 3
	IF x / 10 = INT(x / 10) THEN COLOR 15
	LINE (vrxm + xr * rmin, vrym - yr * rmin)-(vrxm + xr * rmax, vrym - yr * rmax)
	NEXT x
	COLOR 7
	LOCATE 28, 40: PRINT "960";
	LOCATE 26, 39: PRINT "970";
	LOCATE 24, 39: PRINT "980";
	LOCATE 22, 43: PRINT "990";
	LOCATE 21, 48: PRINT "1000";
	LOCATE 22, 55: PRINT "1010";
	LOCATE 24, 58: PRINT "1020";
	LOCATE 26, 58: PRINT "1030";
	LOCATE 28, 57: PRINT "1040";
	COLOR 7
	LOCATE 28, 49: PRINT "hPa";
	COLOR 15
	LOCATE 27, 47: PRINT USING "####.#"; tryck
	COLOR 10
	LOCATE 20, 42: PRINT "Tryck vid havsytan"

   
	RETURN


REM ================== Rita Tempskala ===========

RitaTempSkala1:
	vrxm = 557
	vrym = 400

	vrr = 55



REM ************ Ritar cirkelb†ge 330-210 gr
REM Temp mellan -30 och +30 gr
REM det ger 240/60 gr = 4 grader per tempgrad

	rmin = vrr - 10: rmax = vrr
	bhast = 240 + (tmax + 30) * 4
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 14'ritar vindhast

	rmin = vrr - 10: rmax = vrr
	bhast = 240 + (tmin + 30) * 4
	xvind = SIN(bhast * z9): yvind = COS(bhast * z9)
	LINE (vrxm + xvind * rmin, vrym - yvind * rmin)-(vrxm + xvind * rmax, vrym - yvind * rmax), 14'ritar vindhast


	rktn = 240 + (temp + 30) * 4
	xvind = SIN(rktn * z9): yvind = COS(rktn * z9)
	LINE (vrxm, vrym)-(vrxm + xvind * vrr, vrym - yvind * vrr), 15


	   FOR x = 1 TO 3 STEP stepx
	CIRCLE (vrxm, vrym), vrr + x, 4, 330 * z9, 90 * z9
	CIRCLE (vrxm, vrym), vrr + x, 1, 90 * z9, 210 * z9
	NEXT x

	FOR x = 1 TO 4
	CIRCLE (vrxm, vrym), x, 15
	NEXT x

   
	rmin = vrr - 1: rmax = vrr + 7

	FOR x = -120 TO 120 STEP 20
	xr = SIN(x * z9): yr = COS(x * z9)
	COLOR 1: IF x > 0 THEN COLOR 4
	IF x / 40 = INT(x / 40) THEN COLOR 15
	LINE (vrxm + xr * rmin, vrym - yr * rmin)-(vrxm + xr * rmax, vrym - yr * rmax)
	NEXT x

	COLOR 7
	LOCATE 28, 62: PRINT "-30";
	LOCATE 25, 60: PRINT "-20";
	LOCATE 22, 63: PRINT "-10"
   
	LOCATE 21, 70: PRINT "0";
	LOCATE 22, 76: PRINT "10";
	LOCATE 25, 79: PRINT "20";
	LOCATE 28, 78: PRINT "30";
	
	LOCATE 28, 70: PRINT CHR$(248); "C";
	COLOR 15
	LOCATE 27, 68: PRINT USING "###.#"; temp
	COLOR 10
	
	LOCATE 20, 66: PRINT "Temperatur"
   
	RETURN


REM =============== 24 tim max/min l„ngst ber ===========================

maxmin:

	LINE (0, 463)-(640, 463), 8
	LOCATE 30, 4: COLOR 10: PRINT "Senaste 24 tim --";
	LOCATE 30, 22: PRINT "Max: "; : COLOR 14: PRINT USING "##.#"; hmax;
	COLOR 10: PRINT " m/s";
	LOCATE 30, 37: COLOR 10: PRINT "Min/Max:"; : COLOR 14
	IF pmin < 2000 THEN PRINT USING "####.#"; pmin;  ELSE PRINT USING "####.#"; gpmin;
	COLOR 10: PRINT "/"; : COLOR 14
	IF pmax < 2000 THEN PRINT USING "####.#"; pmax;  ELSE PRINT USING "####.#"; gpmax;
	IF pmin < 2000 THEN gpmin = pmin
	IF pmax < 2000 THEN gpmax = pmax
   
	LOCATE 30, 60: COLOR 10: PRINT "Min/Max:"; : COLOR 14: PRINT USING "+##.#"; tmin;
	COLOR 10: PRINT "/"; :  COLOR 14: REM PRINT USING "+##.#"; tmax;
	IF tmax >= 0 THEN PRINT "+";  ELSE PRINT "-";
	tmax$ = STR$(tmax)
REM    IF tmax >= 0 THEN tmax$ = RIGHT$(tmax$, LEN(tmax$) - 1)
	tmax$ = RIGHT$(tmax$, LEN(tmax$) - 1)
	IF LEFT$(tmax$, 1) = "." THEN PRINT "0";
	PRINT tmax$;
	FOR x = 1 TO LEN(tmax$)
	q$ = MID$(tmax$, x, 1)

	IF q$ = "." THEN RETURN

1 NEXT x
	PRINT ".0";

	RETURN



REM ********** Subrutin f”r ber„kning av max/min och nederb”rd ********

BerMaxMin:

	tmin = t1i: tmax = t1i: pmin = qff: pmax = qff: hmax = 0
	nbd3 = 0: nbd6 = 0: nbd12 = 0: nbd24 = 0: nbd10m = 0: nbd3m = 0
	GOSUB julian
8000 ON ERROR GOTO error8000
	GMaxMinFil$ = "c:\MaxMin\XM" + GFilDatum$
	OPEN GMaxMinFil$ FOR INPUT AS #4
8004    IF EOF(4) THEN CLOSE #4:  GOTO 8001
	INPUT #4, juliandagx, tim, min
REM    LOCATE 10, 10: PRINT juliandag; juliandagx; hmax
	 INPUT #4, max2, p2, temp, d1
	IF juliandag - juliandagx > 1 THEN 8004
REM    IF temp < 10 THEN LOCATE 10, 10: PRINT temp; juliandagx; tim; min: INPUT "", q$
	IF max2 > 100 THEN 375
	IF max2 > hmax THEN hmax = max2
375 IF p2 > 1050 THEN 376
	IF p2 < pmin THEN pmin = p2
	IF p2 > pmax THEN pmax = p2
376 IF temp > 100 THEN 377
	IF temp < tmin THEN tmin = temp
	IF temp > tmax THEN tmax = temp
	IF d1 > 100 THEN 379
	nbd24 = nbd24 + d1
	IF juliandag - juliandagx > .00208 THEN 277   'senaste 3 min
	nbd3m = nbd3m + d1
277 IF juliandag - juliandagx > .0069 THEN 377   'senaste 10 min f”r VWS
	nbd10m = nbd10m + d1
377 IF juliandag - juliandagx > .125 THEN 378
	nbd3 = nbd3 + d1
378 IF juliandag - juliandagx > .25 THEN 371
	nbd6 = nbd6 + d1
371 IF juliandag - juliandagx > .5 THEN 379
	nbd12 = nbd12 + d1


379 GOTO 8004
   

8001 ON ERROR GOTO error8001
	NyMaxMinFil$ = "c:\MaxMin\XM" + NyFilDatum$
	OPEN NyMaxMinFil$ FOR INPUT AS #4
8005    IF EOF(4) THEN CLOSE #4:  GOTO 8002
	INPUT #4, juliandagx, tim, min
	INPUT #4, max2, p2, temp, d1
	
REM    LOCATE 10, 10: PRINT juliandag; juliandagx; hmax
REM    INPUT "", q$
	IF max2 > 100 THEN 385
	IF max2 > hmax THEN hmax = max2
385 IF p2 > 1050 THEN 386
	IF p2 < pmin THEN pmin = p2
	IF p2 > pmax THEN pmax = p2
386 IF temp > 100 THEN 387
	IF temp < tmin THEN tmin = temp
	IF temp > tmax THEN tmax = temp
	IF d1 > 100 THEN 389
	nbd24 = nbd24 + d1
	IF juliandag - juliandagx > .00208 THEN 287   'senaste 3 min
	nbd3m = nbd3m + d1
287 IF juliandag - juliandagx > .0069 THEN 387   'senaste 10 min f”r VWS
	nbd10m = nbd10m + d1

387 IF juliandag - juliandagx > .125 THEN 388
	nbd3 = nbd3 + d1
388 IF juliandag - juliandagx > .25 THEN 381
	nbd6 = nbd6 + d1
381 IF juliandag - juliandagx > .5 THEN 389
	nbd12 = nbd12 + d1

389 REM

	GOTO 8005

8002 nbd3 = nbd3 / 10: nbd6 = nbd6 / 10
	 nbd12 = nbd12 / 10: nbd24 = nbd24 / 10: nbd10m = nbd10m / 10
	 nbdintens = nbd3m / 10 * 20   'nederb”rdsintensitet per tim senaste 3 min

	RETURN

error8000:
	RESUME 8001

error8001:
	RESUME 8002




REM ******* L„sning av MILOS o LOGGER *************

LasningMiLog:

REM ******************* START MILOS ***************************
REM ------------- MILOS ansluts till COM1 ---------------------

	CLOSE :   OPEN "com1:1200,n,7,2,rs,ds,rb2000" FOR INPUT AS #1
1010   REM  on error GOTO Omstart

	INPUT #1, a$        'l„sning av hel str„ng fr†n MILOS {LINE INPUT ?}

	IF StartFlag = 0 AND LEFT$(a$, 2) <> "20" THEN 1010   'f”r att starta r„tt
REM --- Startat r„tt och StartFlag=1 --------
	IF LEFT$(a$, 2) = "20" THEN GOTO DatumTid
	IF LEFT$(a$, 3) = "FA2" THEN GOTO fa2
	IF LEFT$(a$, 4) = "FA10" THEN GOTO fa10
	IF LEFT$(a$, 1) = "P" THEN GOTO p
	IF LEFT$(a$, 3) = "T1I" THEN GOTO t1i
	IF LEFT$(a$, 3) = "CB1" THEN GOTO cb1
	IF LEFT$(a$, 3) = "ST2" THEN GOTO ST2
	IF LEFT$(a$, 6) = "STATUS" THEN GOTO STATUS
	GOSUB WindChill
	IF LEFT$(a$, 1) = "=" THEN GOTO LOGGER
	GOTO 1010


REM **** Rutin f”r att ta datum och tid ur str„ng fr†n MILOS ****
REM ---- Tiden i Milos „ndras i Subrutin UppdateraMILOS -----

DatumTid:
	StartFlag = 1
	ar$ = LEFT$(a$, 4)
	jar = VAL(ar$) - 2006
	man$ = MID$(a$, 6, 2)
	jman = VAL(man$)
	dag$ = MID$(a$, 9, 2)
	jdag = VAL(jdag$)
	tim$ = MID$(a$, 13, 2)
	tim = VAL(tim$): jtim = tim
	min$ = MID$(a$, 16, 2)
	min = VAL(min$): jmin = min
	timestamp$ = ar$ + "-" + man$ + "-" + dag$ + " " + tim$ + ":" + min$ + ":00"
   
	GOSUB julian
	GOTO 1010

REM *** Rutin f”r att extrahera data ur rad som b”rjar med FA2 ***
REM --- 2min-v„rden: fx/fm=hast max/min i m/s, da2=medelvindriktning ----

fa2:
	
	IF MID$(a$, 10, 1) = "/" THEN fa2 = 32767 ELSE fa2 = VAL(MID$(a$, 5, 6))
	IF MID$(a$, 21, 1) = "/" THEN fx2 = 32767 ELSE fx2 = VAL(MID$(a$, 16, 6))
	IF MID$(a$, 43, 1) = "/" THEN da2 = 32767 ELSE da2 = VAL(MID$(a$, 38, 6))
	GOTO 1010


REM *** Rutin f”r att extrahera data ur rad som b”rjar med FA10 ***
REM --- 10min-v„rden:fx/fm=max/min hast i m/s, dx/dm=max/min vindriktn ----

fa10:

	IF MID$(a$, 21, 1) = "/" THEN fx10 = 32767 ELSE fx10 = VAL(MID$(a$, 16, 6))
	IF MID$(a$, 32, 1) = "/" THEN fm10 = 32767 ELSE fm10 = VAL(MID$(a$, 27, 6))
	IF MID$(a$, 54, 1) = "/" THEN dx10 = 32767 ELSE dx10 = VAL(MID$(a$, 49, 6))
	IF MID$(a$, 65, 1) = "/" THEN dm10 = 32767 ELSE dm10 = VAL(MID$(a$, 60, 6))
	IF MID$(a$, 43, 1) = "/" THEN da10 = 32767 ELSE da10 = VAL(MID$(a$, 38, 6))
	IF MID$(a$, 10, 1) = "/" THEN fa10 = 32767 ELSE fa10 = VAL(MID$(a$, 5, 6))
	GOTO 1010


REM *** Rutin f”r att extrahera data ur rad som b”rjar med P ***
REM --- p=tryck, p3=3tim tendens, QFF, rel fukt ---

p:

	IF MID$(a$, 10, 1) = "/" THEN p = 32767 ELSE p = VAL(MID$(a$, 5, 6))
	IF MID$(a$, 21, 1) = "/" THEN p3 = 32767 ELSE p3 = VAL(MID$(a$, 16, 6))
	IF MID$(a$, 54, 1) = "/" THEN qff = 32767 ELSE qff = VAL(MID$(a$, 49, 6))
	IF MID$(a$, 65, 1) = "/" THEN rh = 32767 ELSE rh = VAL(MID$(a$, 60, 6))
	GOTO 1010

REM *** Rutin f”r att extrahera data ur rad som b”rjar med T1I ***
REM --- t1i=temp (ute), t2i=temp (inne), t3i=marktemp, td=daggpunkt ---
REM --- Daggpunkt fel p.g.a givare; ber„knas med Rotronic ------

t1i:

	IF MID$(a$, 10, 1) = "/" THEN t1i = 32767 ELSE t1i = VAL(MID$(a$, 5, 6))
	IF MID$(a$, 32, 1) = "/" THEN t2i = 32767 ELSE t2i = VAL(MID$(a$, 27, 6))
	IF MID$(a$, 54, 1) = "/" THEN t3i = 32767 ELSE t3i = VAL(MID$(a$, 49, 6))
REM IF MID$(a$, 65, 1) = "/" THEN td = 32767 ELSE td = VAL(MID$(a$, 60, 6))
	GOTO 1010

REM *** Rutin f”r att extrahera data ur rad som b”rjar med CB1 ***
REM --- cb1-cb4=molnbaser fr†n Impulsphysik, st1=anv„nds inte ----

cb1:
	LOCATE 9, 7
REM    PRINT a$

	IF MID$(a$, 10, 1) = "/" THEN cb1 = 32767 ELSE cb1 = VAL(MID$(a$, 5, 6))
	IF MID$(a$, 21, 1) = "/" THEN cb2 = 32767 ELSE cb2 = VAL(MID$(a$, 16, 6))
	IF MID$(a$, 32, 1) = "/" THEN cb3 = 32767 ELSE cb3 = VAL(MID$(a$, 27, 6))
	IF MID$(a$, 43, 1) = "/" THEN cb4 = 32767 ELSE cb4 = VAL(MID$(a$, 38, 6))
	IF MID$(a$, 65, 1) = "/" THEN ST1 = 32767 ELSE ST1 = VAL(MID$(a$, 60, 6))
	GOTO 1010

REM *** Rutin f”r att extrahera data ur rad som b”rjar med ST2 ***
REM --- ST2=anv„nds inte ----
ST2:
	IF MID$(a$, 10, 1) = "/" THEN ST2 = 32767 ELSE ST2 = VAL(MID$(a$, 5, 6))
	GOTO 1010

REM *** Bara l„sning av STATUS-str„ng ***

STATUS:
	STATUS$ = RIGHT$(a$, 25)
	GOTO 1010

REM ****************** SLUT MILOS **********************





REM ****************** START LOGGER ********************
REM --------- TefriLoggern ansluts till COM2 -----------

LOGGER:
   

288 OPEN "com2:2400,n,8,1,rs,ds" FOR RANDOM AS #3

	IF varvflag = 0 THEN PRINT #3, "A" 'ett extra A vid f”rsta start
	PRINT #3, "A"           'f”r att polla Loggern
	varvflag = 1


	vz = 0
291 LINE INPUT #3, qq$

	vz = vz + 1
	IF vz = 1 THEN qq$ = RIGHT$(qq$, LEN(qq$) - 1)
	logv(vz) = VAL(qq$)
REM    IF vz = 1 THEN LOCATE 5, 10: PRINT qq$; logv(1)
REM    IF vz = 4 THEN LOCATE 6, 10: PRINT qq$; logv(4)
	IF LEN(qq$) > 6 THEN CLOSE #3: GOTO 995
	GOTO 291


995 vzk = vz - 14
REM    LOCATE 1, 10: PRINT vzk; vz
REM    kvn = 32767: lvn = 32767:
	a2 = 32767: a3 = 32767
	a1 = logv(1 + vzk): a4 = logv(4 + vzk)
	kvn = 12.7 * (a1 / 10 + 12.3): lvn = 6.1875 * (a4 / 10 + 11.3)
REM    LOCATE 10, 10: PRINT a1; kvn
REM    LOCATE 11, 10: PRINT a4; lvn
REM k-faktor kan beh”va justeras
   
	a5 = 32767: a6 = a5: d2 = a5: kvu = a5: lvu = a5
	a7 = logv(7 + vzk): d1 = logv(8 + vzk): d3 = logv(10 + vzk)
	sikt1 = logv(11 + vzk): sikt2 = logv(12 + vzk)

REM ------------V„rde s„tts till 32767 om inga givare------------


REM *** Ber„kning av daggpunkt efter ev.korr av rh fr†n Rotronic ***
	tk = t1i + 273.16
	rh = a7 / 10 + rhkorr  'korr av givarv„rde
	IF rh > 100 THEN rh = 100
	kk = (150 * LOG(100 / rh) - 2 * (tk - 273.16)) + 2711.5
	tdny = 2 * kk * tk / (tk * LOG(100 / rh) + 2 * kk) - 273.16
	td = tdny


REM ****************** LOGGER SLUT **************************




REM ****************** Lagring p† lokal HDD (och p† Server) *******
  

LagraUt:

REM    datumx$ = ar$ + "-" + man$ + "-" + dag$
REM    tidx$ = tim$ + ":" + min$ + ":00"
REM    timestamp$ = datumx$ + " " + tidx$

501 utfil$ = "c:\arkiv\MG" + RIGHT$(ar$, 2) + man$ + dag$ + ".txt"

600 OPEN utfil$ FOR APPEND AS #4
	PRINT #4, timestamp$; ","; tim; ","; min; ",";
	PRINT #4, fa2; ","; da2; ","; fx2; ",";
	PRINT #4, fx10; ","; fm10; ","; dx10; ","; dm10; ",";
	PRINT #4, p; ","; qff; ","; p3; ",";
	PRINT #4, rh; ","; t1i; ","; t2i; ","; t3i; ","; td; ",";
	PRINT #4, kvn; ","; lvn; ","; kvu; ","; lvu; ",";
	PRINT #4, a5; ","; a6; ","; a7; ",";   'a7=rh fr†n Rotronic
	PRINT #4, d1 / 10; ","; d2; ","; d3; ","; 'd1/10 ->mm nbd, d3=pulser
	PRINT #4, sikt1; ","; sikt2
	CLOSE #4

REM -------------- Om inte lagring p† Vectra ta bort rem p† 602 -----------
REM ----- Arkiv-mapp p† Vectra s„tts upp som enhet p: ----------

REM 602 goto 502

	ON ERROR GOTO ERROR502

	utfil$ = "p:\MG" + RIGHT$(ar$, 2) + man$ + dag$ + ".txt"
	OPEN utfil$ FOR APPEND AS #4
	PRINT #4, timestamp$; ","; tim; ","; min; ",";
	PRINT #4, fa2; ","; da2; ","; fx2; ",";
	PRINT #4, fx10; ","; fm10; ","; dx10; ","; dm10; ",";
	PRINT #4, p; ","; qff; ","; p3; ",";
	PRINT #4, rh; ","; t1i; ","; t2i; ","; t3i; ","; td; ",";
	PRINT #4, kvn; ","; lvn; ","; kvu; ","; lvu; ",";
	PRINT #4, a5; ","; a6; ","; a7; ",";   'a7=rh fr†n Rotronic
	PRINT #4, d1 / 10; ","; d2; ","; d3; ","; 'd1/10 ->mm nbd, d3=pulser
	PRINT #4, sikt1; ","; sikt2
	CLOSE #4


REM -------- Lagring f”r ber„kning av max och min ---------------

502 utfil$ = "c:\MaxMin\XM" + RIGHT$(ar$, 2) + man$ + dag$ + ".txt"
	OPEN utfil$ FOR APPEND AS #4
	PRINT #4, juliandag;
	PRINT #4, tim; min;
	PRINT #4, fx2; qff; t1i; d1  'd1 skall vara nederb”rdsm„ngd=antal pulser
	CLOSE #4
   
REM -------- Lagring f”r uppdatering och ev l„sning till hemsida -------

504 OPEN "c:\senaste\senmilog" FOR OUTPUT ACCESS WRITE LOCK READ WRITE AS #4
	PRINT #4, juliandag;
	PRINT #4, tim; min; fa2; da2; fx2; fa10; da10; fx10; dx10; dm10;
	PRINT #4, p; qff; p3; rh; t1i; t2i; t3i; td
	PRINT #4, kvn; kvu; lvn; lvu; a5; a6; a7; d1; d2; d3; sikt1; sikt2
	CLOSE #4

	RETURN   ' G† tillbaka till presentationsprogram



REM ***********  Ber„kning av k”ldfaktor ************

WindChill:

	kmh = fa2 * 3.6'Formeln beh”ver kmh
	wc = 13.12 + .6215 * t1i - 11.37 * kmh ^ .16 + .3965 * t1i * kmh ^ .16
	RETURN



REM ---- Rutin f”r uppdatering av PC-Klocka mot server -----------









VWS:

	ON ERROR GOTO Error499
	VWS$ = "v:\CSV\milog.csv"   ' net use v skall vara \\vectra\milog\
	var$ = "20" + RIGHT$(date1$, 2)
REM    tim = VAL(LEFT$(timex$, 2))
REM    min = VAL(MID$(timex$, 4, 2))
	mm1 = nbd10m
	OPEN VWS$ FOR OUTPUT AS #4
	PRINT #4, "1.00"; ",";
	PRINT #4, var$; ","; man$; ","; dag$; ","; tim; ","; min; ",0,";
	PRINT #4, fa2; ","; fx2; ","; da2; ", ,"; rh; ","; t2i; ",";
	PRINT #4, t1i; ","; qff; ","; mm1; ","; mm1; ","; mm1; ", ";
REM    LOCATE 10, 10: PRINT t1i; p; qff;
	PRINT #4, ", , , , , , , , , , , , ,"; ","; td; " , , , , , , , , , , , ,";
	CLOSE #4

	OPEN "v:\WD\datain38.ini" FOR OUTPUT AS #4
	PRINT #4, USING "#####.#"; fa2 * 1.94; fx2 * 1.94; t1i; rh;
	PRINT #4, USING "#####.#"; qff;
	PRINT #4, USING "#####.#"; mm; da2; t3i; t2i;
	PRINT #4, " 0 0"
	CLOSE #4

499 RETURN




SummaUppdat:

	timestamp$ = ar$ + "-" + man$ + "-" + dag$ + " " + LEFT$(timex$, 5)

REM    IF d1 = 0 THEN 91
REM    IF dagnbd >= 0 THEN 91

	IF dagnbd < 0 THEN 331
	IF d1 = 0 THEN 91

331 utfil$ = summa$ + "an" + ar$ + ".txt"
	IF arnbd >= 0 THEN arnbd = arnbd + d1 / 10 ELSE arnbd = d1 / 10
	OPEN utfil$ FOR OUTPUT AS #4
	PRINT #4, arnbd
	CLOSE #4

	ON ERROR GOTO ERROR333
	utfil$ = "V:\" + "an" + ar$ + ".txt"
	OPEN utfil$ FOR OUTPUT ACCESS WRITE LOCK READ WRITE AS #4
	PRINT #4, arnbd
	CLOSE #4


333 utfil$ = summa$ + "mn" + ar$ + man$ + ".txt"
	IF mannbd >= 0 THEN mannbd = mannbd + d1 / 10 ELSE mannbd = d1 / 10
	OPEN utfil$ FOR OUTPUT AS #4
	PRINT #4, mannbd
	CLOSE #4

	ON ERROR GOTO ERROR334
	utfil$ = "V:\" + "mn" + ar$ + man$ + ".txt"
	OPEN utfil$ FOR OUTPUT ACCESS WRITE LOCK READ WRITE AS #4
	PRINT #4, mannbd
	CLOSE #4

334 utfil$ = summa$ + "dn" + ar$ + man$ + dag$ + ".txt"
	IF dagnbd >= 0 THEN dagnbd = dagnbd + d1 / 10 ELSE dagnbd = d1 / 10
	OPEN utfil$ FOR OUTPUT AS #4
	PRINT #4, dagnbd
	CLOSE #4




91  REM 'arutflag = 0


92  REM 'manutflag = 0




93  dagutflag = 0




								   

94    REM 'RETURN



NetUppdat:

	ON ERROR GOTO ERROR276
	OPEN "v:\uppdat.txt" FOR OUTPUT ACCESS WRITE LOCK READ WRITE AS #4
	PRINT #4, netdatum$
	PRINT #4, nettimex$
	CLOSE #4

	OPEN "v:\vdata.txt" FOR OUTPUT ACCESS WRITE LOCK READ WRITE AS #4
	PRINT #4, da2, da10, dm10, dx10, fa2, fx2, fa10, fx10, qff, p3, t1i, t2i, t3i, rh, td
	PRINT #4, hmax, pmin, pmax, tmin, tmax, wc
	PRINT #4, nbd3, nbd6, nbd12, nbd24, nbdintens, d3, sikt3, kvn, lvn
	CLOSE #4
	sikt1 = sikt3: sikt2 = sikt3
	RETURN



REM -----Rutin f”r uppdatering av klockor i PC och Milos mot NASen ------


UppdateraPC:

ON ERROR GOTO ERROR278

	SHELL "net time \\vidar  /set /yes >nul"
REM    LOCATE 22, 8: COLOR 3
REM    PRINT "<Klocka uppdaterad "; timex$; ">";
REM    TidUppdFlag = 1
REM    COLOR 2
REM    RETURN



REM ---- Rutin f”r uppdatering av klocka i MILOS ------
REM ---- "open" skickas tre g†nger till porten -----

UppdateraMILOS:

	RETURN



LasningVdata:
REM LOCATE 11, 10: PRINT "nere i vdata"
REM INPUT "", q$
	vdatafil$ = "vdata.txt"
	tid$ = "uppdat.txt"

	infil1$ = summapp$ + vdatafil$
	infil2$ = summapp$ + tid$



   
   

REM ========== Tid f”r senaste uppdatering p† VIDAR =============


20  OPEN infil2$ FOR INPUT ACCESS READ SHARED AS #1
	INPUT #1, datum$
	INPUT #1, tid$
	CLOSE #1
	tidg$ = tid$
date1$ = RIGHT$(datum$, 5) + "-20" + LEFT$(datum$, 2)
timex$ = tid$

	timestamp$ = "20" + datum$ + " " + tid$
	
REM    LOCATE 14, 10: PRINT timestamp$: INPUT "", q$


REM     IF gdag$ <> RIGHT$(datum$, 2) THEN RUN
	uppdatmin$ = MID$(timex$, 4, 2)
	uppdatflag = 1: varv = 1
REM    LOCATE 1, 45: COLOR 3: PRINT sek; " "; timex$
   

REM =========== Inl„sning aktuella v„rden ===================


	OPEN infil1$ FOR INPUT ACCESS READ SHARED AS #1
	INPUT #1, da2, da10, dm10, dx10, fa2, fx2, fa10, fx10, qff, p3, t1i, t2i, t3i, rh, td
	INPUT #1, hmax, pmin, pmax, tmin, tmax, wc
	INPUT #1, nbd3, nbd6, nbd12, nbd24, nbdintens, d3, sikt3, kvn, lvn
	CLOSE #1
	sikt1 = sikt3: sikt2 = sikt3
REM    kvnkorr = .56 * t1i - 24.2
REM    kvn = kvn + kvnkorr

784 REM
   
REM LOCATE 16, 10: PRINT da10: INPUT "", q$

RETURN



Omstart:
CLOSE
OPEN "omstart.txt" FOR APPEND AS #1
PRINT #1, "omstart "; date1$; " "; TIME$
CLOSE #1

	 RUN


Vindriktning:
REM    da2 = 351: fa2 = 1: fx2 = 1
	IF da2 > 99 THEN vr1$ = RIGHT$(STR$(da2), 3)
	IF da2 < 100 THEN vr1$ = "0" + RIGHT$(STR$(da2), 2)
	IF da2 < 10 THEN vr1$ = "00" + RIGHT$(STR$(da2), 1)
	IF da2 > 360 THEN vr1$ = "---"

	xr = 22.5: xd = 11.25
	IF fx2 = 0 THEN vr$ = "Lugnt": GOTO 789
	vr$ = ""
REM    IF fa2 > 1000 AND VindUS > 30 THEN vr$ = "Hast U/S": GOTO 789
	IF da2 <= xd THEN vr$ = "  N  "
	IF da2 > 0 * xr + xd THEN vr$ = " NNE "
	IF da2 > 1 * xr + xd THEN vr$ = "  NE "
	IF da2 > 2 * xr + xd THEN vr$ = " ENE "
	IF da2 > 3 * xr + xd THEN vr$ = "  E  "
	IF da2 > 4 * xr + xd THEN vr$ = " ESE "
	IF da2 > 5 * xr + xd THEN vr$ = "  SE "
	IF da2 > 6 * xr + xd THEN vr$ = " SSE "
	IF da2 > 7 * xr + xd THEN vr$ = "  S  "
	IF da2 > 8 * xr + xd THEN vr$ = " SSW "
	IF da2 > 9 * xr + xd THEN vr$ = "  SW "
	IF da2 > 10 * xr + xd THEN vr$ = " WSW "
	IF da2 > 11 * xr + xd THEN vr$ = "  W "
	IF da2 > 12 * xr + xd THEN vr$ = " WNW "
	IF da2 > 13 * xr + xd THEN vr$ = "  NW "
	IF da2 > 14 * xr + xd THEN vr$ = " NNW "
	IF da2 > 15 * xr + xd THEN vr$ = "  N  "
	IF da2 > 360 THEN vr$ = "---"
789 REM LOCATE 2, xp + 14: PRINT "   "
	COLOR 15: LOCATE 2, xp + 14: PRINT vr1$;
	COLOR 8: LOCATE 3, xp + 13: PRINT vr$
	COLOR 15
	RETURN

