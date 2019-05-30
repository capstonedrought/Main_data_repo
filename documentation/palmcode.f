cd C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  00020002
C                                                                       00030002
C     MAIN PROGRAM: /disk13/cobprod/wd53th/wpdiupd/wpdud3.f
C                                                                       00050002
C     AUTHOR: HEDDINGHAUS T.R.   ORG: WD53   DATE: 24 MAR 83;           00060002
C     MODIFY: MISKUS, D.         ORG: WD53   DATE: 25 MAR 87            00070002
C     MODIFY: SABOL, P.          ORG: WD53   DATE: 27 OCT 89            00080002
C          CONVERTED TO VS FORTRAN (FORTRAN 77) ONLY.                   00090002
C          USE VS FORTRAN COMPILERS ONLY!!!                             00100000
C     MODIFY: SABOL, P.          ORG: WD53   DATE: 21 APR 95            00110010
C          CHANGE ORGANIZATION NAME ONLY!!!                             00120010
c             converted to sgi43 - 10 DEC 96
C                                                                       00130002
C     ABSTRACT: THIS ROUTINE COMPUTES THE PALMER DROUGHT, CROP          00140002
C       MOISTURE INDEX, AND PRECIPITATION NEEDED TO END A DROUGHT       00150002
C       FOR GIVEN WEEKS, YEAR, AND CLIMATE DIVISIONS. THIS PROGRAM WAS  00160002
C       OBTAINED FROM IRMA LEWIS AT NCDC AND MODIFIED BY T HEDDINGAUS.  00170002
C                                                                       00180002
C   USAGE:                                                              00190002
C     INPUT FILES:                                                      00200002
C     FT05F001 -  prod.wd53.data.new - PREVIOUS WEEKS DATA         
C     FT01F001 -  prod.wd53.wpdmastr - MASTER TAPE           
C     FT02F001 -  prod.wd53.datacard.clidiv - CLIMATE        
C                 DIVISION DIRECTORY                                    00240002
C     FT04F001 -  prod.wd53.pdi.week.old - PAST DATA
C                                                                       00260002
C   OUTPUT FILES:                                                       00270002
C     FT06F001 -  list.weekly.output3 - LISTING OF OUTPUT                         
C     FT09F001 -  prod.wd53.pdi.week.new - NEW PAST DATA           
C     FT10F001 -  prod.wd53.dum10
C                 FOR THE MONTHLY PALMER DROUGHT PROJECTION ROUTINE     00310002
C     FT11F001 -  prod.wd53.dum11 - OUTPUT FOR AFOS             
C                 AND MAPS FOR JAWF                                     00330002
C     FT12F001 -  prod.wd53.dum12 -  COMMUNICATIONS            
C                 FILE FOR CLIMATE DIVISIONS IN EASTERN REGION          00350002
C     FT13F001 -  prod.wd53.dum13 - COMMUNICATIONS                  
C                 FILE FOR CLIMATE DIVISIONS IN SOUTHERN REGION         00370002
C     FT14F001 -  prod.wd53.dum14 - COMMUNICATIONS               
C                 FILE FOE CLIMATE DIVISIONS IN CENTRAL REGION          00390002
C     FT15F001 -  prod.wd53.dum15 - COMMUNICATIONS               
C                 FILE FOR CLIMATE DIVISIONS IN WESTERN REGION          00410002
C                                                                       00420000
C   SUBPROGRAMS CALLED:                                                 00430002
C     UNIQUE:  -  PHA1,HARMON,RSTATE,WTITLE,XPHI,CTITLE,                00440002
C                 COMUN,CASE                                            00450002
C     W3LIB:   -  iw3jdn,w3fs26
C                                                                       00470002
C   EXIT STATES:                                                        00480002
C     COND = 0    SUCCESSFUL  RUN                                       00490002
C                                                                       00500002
C   REMARKS:  THIS PROGRAM IS DESIGNED TO COMPUTE PALMER DROUGHT        00510002
C             PRODUCTS FOR THE GROWING SEASON BEGINNING 1ST WEEK        00520000
C             IN MARCH. THE PROGRAM CAN RUN FOR ANY PERIOD FOR          00530002
C             65 WEEKS(NWKS). HOWEVER THE PROGRAM SHOULD BE             00540002
C             INITIALIZED FROM THE FEBRUARY PALMER MONTHLY RUN.         00550002
C                                                                       00560002
C ATTRIBUTES:                                                           00570002
C   LANGUAGE: FORTRAN 77                                                00580002
C   SOURCE STATEMENTS: 536                                              00590002
C                                                                       00600002
C$$$                                                                    00610002
C                                                                       00620000
      COMMON               K,DUM,DK,Q,IDUM                              00630002
      COMMON ITAB(3),TAB(40),VEC(9),B,SL,UL,RS,RU,IC,ISTN,IYR,IWK,DATE,T00640002
      COMMON P,REC(9),ID1,W,BB,HE,ALAT,C(60),AO(5),IP,JFL,LIK,KBX,JH    00650002
      REAL*4 SEC(9),PHI(65)                                             00660002
      INTEGER*4 NSTATE(196),JULN(65)                                    00670002
      EQUIVALENCE (TAB(1),PP),(TAB(2),TT),(TAB(3),SP),(TAB(4),UW),(TAB(500680002
     1),ZE),(TAB(6),PE),(TAB(7),PL),(TAB(8),PR),(TAB(9),R),(TAB(10),TL),00690002
     2(TAB(11),ET),(TAB(12),RO),(TAB(13),ALPHA),(TAB(14),BETA),(TAB(15),00700002
     3GAMMA),(TAB(16),DELTA),(TAB(17),AKAP),(TAB(18),CET),(TAB(19),CR), 00710002
     4(TAB(20),CRO),(TAB(21),CL),(TAB(22),CP),(TAB(23),CD),(TAB(24),Z), 00720000
     5(TAB(25),SSS),(TAB(26),SSU),(TAB(27),PV),(TAB(28),PRO),(TAB(29),PX00730002
     61),(TAB(30),PX2),(TAB(31),PX3),(TAB(32),PG),(TAB(33),YP),(TAB(34),00740002
     7PDI),(TAB(35),UD),(TAB(36),DE),(TAB(37),CI),(TAB(38),PY),(TAB(39) 00750002
     8,GM),(ITAB(1),ID),(ITAB(2),IY),(ITAB(3),IW),(SEC(1),TAB(25)),     00760002
     9(TAB(40),CC)
      EQUIVALENCE (VEC(1),SS),(VEC(2),SU),(VEC(3),V),(VEC(4),PROB),     00780002
     1(VEC(5),X1),(VEC(6),X2),(VEC(7),X3),(VEC(8),GI),(VEC(9),YI)       00790002
      EQUIVALENCE (REC(1),RSS),(REC(2),RSU),(REC(3),RV),(REC(4),RPRO),  00800002
     1(REC(5),RX1),(REC(6),RX2),(REC(7),RX3),(REC(8),RG),(REC(9),RY)    00810002
C                                                                       00820000
C      PHI  -  SOLAR DECLINATION FOR NWKS WENESDAYS                     00830002
C      JULN -  JULIAN DATES FOR WENESDAYS OF NWKS WEEKS                 00840002
C      NWKS -  NUMBER OF WEEKS COMPUTATIONS CAN BE MADE FOR THE SEASON  00850002
C      JULS -  JULIAN DATE OF WENESDAY OF WEEK ONE                      00860002
C                                                                       00870002
      DATA NWKS/65/                                                     00880002
      open(unit=1,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.wpdmastr',
     + blank='zero',status='unknown',access='sequential')
      open(unit=2,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.datacard.clidiv',
     + blank='zero',status='unknown',access='sequential')
      open(unit=4,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.pdi.week.old',
     + blank='zero',status='unknown',access='sequential')
      open(unit=5,file=
     + '/disk13/cobprod/wd53th/wpdiupd/ncdc.conv',
     + blank='zero',status='unknown',access='sequential')
      open(unit=6,file=
     + '/disk13/cobprod/wd53th/wpdiupd/listing.weekly.output3',
     + blank='zero',status='unknown',access='sequential')
      open(unit=9,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.pdi.week.new',
     + blank='zero',status='unknown',access='sequential')
      open(unit=10,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.dum10',
     + blank='zero',status='unknown',access='sequential')
      open(unit=11,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.dum11',
     + blank='zero',status='unknown',access='sequential')
      open(unit=12,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.dum12',
     + blank='zero',status='unknown',access='sequential')
      open(unit=13,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.dum13',
     + blank='zero',status='unknown',access='sequential')
      open(unit=14,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.dum14',
     + blank='zero',status='unknown',access='sequential')
      open(unit=15,file=
     + '/disk13/cobprod/wd53th/wpdiupd/prod.wd53.dum15',
     + blank='zero',status='unknown',access='sequential')     
C     READ IN STARTING JULIAN DATE,CURRENT WEEK NUMBER,AND YEAR         00900002
      READ(5,113) JULS,JWK,JYR,myr,mmo,mda
 113  FORMAT(2I3,2i5,2i3)                                               00920000
C     READ IN STATE NAMES FROM DIRECTORY                                00930002
      CALL RSTATE(NSTATE)                                               00940002
C     COMPUTE SOLAR DECLINATION ANGLES AND JULIAN DATES FOR ALL         00950002
C     NWKS WEEKS                                                        00960002
      CALL XPHI(PHI,JULN,JULS,NWKS,JYR)                                 00970002
 112  FORMAT(1x)                                                        00980002
      JDIV=0                                                            00990002
      JFL=0                                                             01000002
      LIK=0                                                             01010002
      LINE=0                                                            01020000
C      COMPUTE JULIAN DATE OF MID 4 WEEKS - DATEM                       01030002
       JWK4=JWK-3                                                       01040002
       CDMN=0.                                                          01050002
       DATE=JULN(JWK)                                                   01060002
       DATEM=DATE-10.                                                   01070002
       IF(DATEM.GT.0.) GO TO 998                                        01080002
       DATEM=355+DATE                                                   01090002
       LEAP=MOD(JYR,4)                                                  01100002
       IF(LEAP.EQ.0) DATEM=356+DATE                                     01110002
  998  CONTINUE                                                         01120000
      KBX=0                                                             01160002
C                                                                       01170002
C      READ ON UNIT 5:                                                  01180002
C              ISTN - CLIMATE DIVISION NUMBER                           01190002
C              IYR  - YEAR                                              01200002
C              IWK  - WEEK NUMBER (1-NWKS)                              01210002
C              T    - AVERAGE WEEKLY TEMPERATURE (F) FOR CLI DIV        01220000
C              P    - TOTAL WEEKLY PRECIPITATION (INCHES) FOR CLI DIV   01230002
C              RSS  - AVAILABLE MOISTURE IN SURFACE SOIL (0-1. INCHES)  01240002
C              RSU  - AVAILABLE MOISTURE IN UNDERLYING SOIL             01250002
C              RV   - ACCUMULATED VALUE OF UD OR UW                     01260002
C              RPRO - PRECENTAGE PROBABILITY THE WEATHER SPELL HAS ENDED01270002
C              RX1  - SEVERITY INDEX FOR WET SPELL BECOMING ESTABLISHED 01280002
C              RX2  - SEVERITY INDEX FOR A DROUGHT BECOMING ESTABLISHED 01290002
C              RX3  - SEVERITY INDEX FOR A WET SPELL OR DROUGHT THAT    01300002
C                     HAS BECOME ESTABLISHED                            01310002
C              RG   - GRAVITATION WATER INDEX - OR EXCESSIVE MOISTURE   01320000
C                      INDEX  -  GT OR EQ 0.                            01330002
C              RY   - INDEX OF THE EVAPOTRANSPIRATION ANOMALY           01340002
C              IC=1 - ACCEPT INFORMATION FROM RSS TO RY IN PREFERENCE   01350002
C                     TO THAT ON THE PAST HISTORY DATA TAPE             01360002
C              JH=1 - NO PAST HISTORY DATA TAPE IS AVAILABLE            01370002
      READ(5,21) ISTN,IYR,IWK,T,P,     RSS,RSU,RV,RPRO,RX1,RX2,RX3,RG,  01380002
     1RY,IC,JH                                                          01390002
      IF (IWK.GT.NWKS) GO TO 38                                         01400002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++01410002
C                                                                       01420000
C  IC=1       TAKE BEGINNING INFORMATION FROM CARD(SS,SU,V,PROB,X1,X2,X301430002
C  JH=1      NO PAST DATA TAPE      (JH=0 OR BLANK THERE IS A PDT)      01440002
C                                                                       01450002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++01460002
 21   FORMAT (2I4,I2,F4.0,F5.1,F4.2,2F5.2,F3.0,5F5.2,I1,1X,I1)          01470002
C     WRITE TITLES FOR 1ST STATE ONTO UNIT 6                            01480002
      CALL WTITLE(myr,mmo,mda,ISTN,NSTATE)   
C     WRITE TITLES FOR 4 COMMINICATIONS FILES                           01500002
      CALL CTITLE(jyr,jwk,myr,mmo,mda,jmo3)                             01510002
      jmo1=mmo
      kyr=myr
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++01520000
C                                                                       01530002
C      READ IN THE MASTER TAPE WHICH CONTAINS PARAMETERS FOR EACH       01540002
C       CLIMATE DIVISION.                                               01550002
C         ID1  - CLI DIV NUMBER OR STATE DIVISION IDENTIFIER            01560002
C         W    - WATER CAPACITY OF UNDERLYING SOIL (INCHES)             01570002
C                (W + 1 = TOTAL WATER CAPACITY)                         01580002
C         BB   - B COEFFICIENT (CLI DIV CONSTANT). USED TO COMPUTE PE   01590002
C                (POTENTIAL EVAPOTRANSPIRATION) BY THORNWAITES METHOD   01600002
C         HE   - HEAT INDEX. PARAMETER COMPUTED FROM PAST MONTHLY TEMPS 01610002
C                FOR EACH CLI DIV.  SAME USE AS BB.                     01620000
C         ALAT - NEGATIVE TANGENT OF CLI DIV LATITUDE. SAME USE AS BB.  01630002
C         C(60) - ARRAY CONTAINING AMPLITUDES AND PHASES OF ALPHA,BETA, 01640002
C                 DELTA,GAMMA,& K COEFFICENTS OF 6 HARMONICS TO COMPUTE 01650002
C                 WEEKLY VALUES FROM THE MONTHLY VALUES.                01660002
C                 USED TO COMPUTE WEEKLY CAFEC PARAMETERS.              01670002
C          C(1) - AMPLITUDE OF FIRST HARMONIC(12 MO PERIOD) OF ALPHA    01680002
C          C(2) - PHASE OF FIRST HARMONIC OF ALPHA                      01690002
C          C(3) - AMPLITUDE OF SECOND HARMONIC OF ALPHA                 01700002
C                 ...........                                           01710002
C          C(12)- PHASE OF SIXTH HARMONIC OF ALPHA                      01720000
C                 ...........                                           01730002
C          C(60)- PHASE OF SIXTH HARMONIC OF K COEFFICIENT              01740002
C         AO(5) - MEANS OF ALPHA,BETA,GAMMA,DELTA,K - SAME USE AS C     01750002
C          AO(1)- MEAN OF ALPHA                                         01760002
 2    READ (1,1,END=888,ERR=888)    ID1,W,BB,HE,ALAT,C,AO               01770002
 1    FORMAT(I4,2F5.2,F4.0,F7.4,65F11.6)                                01780002
      IF  (ID1-ISTN)  2,3,36                                            01790002
 3    DO 4 J=1,9                                                        01800002
 4    VEC(J)=REC(J)                                                     01810002
      IF (JH.NE.1) GO TO 6                                              01820000
      CI= RG+RY                                                         01830002
      IF (IC.EQ.1) GO TO 10                                             01840002
 81   WRITE (6,919)                                                     01850002
 919  FORMAT (2x,'NO HEADER INFO AND NO PAST DATA TAPE INVESTIGATE.')   01860002
      GO TO 6666                                                        01870002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++01880002
C                                                                       01890002
C  FIND STA. ON PAST DATA TAPE                                          01900002
C                                                                       01910002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++01920000
 6    IF(LIK.EQ.1) GO TO 10                                             01930002
C      READ DATA FROM PAST HISTORY. CONTAINS:                           01940002
C      ITAB(4) - (1) ID - CLI DIV NUMBER                                01950002
C                (2) IY - YEAR                                          01960002
C                (3) IW - WEEK NUMBER (1-NWKS)                          01970002
C                (4) KFL                                                01980002
C      TAB(40) - (1) PP - PRECIPITATION TOTAL (INCHES)                  01990002
C                (2) TT - TEMPERATURE AVERAGE (F)                       02000002
C                (3) SP - AVAILABLE MOISTURE  IN BOTH LAYERS AT         02010002
C                         START OF WEEK                                 02020000
C                (4) UW - EFFECTIVE WETNESS IN ENDING A DROUGHT         02030002
C                (5) ZE - MOISTURE ANOMALY REQUIRED TO END              02040002
C                         "WEATHER SPELL IN A SINGLE WEEK"              02050002
C                (6) PE - POTENTIAL EVAPORTRANSPIRATION (THORNTHWAITE)  02060002
C                (7) PL - POTENTIAL LOSS                                02070002
C                (8) PR - POTENTIAL RECHARGE                            02080002
C                (9) R  - RECHARGE                                      02090002
C               (10) TL - MOISTURE LOSS                                 02100002
C               (11) ET - EVAPORTRANSPIRATION                           02110002
C               (12) RO - RUNOFF                                        02120000
C               (13) ALPHA - COEFFICIENT OF EVAPORATION                 02130002
C               (14) BETA - COEFFICIENT OF RECHARGE                     02140002
C               (15) GAMMA - COEFFICIENT OF RUNOFF                      02150002
C               (16) DELTA - COEFFICIENT OF MOISTURE DELETION           02160002
C               (17) AKAP(K COEFFICIENT) - CLIMATIC CHARACTERISTIC      02170002
C               (18) CET - ESTIMATED(CAFEC) EVAPORTRANSPIRATION         02180002
C               (19) CR - ESTIMATED(CAFEC) RECHARGE                     02190002
C               (20) CRO - ESTIMATED(CAFEC) RUNOFF                      02200002
C               (21) CL - ESTIMATED(CAFEC) LOSS                         02210002
C               (22) CP - ESTIMATED(CAFEC) PRECIPITATION                02220000
C               (23) CD - (P-CP)                                        02230002
C               (24) Z  - MOISUTURE ANOMALY INDEX                       02240002
C               (25) SSS - AVAILABLE MOISTURE IN SURFACE LAYER          02250002
C               (26) SSU - AVAILABLE MOISTURE IN UNDERLYING LAYER       02260002
C               (27) PV - ACCUMULATED VALUE OF UW OR UD                 02270002
C               (28) PRO - PROBABILTY WEATHER SPELL HAS ENDED           02280002
C               (29) PX1 - X1 TERM                                      02290002
C               (30) PX2 - X2 TERM                                      02300002
C               (31) PX3 - X3 TERM                                      02310002
C               (32) PG - GRAVITATIONAL WATER INDEX                     02320000
C               (33) YP - INDEX OF EVAPOTRANSPIRATION ANOMALY           02330002
C               (34) PDI  - MODIFIED PALMER DROUGHT INDEX               02340011
C               (35) UD - EFFECTIVE DRYNESS IN ENDING A WET SPELL       02350002
C               (36) DE -  (ET-CET)/SQRT(ALPHA)                         02360002
C               (37) CI - CROP MOISTURE INDEX (CMI)                     02370002
C               (38) PY -  YP PRIME                                     02380002
C               (39) GM - AVERAGE PERCENT OF FIELD CAPACITY DURING WEEK 02390002
C               (40) CC - CHANGE IN CMI FROM PREVIOUS WEEK              02400002
 7    READ   (4,5,END=42,ERR=888) ITAB,TAB                              02410002
      IF(IW.GE.JWK4.AND.ISTN.EQ.ID) CDMN=CDMN+CD                        02420000
 5    FORMAT(2I4,I2,F4.1,F4.0,10F6.2,4F6.4,F6.3,10F6.2,F4.0,12F6.2) 
 777  IF (ISTN-ID) 210,8,200                                            02440002
 200  KFL=0                                                             02450002
      WRITE (9,5) ITAB,TAB                                              02460002
      GO TO 6                                                           02470002
 210  IF (IC.EQ.1) GO TO 108                                            02480002
      GO TO 81                                                          02490002
 8    IF (IC.EQ.1) GO TO 106                                            02500002
      DO 105 I=1,9                                                      02510002
 105  VEC(I)=SEC(I)                                                     02520000
 1058 IDUM=IW+1                                                         02530002
      IF (IDUM-IWK) 200,9,109                                           02540002
 106  IF (ID.NE.ISTN) GO TO 108                                         02550002
      READ (4,5,END=107,ERR=888) ITAB,TAB                               02560002
      IF(IW.GE.JWK4.AND.ISTN.EQ.ID) CDMN=CDMN+CD                        02570002
      CI= RG+ RY                                                        02580002
 107  IF (ISTN.NE.6606) GO TO 88                                        02590002
      GO TO 10                                                          02600002
 108  BACKSPACE  4                                                      02610002
      GO TO  10                                                         02620000
 42   LIK=1                                                             02630002
      GO TO 10                                                          02640002
 109  WRITE (6,1099)                                                    02650002
 1099  FORMAT (5X,' PDT WK .GT.CARD WK ')                               02660002
      GO TO 6666                                                        02670002
  9   WRITE (9,5) ITAB,TAB                                              02680002
      KFL= 0                                                            02690002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++02700002
C      BEGIN CALAULATIONS                                               02710002
C     CALCULATE VALUES NEEDED IN THRONWAITES FORMULA TO COMPUTE PE      02720000
C      PHI  -  SOLAR DECLINATION , FUNCTION OF IWK                      02730002
C      FINAL DK IS THE K SUB D TERM IN THRONWAITES FORMULAS TO COMPUTE  02740002
C       PE FOR (32F LT T LT 80F) AND (T GE 80F)                         02750002
 10   DUM=ALAT*((SIN(PHI(IWK)))/(COS(PHI(IWK))))                        02760002
      DK=ATAN(SQRT(1.-DUM*DUM)/DUM)                                     02770002
       IF (DK.GT.0.0) GO TO 51                                          02780002
      DK=DK+3.141593                                                    02790002
 51   DK=(DK+.0157)/1.57                                                02800002
C      CALL PHAI - COMPUTES CMI, Z INDEX, AND RELATED TERMS             02810002
      CALL PHA1(JULN,DATEM,AKMN)                                        02820000
      ID=ISTN                                                           02830002
      IY=IYR                                                            02840002
      IW=IWK                                                            02850002
      KFL=1                                                             02860002
      TT=T                                                              02870002
      PP=P                                                              02880002
      IF (PROB-0.0) 20,22,101                                           02890002
 20   WRITE (6,211)PROB                                                 02900002
 211  FORMAT (2x,'PROBABILITY =',E14.8)                                 02910002
      GO TO 6666                                                        02920000
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++02930002
 22   IF (X3.GT..5) GO TO 59                                            02940002
      IF (X3+.5) 23,100,100                                             02950002
 23   IF (Z+.04) 25,95,95                                               02960002
 25   PV=0.0                                                            02970002
      PRO=0.0                                                           02980002
      PX3=.975*X3+Z/3.                                                  02990002
      PX2=0.0                                                           03000002
      PX1=0.0                                                           03010002
 29   ISTAT= ISTN/100                                                   03020000
C      THIS IF STATEMENT INSERTED BY T HEDDINGHUAS TO HANDLE            03030002
C       RARE SITUATIONS                                                 03040002
       IF(PRO.GT.100..OR.PRO.LT.0.) PRO=0.                              03050002
      IT= TT                                                            03060002
      IPR= PRO +.5                                                      03070002
      IDIV= ISTN- ISTAT*100                                             03080002
 28   FORMAT (F5.1,I3,F6.2,F5.2,2F6.2,F5.1,F6.2,F6.3,7F6.2,I5,4F6.2,1X, 03090002
     22I4,I3)                                                           03100002
C                                                                       03110002
      IF (LINE.LT.400) GO TO 30                                         03120000
      CALL WTITLE(myr,mmo,mda,ISTN,NSTATE)                              03130002
      LINE= 0                                                           03140002
  30  CONTINUE                                                          03150002
c     check when pro gt 99.5 & lt 100. 
      if(pro.ge.99.5.and.pro.lt.100.) then
       pro=100.0
       ipr=100
       px2abs=abs(px2)
        px3abs=abs(px3)
        if(px1.ge.px3abs) then
         px3=px1
         px1=0.
        endif
        if(px2abs.ge.px3abs) then
         px3=px2
         px2=0.
        endif
      endif                   
      IF(IDIV.NE.JDIV) WRITE(6,112)                                     03160002
      JDIV=IDIV                                                         03170002
C     WRITE DATA TO COMMINICATIONS FILES & COMPUTE PDI & PADD           03180002
      CALL COMUN(ISTAT,IDIV,IT,IPR,X1,X2,X3,JWK,JMO1,CDMN,AKMN,JWK4,    03190002
     1KYR,JMO3,PROB,PADD)                                               03200002
C     COMPUTATIONS OF ALL PARAMETERS FOR WK CD COMPLETED-WRITE TO PAPER 03210002
      WRITE(6,28)PP,IT,SP,SSS,SSU,PE,PR,ET,ALPHA,CET,CP,CD,Z,PX1,PX2,   03220000
     1PX3,IPR,PDI,PADD,CI,CC,IW,ISTAT,IDIV                              03230002
      LINE= LINE+1                                                      03240002
      X= 0.0                                                            03250002
      WRITE (9,5) ITAB,TAB                                              03260002
C                                                                       03270002
C  2ND READ                                                             03280002
 31   READ(5,21,END=26,ERR=888)ISTN,IYR,IWK,T,P,RSS,RSU,RV,RPRO,RX1,RX2,03290002
     1RX3,RG,RY,IC,JHH                                                  03300002
C                                                                       03310002
 331  IF (IWK.GT.NWKS) GO TO 38                                         03320000
      IF (ID.NE.ISTN) GO TO 43                                          03330002
 322  IF (IWK.EQ.(IW+1)) GO TO 311                                      03340002
      IF (IWK.EQ.1) GO TO 311                                           03350002
      WRITE(6,32) IW,IWK                                                03360002
 32   FORMAT (1X,'OLD WK=',I2,'  NEW WK=',I2)                           03370002
      GO TO 6666                                                        03380002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++03390002
 311  DO 321 J=1,9                                                      03400002
 321  VEC(J)=SEC(J)                                                     03410002
      GO TO 10                                                          03420000
 35   READ (1,1,END=888,ERR=888)    ID1,W,BB,HE,ALAT,C,AO               03430002
      IF (ID1-ISTN) 35,3,36                                             03440002
 36   WRITE (6,37)  ISTN,ID1                                            03450002
      GO TO 6666                                                        03460002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++03470002
 37   FORMAT (2x,'THIS STN IS NOT ON MASTER TAPE',2I6)                  03480002
 38   WRITE  (6,39)   ID,ISTN,IWK                                       03490002
 39   FORMAT(2x,'STATIONS OUT OF SEQUENCE',I4,3X,I4,'OR WEEK=',I3)      03500002
C                                                                       03510002
      GO TO 27                                                          03520000
 26   IF (ISTN.EQ.6606.AND.ID1.EQ.6606) GO TO 27                        03530002
 266  IF (JH.EQ.1) GO TO 27                                             03540002
      IF (LIK.EQ.1) GO TO 27                                            03550002
 267   READ (4,5,END=27,ERR=888) ITAB,TAB                               03560002
 2667 IF (ID.EQ.ISTN) GO TO 267                                         03570002
      WRITE (9,5) ITAB ,TAB                                             03580002
      GO TO 267                                                         03590002
 27    ENDFILE 9                                                        03600002
 277  WRITE (6,278)                                                     03640002
 278  FORMAT (2X,'END OF JOB')                                          03650002
 6666 CONTINUE                                                          03660002
      close(unit=1)
      close(unit=2)
      close(unit=4)
      close(unit=5)
      close(unit=6)
      close(unit=9)
      close(unit=10)
      close(unit=11)
      close(unit=12)
      close(unit=13)
      close(unit=14)
      close(unit=15)
      STOP                                                              03680002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++03690002
 43   IF (ISTN/100.EQ.ID/100) GO TO 444                                 03700002
C     WRITE TITLES FOR EACH STATE ONTO UNIT 6                           03710002
      CALL WTITLE(myr,mmo,mda,ISTN,NSTATE)                              03720000
      LINE= 0                                                           03730002
 444  IF (JH.EQ.1) GO TO 35                                             03740002
 44   READ (4,5,END=45,ERR=888) ITAB,TAB                                03750002
      IF(IW.GE.JWK4.AND.ISTN.EQ.ID) CDMN=CDMN+CD                        03760002
      IF (ID.EQ.ISTN) GO TO 48                                          03770002
      IF (ID1.EQ.ID) GO TO 44                                           03780002
      IF (ID.GT.ISTN) GO TO 45                                          03790002
      KFL=0                                                             03800002
      WRITE (9,5) ITAB,TAB                                              03810002
      GO TO 44                                                          03820000
 45   IF (IC.EQ.1) GO TO 108                                            03830002
      GO TO 38                                                          03840002
 48   READ (1,1,END=36,ERR=888) ID1,W,BB,HE,ALAT,C,AO                   03850002
      IF (ID1.NE.ISTN) GO TO 48                                         03860002
      DO 49 KJ=1,9                                                      03870002
 49   VEC(KJ)= REC(KJ)                                                  03880002
      GO TO 8                                                           03890002
 888  WRITE(6,50)                                                       03900002
 50   FORMAT (' ERROR IN READ')                                         03910002
      GO TO 88                                                          03920000
C41   IF (ISTN.NE.0) GO TO 38                                           03930002
C     IF (ID-ISTN) 35,35,41                                             03940002
 59   IF (Z-.04)  60,25,25                                              03950002
 60   UD=Z-.04                                                          03960002
      IF (V.GT.0) GO TO 93                                              03970002
      PV=UD+V                                                           03980002
 62   IF (PV.GE.0.) GO TO 25                                            03990002
      ZE=-2.925*X3+1.5                                                  04000002
 64   IF (PROB.GE.100.) GO TO 77                                        04010002
      Q=ZE+V                                                            04020000
 66   PRO=(PV/Q)*100.                                                   04030002
      IF (PRO.LT.100.) GO TO 94                                         04040002
      PRO=100.                                                          04050002
 68   PX3=0.0                                                           04060002
 69   IF (X1-0.0) 92,92,70                                              04070002
 70   PX1=.975*X1+Z/3.                                                  04080002
      IF (PX1-0.0)   71,89,89                                           04090002
 71   PX1=0.0                                                           04100002
 72   IF (X2-0.0) 73,87,87                                              04110002
 73   PX2=.975*X2+Z/3.                                                  04120000
      IF (PX2-0.0)  84,74,74                                            04130002
 74   PX2=0.0                                                           04140002
 75   IF (PX3-0.0)  76,78,76                                            04150002
 76   GO TO 29                                                          04160002
 77   Q=ZE                                                              04170002
      GO TO 66                                                          04180002
 78   IF (PX1.EQ.0.) GO TO 82                                           04190002
      IF (PX2.NE.0.) GO TO 76                                           04200002
 82      GO TO 29                                                       04210002
 84   IF (PX2+1.0) 85,85,75                                             04220000
 85   IF (PX3.NE.0.) GO TO 76                                           04230002
      PX3=PX2                                                           04240002
      PX2=0.0                                                           04250002
      GO TO 29                                                          04260002
 87   IF (Z-0.0) 73,74,74                                               04270002
 88   GO TO 6666                                                        04280002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++04290002
 89   IF (PX1.LT.1.) GO TO 72                                           04300002
      IF (PX3.NE.0.) GO TO 72                                           04310002
      PX3=PX1                                                           04320000
      PX2=.975*X2+Z/3.                                                  04330002
      IF(PX2.GT.0.) PX2=0.                                              04340002
      PX1=0.0                                                           04350002
      GO TO 29                                                          04360002
 92   IF (Z-0.0)71,71,70                                                04370002
 94   PX3=.975*X3+Z/3.                                                  04380002
      GO TO 69                                                          04390002
 93   PV=UD                                                             04400002
      GO TO 62                                                          04410002
 95   UW=Z+.04                                                          04420000
      IF (V.LE.0.) GO TO 99                                             04430002
      PV=UW+V                                                           04440002
 97   IF (PV.LE.0.) GO TO 25                                            04450002
      ZE=-2.925*X3-1.5                                                  04460002
      GO TO  64                                                         04470002
 99   PV=UW                                                             04480002
      GO  TO 97                                                         04490002
 100  PV=0.0                                                            04500002
      PRO=0.0                                                           04510002
      GO TO 68                                                          04520000
 101  IF (PROB-100.) 102,22,20                                          04530002
 102  IF (X3-0.0)  95,95,60                                             04540002
C103  GO TO 29                                                          04550002
      END                                                               04560002
      SUBROUTINE PHA1(JULN,DATEM,AKMN)                                  04570002
C                                                                       04580002
C      THIS SUBROUTINE COMPUTES THE WEEKLY MOISTURE ANOMALY Z INDEX,    04590002
C      PE, CAFEC PARAMETERS, PR,PL AND OTHER TERMS.                     04600002
C      ALSO COMPUTES CMI AND RELATED TERMS.                             04610002
C                                                                       04620000
C      SUBROUTINES CALLED - HARMON                                      04630002
C                                                                       04640002
C                                                                       04650002
      COMMON                                            K,DUM,DK,Q,IDUM 04660002
      COMMON ITAB(3),TAB(40),VEC(9),B,SL,UL,RS,RU,IC,ISTN,IYR,IWK,DATE,T04670002
      COMMON P,REC(9),ID1,W,BB,HE,ALAT,C(60),AO(5),IP,JFL,LIK,KBX,JH    04680002
      REAL*4 SEC(9)                                                     04690002
      INTEGER*4 JULN(1)                                                 04700002
      EQUIVALENCE (TAB(1),PP),(TAB(2),TT),(TAB(3),SP),(TAB(4),UW),(TAB(504710002
     1),ZE),(TAB(6),PE),(TAB(7),PL),(TAB(8),PR),(TAB(9),R),(TAB(10),TL),04720000
     2(TAB(11),ET),(TAB(12),RO),(TAB(13),ALPHA),(TAB(14),BETA),(TAB(15),04730002
     3GAMMA),(TAB(16),DELTA),(TAB(17),AKAP),(TAB(18),CET),(TAB(19),CR), 04740002
     4(TAB(20),CRO),(TAB(21),CL),(TAB(22),CP),(TAB(23),CD),(TAB(24),Z), 04750002
     5(TAB(25),SSS),(TAB(26),SSU),(TAB(27),PV),(TAB(28),PRO),(TAB(29),PX04760002
     61),(TAB(30),PX2),(TAB(31),PX3),(TAB(32),PG),(TAB(33),YP),(TAB(34),04770002
     7PDI),(TAB(35),UD),(TAB(36),DE),(TAB(37),CI),(TAB(38),PY),(TAB(39) 04780002
     8,GM),(ITAB(1),ID),(ITAB(2),IY),(ITAB(3),IW),(SEC(1),TAB(25)),     04790002
     9(TAB(40),CC)
      EQUIVALENCE (VEC(1),SS),(VEC(2),SU),(VEC(3),V),(VEC(4),PROB),     04810002
     1(VEC(5),X1),(VEC(6),X2),(VEC(7),X3),(VEC(8),GI),(VEC(9),YI)       04820000
C                                                                       04830002
      R435=1./4.35                                                      04840002
      DATE= JULN(IWK)                                                   04850002
C       INITIALIZE TAB ARRAY TO ZERO                                    04860002
      DO 9 I=1,36                                                       04870002
 9    TAB(I)=0.0                                                        04880002
      RU=0.                                                             04890002
      SP=SS+SU                                                          04900002
      PR=W+1.-SP                                                        04910002
      IF (T-32.) 11,11,47                                               04920000
 11   PE=0.0                                                            04930002
 12   IF ((SS-PE)-0.0) 13,50,50                                         04940002
 13   PL= SS+((PE-SS)*SU)/(W+1.)                                        04950002
      IF (PL.LE.SP) GO TO 14                                            04960002
      PL=SP                                                             04970002
 14   B=P-PE                                                            04980002
      IF (B-0.0)  15,53,53                                              04990002
 15   R=0.0                                                             05000002
      IF (SS-ABS(B)) 16,51,51                                           05010002
 16   SL=SS                                                             05020000
      SSS=0.0                                                           05030002
      UL=(ABS(B)-SL)*SU/(W+1.)                                          05040002
      IF  (UL-SU)  17,17,52                                             05050002
 17   SSU=SU-UL                                                         05060002
 18   TL=SL+UL                                                          05070002
      RO=0.0                                                            05080002
      ET=P+SL+UL                                                        05090002
C      COMPUTE Z INDEX AND CAFEC PARAMETERS                             05100002
 19   CALL HARMON   (ALPHA,1,DATE,1)                                    05110002
      CET=ALPHA*PE                                                      05120000
      CALL HARMON   (BETA,13,DATE,2)                                    05130002
      CR=BETA*PR*R435                                                   05140002
      CALL HARMON    (GAMMA,25,DATE,3)                                  05150002
      CRO=GAMMA*SP*R435                                                 05160002
      CALL HARMON    (DELTA,37,DATE,4)                                  05170002
      CL=DELTA*PL                                                       05180002
      CP=CET+CR+CRO-CL                                                  05190002
      CD=P-CP                                                           05200002
      CALL HARMON    (AKAP,49,DATE,5)                                   05210002
      Z=AKAP*CD                                                         05220000
C     COMPUTE K COEFFICENT FOR MID 4 WEEKS                              05230002
      CALL HARMON(AKMN,49,DATEM,5)                                      05240002
C      COMPUTE CMI AND RELATED TERMS                                    05250002
C       COMPUTE AVERAGE PERCENT OF FIELD CAPACITY DURING THE WEEK - GM  05260002
      GM=.5*(SP+SSS+SSU)/(W+1.)                                         05270002
C         IF GI=0  -  H=0                                               05280002
      IF (GI.NE.0.) GO TO 21                                            05290002
      H=0.                                                              05300002
 20   PG=GI-H+RO+GM*R                                                   05310002
      IF (ALPHA.EQ.0.) GO TO 200                                        05320000
      DE=(ET-CET)/SQRT(ALPHA)                                           05330002
 201  YP= .67*YI+1.8*DE                                                 05340002
      IF (YP.LT.0.) PY=YP                                               05350002
      IF (YP.GE.0.) PY=YP*GM                                            05360002
      CC= PY+PG-CI                                                      05370002
      CI= PY+PG                                                         05380002
      RETURN                                                            05390002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++05400002
 200  DE= 0.                                                            05410002
      GO TO 201                                                         05420000
C         IF 0 LT GI LT .5  -  H=GI                                     05430002
 21   IF (GI.GT..50) GO TO 22                                           05440002
      H=GI                                                              05450002
      GO TO 20                                                          05460002
C         IF .5 LE GI LE 1.  -  H = .5                                  05470002
 22   IF (GI.GT.1.) GO TO 23                                            05480002
      H=.5                                                              05490002
      GO TO 20                                                          05500002
C         IF GI GT 1.  -  H = .5 * GI                                   05510002
 23   H=.5*GI                                                           05520000
      GO TO 20                                                          05530002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++05540002
C      COMPUTE PE FOR T GT 32 F                                         05550002
 47   IF  (T-80.)  48,49,49                                             05560002
C      COMPUTE PE FOR 32F LT T LT 80F                                   05570002
 48   DUM= LOG(T-32.)                                                   05580002
      PE=(EXP( -3.863233+BB*1.715598-BB* LOG(HE)+BB*DUM))*DK*7.         05590002
      GO TO 12                                                          05600002
C      COMPUTE FOR T GE 80F                                             05610002
 49   PE=(SIN(T/57.3-.166)-.76)*DK*7.                                   05620000
      GO TO 12                                                          05630002
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++05640002
C      COMPUTE HYDROLOGIC TERMS                                         05650002
 50   PL=PE                                                             05660002
      GO TO 14                                                          05670002
 51   SL=ABS(B)                                                         05680002
      SSS=SS-SL                                                         05690002
      UL=0.0                                                            05700002
      SSU=SU                                                            05710002
      GO TO 18                                                          05720000
 52   UL=SU                                                             05730002
      GO TO 17                                                          05740002
 53   ET=PE                                                             05750002
      TL=0.0                                                            05760002
      IF  (B-(1.-SS)) 54,54,55                                          05770002
 54   R=B                                                               05780002
      SSS=SS+B                                                          05790002
      SSU=SU                                                            05800002
      RO=0.0                                                            05810002
      GO TO 19                                                          05820000
 55   RS=1.-SS                                                          05830002
      SSS=1.                                                            05840002
      D=B-RS                                                            05850002
      E=W-SU                                                            05860002
      IF (E-D)  56,58,58                                                05870002
 56   RU=E                                                              05880002
      RO=D-RU                                                           05890002
 57   SSU=SU+RU                                                         05900002
      R=RS+RU                                                           05910002
      GO TO 19                                                          05920000
 58   RU=D                                                              05930002
      RO=0.0                                                            05940002
      GO TO 57                                                          05950002
      END                                                               05960002
      SUBROUTINE  HARMON    (AA,II,TIME,JJ)                             05970002
C                                                                       05980002
C      THIS SUBROUTINE COMPUTES THE WEEKLY ALPHA,BETA,GAMMA,DELTA, AND  05990002
C       K COEFFICENTS FROM THE GIVEN MONTHLY VALUES BY SUMMING THEIR    06000002
C       MEANS AND SIX HARMONIC COMPONENTS.                              06010002
C                                                                       06020000
      COMMON                                            K,DUM,DK,Q,IDUM 06030002
      COMMON ITAB(3),TAB(40),VEC(9),B,SL,UL,RS,RU,IC,ISTN,IYR,IWK,DATE,T06040002
      COMMON P,REC(9),ID1,W,BB,HE,ALAT,C(60),AO(5),IP,JFL,LIK,KBX,JH    06050002
      REAL*4 SEC(9)                                                     06060002
      EQUIVALENCE (TAB(1),PP),(TAB(2),TT),(TAB(3),SP),(TAB(4),UW),(TAB(506070002
     1),ZE),(TAB(6),PE),(TAB(7),PL),(TAB(8),PR),(TAB(9),R),(TAB(10),TL),06080002
     2(TAB(11),ET),(TAB(12),RO),(TAB(13),ALPHA),(TAB(14),BETA),(TAB(15),06090002
     3GAMMA),(TAB(16),DELTA),(TAB(17),AKAP),(TAB(18),CET),(TAB(19),CR), 06100002
     4(TAB(20),CRO),(TAB(21),CL),(TAB(22),CP),(TAB(23),CD),(TAB(24),Z), 06110002
     5(TAB(25),SSS),(TAB(26),SSU),(TAB(27),PV),(TAB(28),PRO),(TAB(29),PX06120000
     61),(TAB(30),PX2),(TAB(31),PX3),(TAB(32),PG),(TAB(33),YP),(TAB(34),06130002
     7PDI),(TAB(35),UD),(TAB(36),DE),(TAB(37),CI),(TAB(38),PY),(TAB(39) 06140002
     8,GM),(ITAB(1),ID),(ITAB(2),IY),(ITAB(3),IW),(SEC(1),TAB(25)),     06150002
     9(TAB(40),CC)
      KK=II+10                                                          06170002
      AA=0.0                                                            06180002
      AL=0.0                                                            06190002
      DO 10 J=II,KK,2                                                   06200002
      AL=AL+1.                                                          06210002
      LL=J+1                                                            06220000
 10   AA=AA+C(J)*COS((6.283185*AL)*(TIME-C(LL))/365.)                   06230002
      AA=AA+AO(JJ)                                                      06240002
      IF  (II-49) 20,30,30                                              06250002
 20   IF  (AA-1.0) 25,25,35                                             06260002
 25   IF  (AA-0.0) 26,30,30                                             06270002
 26   AA=0.0                                                            06280002
 30   RETURN                                                            06290002
 35   AA=1.0                                                            06300002
      GO TO 30                                                          06310002
      END                                                               06320000
      SUBROUTINE RSTATE(NSTATE)                                         06330002
C     THIS SUBROUTINE STORES NAMES OF STATES INTO ARRAY NSTATE FROM THE 06340002
C     CLIMATE DIVISION DIRECTORY                                        06350002
      INTEGER*4 NSTATE(196),NSTAT(4)                                    06360002
      IN=1                                                              06370002
 6    FORMAT(I2,1X,I2,1X,4A4)                                           06380002
 1    READ(2,6,END=888) IST,IDIV,NSTAT                                  06390002
      IF(IDIV.NE.0) GO TO 1                                             06400002
      DO 2 I=1,4                                                        06410002
      NSTATE(IN)=NSTAT(I)                                               06420000
      IN=IN+1                                                           06430002
 2    CONTINUE                                                          06440002
      GO TO 1                                                           06450002
 888  CONTINUE                                                          06460002
      REWIND 2                                                          06470002
      RETURN                                                            06480002
      END                                                               06490002
      SUBROUTINE WTITLE(jyr,jmo2,jda2,ISTN,NSTATE)                    
C     THIS SUBROUTINE PRINTS OUT TITLES FOR EACH STATE AND PUERTO RICO  06510002
      INTEGER*4 NSTATE(196)                                             06520000
      CHARACTER*4 JMON(12)                                              06530002
      DATA JMON/' JAN',' FEB',' MAR',' APR',' MAY',' JUN',' JUL',' AUG',06540002
     1' SEP',' OCT',' NOV',' DEC'/                                      06550002
 1010 FORMAT(39X,'WEEKLY PALMER DROUGHT AND CROP MOISTURE DATA',        06560002
     1/45X,'FOR THE CLIMATE DIVISIONS IN ',4A4,/55X,                    06570002
     2A4,1X,I2,', ',I4,/3X,                                             06580002
     3'P   T    SP   SS    SU    PE   PR    ET ALPHA   CET    CP    CD  06590002
     4  Z     X1    X2    X3 PROB   PDI  PADD    C     CC   WK STA DIV')06600002
 1020 FORMAT(39X,'WEEKLY PALMER DROUGHT AND CROP MOISTURE DATA',        06610002
     1/45X,'FOR THE CLIMATE DIVISION IN ',4A4,/55X,                     06620000
     2A4,1X,I2,', ',I4,/3X,                                             06630002
     3'P   T    SP   SS    SU    PE   PR    ET ALPHA   CET    CP    CD  06640002
     4  Z     X1    X2    X3 PROB   PDI  PADD    C     CC   WK STA DIV')06650002
c     if (jyr.le.94) nyr=jyr+2000
c     if (jyr.gt.94) nyr=jyr+1900
      ISTATE=ISTN/100                                                   06760002
      IF (ISTATE.EQ.66) ISTATE=49                                       06770002
      J1=1+(ISTATE-1)*4                                                 06780002
      J4=J1+3                                                           06790002
      IF (ISTATE.EQ.37) GO TO 10                                        06800002
      WRITE(6,1010) (NSTATE(JNO),JNO=J1,J4),JMON(JMO2),JDA2,jYR         06810002
      RETURN                                                            06820000
   10 WRITE(6,1020) (NSTATE(JNO),JNO=J1,J4),JMON(JMO2),JDA2,jYR         06830002
      RETURN                                                            06840002
      END                                                               06850002
      SUBROUTINE XPHI(PHI,JULN,JULS,NWKS,JYR)                           06860002
C                                                                       06870002
C     THIS SUBROUTINE COMPUTES WEEKLY SOLAR DECLINATION ANGLES FROM THE 06880002
C       STARTING JULIAN DATE (WENESDAY OF WEEK ONE) FOR NWKS NUMBER OF  06890002
C       WEEKS- ALSO JULIAN DATES FOR WENESDAY OF EACH WEEK.             06900002
C                                                                       06910002
C                                                                       06920000
C                                                                       06930002
      REAL*8 CONV,CON,XJUL,D,DR,DR2,SIGMA,SIGR,DD                       06940002
      REAL*4 PHI(1)                                                     06950002
      INTEGER*4 JULN(1)                                                 06960002
      DATA CON/.3978499/,CONV/.0174532925/                              06970002
      NDYR=365                                                          06980002
      LEAP=MOD(JYR,4)                                                   06990002
      IF(LEAP.EQ.0) NDYR=366                                            07000002
      DO 1 I=1,NWKS                                                     07010002
      JULN(I)=JULS+(I-1)*7                                              07020000
      IF(JULN(I).LE.NDYR) GO TO 1                                       07030002
      JULN(I)=JULN(I)-NDYR                                              07040002
    1 CONTINUE                                                          07050002
      DO 2 I=1,NWKS                                                     07060002
      XJUL=JULN(I)-1                                                    07070002
      D=(XJUL*360.)/365.242                                             07080002
      DR=D*CONV                                                         07090002
      DR2=2.*DR                                                         07100002
      SIGMA=279.9348+D+1.914827*DSIN(DR)-0.079525*DCOS(DR)+0.019938*    07110002
     *DSIN(DR2)-0.001620*DCOS(DR2)                                      07120000
      SIGR=SIGMA*CONV                                                   07130002
      DD=DASIN(CON*DSIN(SIGR))                                          07140002
      PHI(I)=DD                                                         07150002
 2    CONTINUE                                                          07160002
C      DO 3 I=1,NWKS                                                    07170002
C   3  WRITE(6,100) I,JULN(I),PHI(I)                                    07180002
  100  FORMAT(1X,'WEEK = ',I3,'  JULN DATE=',I4,'   PHI=',F9.5)         07190002
      RETURN                                                            07200002
      END                                                               07210002
      SUBROUTINE CTITLE(IYR,IWK,myr,mmo,mda,kmo)                        07220000
C     THIS SUBROUTINE PRINTS OUT TITLES FOR MICROMATION                 07230002
      INTEGER*4 NSTATE(196)                                             07240002
      CHARACTER*4 JMON(12)                                              07250002
      DATA JMON/' JAN',' FEB',' MAR',' APR',' MAY','JUNE','JULY',' AUG',07260002
     1'SEPT',' OCT',' NOV',' DEC'/                                      07270002
 1020 FORMAT(1X,/,13X,                                                  07280002
     1'WEEKLY PALMER DROUGHT AND CROP MOISTURE DATA',/12X,              07290002
     2'FOR THE CLIMATE DIVISIONS IN THE EASTERN REGION',/16X,           07300010
     3'CLIMATE PREDICTION CENTER-NCEP-NWS-NOAA',/19X,'WEEK ',           07310010
     4I2,' OF THE ',I4,' GROWING SEASON',/25X,                          07320010
     *'ENDING ',A4,1X,I2,', ',I4,/1X,/1X,                               07330002
     5'ST CD TMP PRCP   SOIL    PCT POT  RUN  CROP  CHNG  MONTH PRELIM',07340002
     6'-P PRCIP',/1X,6X,'(F) (IN) MOISTURE  FLD EVAP OFF  MOIST FR',    07350002
     7'OM  MOIST FINAL -F NEED',/1X,15X,'UPPR LOWR CPC (IN) (IN) ',     07360002
     8'INDEX PREV  ANOML PALMER  TO END',/1X,15X,                       07370002
     9'LAYR LAYR END',17X,'WEEK   (Z)', 2('  DROUTH'),/1X,15X,          07380002
     *2('(IN) '),'WEEK',22X,2('INDEX '),3X,'(IN)'/)                     07390002
 1030 FORMAT(1X,/,13X,                                                  07400002
     1'WEEKLY PALMER DROUGHT AND CROP MOISTURE DATA',/12X,              07410002
     2'FOR THE CLIMATE DIVISIONS IN THE SOUTHERN REGION',/16X,          07420010
     3'CLIMATE PREDICTION CENTER-NCEP-NWS-NOAA',/19X,'WEEK ',           07430010
     4I2,' OF THE ',I4,' GROWING SEASON',/25X,                          07440010
     *'ENDING ',A4,1X,I2,', ',I4,/1X,/1X,                               07450002
     5'ST CD TMP PRCP   SOIL    PCT POT  RUN  CROP  CHNG  MONTH PRELIM',07460002
     6'-P PRCIP',/1X,6X,'(F) (IN) MOISTURE  FLD EVAP OFF  MOIST FR',    07470002
     7'OM  MOIST FINAL -F NEED',/1X,15X,'UPPR LOWR CPC (IN) (IN) ',     07480002
     8'INDEX PREV  ANOML PALMER  TO END',/1X,15X,                       07490002
     9'LAYR LAYR END',17X,'WEEK   (Z)', 2('  DROUTH'),/1X,15X,          07500002
     *2('(IN) '),'WEEK',22X,2('INDEX '),3X,'(IN)'/)                     07510002
 1040 FORMAT(1X,/,13X,                                                  07520000
     1'WEEKLY PALMER DROUGHT AND CROP MOISTURE DATA',/12X,              07530002
     2'FOR THE CLIMATE DIVISIONS IN THE CENTRAL REGION',/16X,           07540010
     3'CLIMATE PREDICTION CENTER-NCEP-NWS-NOAA',/19X,'WEEK ',           07550010
     4I2,' OF THE ',I4,' GROWING SEASON',/25X,                          07560010
     *'ENDING ',A4,1X,I2,', ',I4,/1X,/1X,                               07570002
     5'ST CD TMP PRCP   SOIL    PCT POT  RUN  CROP  CHNG  MONTH PRELIM',07580002
     6'-P PRCIP',/1X,6X,'(F) (IN) MOISTURE  FLD EVAP OFF  MOIST FR',    07590002
     7'OM  MOIST FINAL -F NEED',/1X,15X,'UPPR LOWR CPC (IN) (IN) ',     07600002
     8'INDEX PREV  ANOML PALMER  TO END',/1X,15X,                       07610002
     9'LAYR LAYR END',17X,'WEEK   (Z)', 2('  DROUTH'),/1X,15X,          07620000
     *2('(IN) '),'WEEK',22X,2('INDEX '),3X,'(IN)'/)                     07630002
 1050 FORMAT(1X,/,13X,                                                  07640002
     1'WEEKLY PALMER DROUGHT AND CROP MOISTURE DATA',/12X,              07650002
     2'FOR THE CLIMATE DIVISIONS IN THE WESTERN REGION',/16X,           07660010
     3'CLIMATE PREDICTION CENTER-NCEP-NWS-NOAA',/19X,'WEEK ',           07670010
     4I2,' OF THE ',I4,' GROWING SEASON',/25X,                          07680010
     *'ENDING ',A4,1X,I2,', ',I4,/1X,/1X,                               07690002
     5'ST CD TMP PRCP   SOIL    PCT POT  RUN  CROP  CHNG  MONTH PRELIM',07700002
     6'-P PRCIP',/1X,6X,'(F) (IN) MOISTURE  FLD EVAP OFF  MOIST FR',    07710002
     7'OM  MOIST FINAL -F NEED',/1X,15X,'UPPR LOWR CPC (IN) (IN) ',     07720000
     8'INDEX PREV  ANOML PALMER  TO END',/1X,15X,                       07730002
     9'LAYR LAYR END',17X,'WEEK   (Z)', 2('  DROUTH'),/1X,15X,          07740002
     *2('(IN) '),'WEEK',22X,2('INDEX '),3X,'(IN)'/)                     07750002
c     if (iyr.gt.94) jyr=iyr+1900
c     if (iyr.le.94) jyr=iyr+2000
c     if (myr.gt.94) nyr=myr+1900
c     if (myr.le.94) nyr=myr+2000
      jyr=iyr
      nyr=myr
      mcen=iw3jdn(nyr,mmo,mda)
      ncen=mcen+7
      call w3fs26(ncen,kyr,kmo,kda,kwk,kjd)
      write(12,1020) iwk,jyr,jmon(mmo),mda,nyr
      write(13,1030) iwk,jyr,jmon(mmo),mda,nyr
      write(14,1040) iwk,jyr,jmon(mmo),mda,nyr
      write(15,1050) iwk,jyr,jmon(mmo),mda,nyr
      RETURN                                                            08050002
      END                                                               08060002
      SUBROUTINE COMUN(ISTAT,IDIV,IT,IPR,X1,X2,X3,JWK,JMO1,CDM,AKM,JW4, 08070002
     1KYR,JMO3,PROB,PADD)                                               08080002
C     THIS SUBROUTINE WRITES DATA TO COMMUNICATIONS FILES,              08090002
C     AFOS AND ANALYSES FILE,AND MONTHLY PROJECTION FILE.               08100002
C                                                                       08110002
      INTEGER*4 IFLG(49)                                                08120000
      CHARACTER*2 JSTAT(49)                                             08130002
      COMMON                                            K,DUM,DK,Q,IDUM 08140002
      COMMON ITAB(3),TAB(40),VEC(9),B,SL,UL,RS,RU,IC,ISTN,IYR,IWK,DATE,T08150002
      COMMON P,REC(9),ID1,W,BB,HE,ALAT,C(60),AO(5),IP,JFL,LIK,KBX,JH    08160002
      REAL*4 SEC(9)                                                     08170002
      CHARACTER*1 LPFLG,LFFLG,LFLAG                                     08180002
      EQUIVALENCE (TAB(1),PP),(TAB(2),TT),(TAB(3),SP),(TAB(4),UW),(TAB(508190002
     1),ZE),(TAB(6),PE),(TAB(7),PL),(TAB(8),PR),(TAB(9),R),(TAB(10),TL),08200002
     2(TAB(11),ET),(TAB(12),RO),(TAB(13),ALPHA),(TAB(14),BETA),(TAB(15),08210002
     3GAMMA),(TAB(16),DELTA),(TAB(17),AKAP),(TAB(18),CET),(TAB(19),CR), 08220000
     4(TAB(20),CRO),(TAB(21),CL),(TAB(22),CP),(TAB(23),CD),(TAB(24),Z), 08230002
     5(TAB(25),SSS),(TAB(26),SSU),(TAB(27),PV),(TAB(28),PRO),(TAB(29),PX08240002
     61),(TAB(30),PX2),(TAB(31),PX3),(TAB(32),PG),(TAB(33),YP),(TAB(34),08250002
     7PDI),(TAB(35),UD),(TAB(36),DE),(TAB(37),CI),(TAB(38),PY),(TAB(39) 08260002
     8,GM),(ITAB(1),ID),(ITAB(2),IY),(ITAB(3),IW),(SEC(1),TAB(25)),     08270002
     9(TAB(40),CC)
       DATA JSTAT/'AL','AZ','AR','CA','CO','CT','DE','FL','GA','ID',    08290002
     1'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI',                08300002
     2'MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY',                08310002
     3'NC','ND','OH','OK','OR','PA','RI','SC','SD','TN',                08320000
     4'TX','UT','VT','VA','WA','WV','WI','WY','PR'/                     08330002
C    IFLG - ARRAY TO DESIGNATE WHICH REGION EACH STATE BELONGS          08340002
C           IFLG - 1 - EASTERN REGION                                   08350002
C                  2 - SOUTHERN REGION                                  08360002
C                  3 - CENTRAL REGION                                   08370002
C                  4 - WESTERN REGION                                   08380002
      DATA IFLG/2,4,2,4,3,1,1,2,2,4,                                    08390002
     13,3,3,3,3,2,1,1,1,3,                                              08400002
     23,2,3,4,3,4,1,1,2,1,                                              08410002
     31,3,1,2,4,1,1,1,3,2,2,4,1,1,4,1,3,3,2/                            08420000
      DATA LPFLG/'P'/,LFFLG/'F'/                                        08430002
C     DETERMINE THE PRELIMINARY PALMER DROUGHT INDEX                    08440002
      CALL CASE(PRO,PX1,PX2,PX3,PDI)                                    08450002
C     PRLEMINARY PDI VALUE SELECTED-MAY OR MAY NOT BE FINAL PDI         08460002
C                                                                       08470002
C     COMPUTE PERCENT OF FIELD CAPACITY AT END OF WEEK                  08480002
      PFCAP=((SSS+SSU)/(W+1.))*100.                                     08490002
      IPFCAP=PFCAP+.501                                                 08500000
C                                                                       08510002
C     COMPUTE ADDITIONAL PRECIPITATION NEEDED TO END THE DROUGHT        08520002
      PED=0.                                                            08530002
      PADD=0.                                                           08540002
      IF (PDI.GE.-.5) GO TO 2                                           08550002
      CALL CASE(PROB,X1,X2,X3,X)                                        08560002
        IF(X.GT.-.5) X=PDI                                              08570002
      ZEP=-2.925*X-1.5                                                  08580002
      PED=ZEP/AKAP+CP                                                   08590002
      PED=PED+.005                                                      08600000
      PADD=PED-PP                                                       08610002
C     PRINT 100,PED,ZEP,AKAP,CP,X1,X2,X3                                08620002
C 100 FORMAT(1X,'PED=',F6.1,' ZEP=',F6.2,' AKAP=',F6.2,' CP=',F6.2,     08630002
C    1' X1,X2,X3=',3F6.2)                                               08640002
    2 CONTINUE                                                          08650002
C     MATCH WEEK NUMBERS - CURRENT WEEK ONLY IS WRITTEN ONTO FILES      08660007
      IF(IW.NE.JWK) GO TO 3                                             08670007
C    SET LFLAG EQUAL TO "P" FOR PRELIMINARY PDI AND TO "F" FOR FINAL PDI08680002
       LFLAG=LPFLG                                                      08690002
       IF(PRO.NE.0..AND.PRO.NE.100.) GO TO 5                            08700002
       IF(PX3.NE.0.) GO TO 4                                            08710002
       IF(PX1.EQ.0..OR.PX2.EQ.0.) LFLAG=LFFLG                           08720000
       GO TO 5                                                          08730002
    4  LFLAG=LFFLG                                                      08740002
    5  CONTINUE                                                         08750002
      ISTE=ISTAT                                                        08760002
      IF(ISTAT.EQ.66) ISTE=49                                           08770002
      IFG=IFLG(ISTE)                                                    08780002
C       READ LATITUDE AND LONGITUDE OF CD FROM CD DIRECTORY             08790002
    1 READ(2,950,END=999) IST,IDV,ILAT,ILON                             08800002
      IF(IDV.EQ.0) GO TO 1                                              08810002
      JD=IDV+IST*100                                                    08820000
      IF(ISTN.NE.JD) GO TO 1                                            08830002
      IDV=MOD(IDV,10)                                                   08840002
C     COMPUTE 4 WEEK Z INDEX - ZM                                       08850002
      CDM=CDM+CD                                                        08860002
      ZM=AKM*CDM                                                        08870002
      IF(JW4.GT.0) GO TO 6                                              08880002
      ZM=99.99                                                          08890002
  6   CONTINUE                                                          08900002
C     WRITE DATA TO PROJECTION FILE IF LAST WEN OF MONTH                08910002
      IF(JMO1.NE.JMO3)                                                  08920000
     1WRITE(10,960) ID,KYR,JMO1,IW,SSS,SSU,PV,PRO,PX1,PX2,PX3           08930002
C      WRITE DATA ONTO AFOS AND ANALYSES FILE                           08940002
      WRITE(11,980) ILAT,ILON,CI,CC,PDI,PADD,LFLAG,JSTAT(ISTE),IDV      08950002
C      WRITE DATA ONTO COMMUNICATIONS FILES                             08960002
C      CHECK ADD PRECIP TO END DROUGHT - IF 0 THEN WRITE AS BLANK       08970002
      IF(PADD.EQ.0.0) GO TO 7                                           08980002
      IF(IFG.EQ.1) WRITE(12,970) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 08990002
     1PE,RO,CI,CC,ZM,PDI,LFLAG,PADD                                     09000002
      IF(IFG.EQ.2) WRITE(13,970) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09010002
     1PE,RO,CI,CC,ZM,PDI,LFLAG,PADD                                     09020000
      IF(IFG.EQ.3) WRITE(14,970) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09030002
     1PE,RO,CI,CC,ZM,PDI,LFLAG,PADD                                     09040002
      IF(IFG.EQ.4) WRITE(15,970) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09050002
     1PE,RO,CI,CC,ZM,PDI,LFLAG,PADD                                     09060002
      GO TO 3                                                           09070002
   7  CONTINUE                                                          09080002
C    WRITE DATA TO COMMUNICATION FILES AS ABOVE BUT WITH PADD BLANK     09090002
      IF(IFG.EQ.1) WRITE(12,971) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09100002
     1PE,RO,CI,CC,ZM,PDI,LFLAG                                          09110002
      IF(IFG.EQ.2) WRITE(13,971) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09120000
     1PE,RO,CI,CC,ZM,PDI,LFLAG                                          09130002
      IF(IFG.EQ.3) WRITE(14,971) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09140002
     1PE,RO,CI,CC,ZM,PDI,LFLAG                                          09150002
      IF(IFG.EQ.4) WRITE(15,971) JSTAT(ISTE),IDIV,IT,PP,SSS,SSU,IPFCAP, 09160002
     1PE,RO,CI,CC,ZM,PDI,LFLAG                                          09170002
    3 CONTINUE                                                          09180002
      CDM=0.                                                            09190002
      RETURN                                                            09200002
  999 CONTINUE                                                          09210002
      WRITE(6,991)                                                      09220000
  991 FORMAT(1X,'READ TO END OF CLIMATE DIVISION DIRECTORY 2ND R')      09230002
  970 FORMAT(1X,A2,I3,I4,F5.1,2F5.2,I4,2F5.2,F6.2,F6.2,F6.2,F7.2,1X,    09240002
     *A1,F6.2)                                                          09250002
  971 FORMAT(1X,A2,I3,I4,F5.1,2F5.2,I4,2F5.2,F6.2,F6.2,F6.2,F7.2,1X,A1) 09260002
  950 FORMAT(I2,1X,I2,52X,I5,I6)                                        09270002
  980 FORMAT(1X,2I6,4F6.2,A1,A2,I1)                                     09280002
  960 FORMAT(2I4,2I2,3F7.2,F5.1,3F7.2)                                  09290002
      RETURN                                                            09300002
      END                                                               09310002
      SUBROUTINE CASE (PROB,X1,X2,X3,PALM)                              09320000
C                                                                       09330002
C     THIS SUBROUTINE SELECTS THE PRELIMINARY (OR NEAR-REAL TIME)       09340002
C     PALMER DROUGHT SEVERITY INDEX (PDSI) FROM THE GIVEN X VALUES      09350002
C     DEFINED BELOW AND THE PROBABILTIY (PROB) OF ENDING EITHER A       09360002
C     DROUGHT OR WET SPELL.                                             09370002
C                                                                       09380002
C     X1 - INDEX FOR INCIPIENT WET SPELLS (ALWYS POSITIVE)              09390002
C     X2 - INDEX FOR INCIPIENT DRY SPELLS (ALWAYS NEGATIVE)             09400002
C     X3 - SEVERITY INDEX FOR AN ESTABLISHED WET SPELL (POSITIVE)       09410002
C          OR DROUGHT (NEGATIVE)                                        09420000
C     PALM - THE SELECTED PDSI (EITHER PRELIMINARY OF FINAL)            09430002
      IF (X3.NE.0.) GO TO 10                                            09440002
C     IF X3=0 THE INDEX IS NEAR NORMAL AND EITHER A DRY OR WET SPELL    09450002
C     EXISTS. CHOOSE THE LARGEST ABSOLUTE VALUE OF X1 OR X2.            09460002
      PALM=X1                                                           09470002
      IF (ABS(X2).GT.ABS(X1)) PALM=X2                                   09480002
      GO TO 100                                                         09490002
C                                                                       09500002
   10 CONTINUE                                                          09510002
      PRO=PROB/100.                                                     09520000
      IF(PROB.GT.0..AND.PROB.LT.100.) GO TO 20                          09530002
C     A WEATHER SPELL IS ESTABLISHED AND PALM EQUALS X3 AND IS FINAL    09540002
      PALM=X3                                                           09550002
      GO TO 100                                                         09560002
C                                                                       09570002
   20 CONTINUE                                                          09580002
      IF(X3.GT.0.) GO TO 30                                             09590002
C     TAKE WEIGHTED SUM OF X3 AND X1                                    09600002
      PALM=(1.-PRO)*X3+PRO*X1                                           09610002
      IF(X1.EQ.0.) PALM=X3                                              09620008
      GO TO 100                                                         09630000
  30  CONTINUE                                                          09640002
C     TAKE WEIGHTED SUM OF X3 AND X2                                    09650002
      PALM=(1.-PRO)*X3+PRO*X2                                           09660002
      IF(X2.EQ.0.) PALM=X3                                              09670008
C                                                                       09680002
  100 RETURN                                                            09690002
      END                                                               09700002
       SUBROUTINE W3FS26(JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 87-03-29
C
C ABSTRACT: COMPUTES YEAR (4 DIGITS), MONTH, DAY, DAY OF WEEK, DAY
C   OF YEAR FROM JULIAN DAY NUMBER. THIS SUBROUTINE WILL WORK
C   FROM 1583 A.D. TO 3300 A.D.
C
C PROGRAM HISTORY LOG:
C   87-03-29  R.E.JONES
C   89-10-25  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FS26(JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     JLDAYN ARG LIST  INTEGER   JULIAN DAY NUMBER
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IYEAR  ARG LIST  INTEGER   YEAR  (4 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH
C     IDAY   ARG LIST  INTEGER   DAY
C     IDAYWK ARG LIST  INTEGER   DAY OF WEEK (1 IS SUNDAY, 7 IS SAT)
C     IDAYYR ARG LIST  INTEGER   DAY OF YEAR (1 TO 366)
C
C   REMARKS: A JULIAN DAY NUMBER CAN BE COMPUTED BY USING ONE OF THE
C     FOLLOWING STATEMENT FUNCTIONS. A DAY OF WEEK CAN BE COMPUTED
C     FROM THE JULIAN DAY NUMBER. A DAY OF YEAR CAN BE COMPUTED FROM
C     A JULIAN DAY NUMBER AND YEAR.
C
C      IYEAR (4 DIGITS)
C
C      JDN(IYEAR,MONTH,IDAY) = IDAY - 32075
C    &            + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
C    &            + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
C    &            - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
C
C      IYR (4 DIGITS) , IDYR(1-366) DAY OF YEAR
C
C      JULIAN(IYR,IDYR) = -31739 + 1461 * (IYR + 4799) / 4
C    &                    -3 * ((IYR + 4899) / 100) / 4 + IDYR
C
C      DAY OF WEEK FROM JULIAN DAY NUMBER, 1 IS SUNDAY, 7 IS SATURDAY.
C
C      JDAYWK(JLDAYN) = MOD((JLDAYN + 1),7) + 1
C
C      DAY OF YEAR FROM JULIAN DAY NUMBER AND 4 DIGIT YEAR.
C
C      JDAYYR(JLDAYN,IYEAR) = JLDAYN -
C     &  (-31739+1461*(IYEAR+4799)/4-3*((IYEAR+4899)/100)/4)
C
C      THE FIRST FUNCTION WAS IN A LETTER TO THE EDITOR COMMUNICATIONS
C      OF THE ACM  VOLUME 11 / NUMBER 10 / OCTOBER, 1968. THE 2ND
C      FUNCTION WAS DERIVED FROM THE FIRST. THIS SUBROUTINE WAS ALSO
C      INCLUDED IN THE SAME LETTER. JULIAN DAY NUMBER 1 IS
C      JAN 1,4713 B.C. A JULIAN DAY NUMBER CAN BE USED TO REPLACE A
C      DAY OF CENTURY, THIS WILL TAKE CARE OF THE DATE PROBLEM IN
C      THE YEAR 2000, OR REDUCE PROGRAM CHANGES TO ONE LINE CHANGE
C      OF 1900 TO 2000. JULIAN DAY NUMBERS CAN BE USED FOR FINDING
C      RECORD NUMBERS IN AN ARCHIVE OR DAY OF WEEK, OR DAY OF YEAR.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864
C
C$$$
C
       L      = JLDAYN + 68569
       N      = 4 * L / 146097
       L      = L - (146097 * N + 3) / 4
       I      = 4000 * (L + 1) / 1461001
       L      = L - 1461 * I / 4 + 31
       J      = 80 * L / 2447
       IDAY   = L - 2447 * J / 80
       L      = J / 11
       MONTH  = J + 2 - 12 * L
       IYEAR  = 100 * (N - 49) + I + L
       IDAYWK = MOD((JLDAYN + 1),7) + 1
       IDAYYR = JLDAYN -
     &  (-31739 +1461 * (IYEAR+4799) / 4 - 3 * ((IYEAR+4899)/100)/4)
       RETURN
       END
       FUNCTION IW3JDN(IYEAR,MONTH,IDAY)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: IW3JDN         COMPUTE JULIAN DAY NUMBER
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 87-03-29
C
C ABSTRACT: COMPUTES JULIAN DAY NUMBER FROM YEAR (4 DIGITS), MONTH,
C   AND DAY. IW3JDN IS VALID FOR YEARS 1583 A.D. TO 3300 A.D.
C   JULIAN DAY NUMBER CAN BE USED TO COMPUTE DAY OF WEEK, DAY OF
C   YEAR, RECORD NUMBERS IN AN ARCHIVE, REPLACE DAY OF CENTURY,
C   FIND THE NUMBER OF DAYS BETWEEN TWO DATES.
C
C PROGRAM HISTORY LOG:
C   87-03-29  R.E.JONES
C   89-10-25  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:   II = IW3JDN(IYEAR,MONTH,IDAY)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IYEAR  ARG LIST  INTEGER   YEAR           ( 4 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH OF YEAR   (1 - 12)
C     IDAY   ARG LIST  INTEGER   DAY OF MONTH    (1 - 31)
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IW3JDN FUNTION   INTEGER   JULIAN DAY NUMBER
C                      JAN. 1,1960 IS JULIAN DAY NUMBER 2436935
C                      JAN. 1,1987 IS JULIAN DAY NUMBER 2446797
C
C   REMARKS: JULIAN PERIOD WAS DEVISED BY JOSEPH SCALIGER IN 1582.
C     JULIAN DAY NUMBER #1 STARTED ON JAN. 1,4713 B.C. THREE MAJOR
C     CHRONOLOGICAL CYCLES BEGIN ON THE SAME DAY. A 28-YEAR SOLAR
C     CYCLE, A 19-YEAR LUNER CYCLE, A 15-YEAR INDICTION CYCLE, USED
C     IN ANCIENT ROME TO REGULATE TAXES. IT WILL TAKE 7980 YEARS
C     TO COMPLETE THE PERIOD, THE PRODUCT OF 28, 19, AND 15.
C     SCALIGER NAMED THE PERIOD, DATE, AND NUMBER AFTER HIS FATHER
C     JULIUS (NOT AFTER THE JULIAN CALENDAR). THIS SEEMS TO HAVE
C     CAUSED A LOT OF CONFUSION IN TEXT BOOKS. SCALIGER NAME IS
C     SPELLED THREE DIFFERENT WAYS. JULIAN DATE AND JULIAN DAY
C     NUMBER ARE INTERCHANGED. A JULIAN DATE IS USED BY ASTRONOMERS
C     TO COMPUTE ACCURATE TIME, IT HAS A FRACTION. WHEN TRUNCATED TO
C     AN INTEGER IT IS CALLED AN JULIAN DAY NUMBER. THIS FUNCTION
C     WAS IN A LETTER TO THE EDITOR OF THE COMMUNICATIONS OF THE ACM
C     VOLUME 11 / NUMBER 10 / OCTOBER 1968. THE JULIAN DAY NUMBER
C     CAN BE CONVERTED TO A YEAR, MONTH, DAY, DAY OF WEEK, DAY OF
C     YEAR BY CALLING SUBROUTINE W3FS26.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C$$$
C
       IW3JDN  =    IDAY - 32075
     &            + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
     &            + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
     &            - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
       RETURN
       END
