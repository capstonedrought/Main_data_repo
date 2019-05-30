C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  00020002
C                                                                       00030002
C     MAINC     FT04F001 - PAST DATA                                    00050002
C                                                                       00130002
C     ABSTRACT: THIS ROUTINE READS THE PALMER DROUGHT AND CROP          00140002
C       MOISTURE DATA for the years 1988 to 1996
C
C     FT04F001 - PAST DATA
C                                                                       00260002
C                                                                       00600002
C$$$                                                                    00610002
C                                                                       00620000
      DIMENSION ITAB(4),TAB(40)
      EQUIVALENCE (TAB(1),PP),(TAB(2),TT),(TAB(3),SP),(TAB(4),UW),(TAB(500680002
     1),ZE),(TAB(6),PE),(TAB(7),PL),(TAB(8),PR),(TAB(9),R),(TAB(10),TL),00690002
     2(TAB(11),ET),(TAB(12),RO),(TAB(13),ALPHA),(TAB(14),BETA),(TAB(15),00700002
     3GAMMA),(TAB(16),DELTA),(TAB(17),AKAP),(TAB(18),CET),(TAB(19),CR), 00710002
     4(TAB(20),CRO),(TAB(21),CL),(TAB(22),CP),(TAB(23),CD),(TAB(24),Z), 00720000
     5(TAB(25),SSS),(TAB(26),SSU),(TAB(27),PV),(TAB(28),PRO),(TAB(29),PX00730002
     61),(TAB(30),PX2),(TAB(31),PX3),(TAB(32),PG),(TAB(33),YP),(TAB(34),00740002
     7PDI),(TAB(35),UD),(TAB(36),DE),(TAB(37),CI),(TAB(38),PY),(TAB(39) 00750002
     8,GM),(ITAB(1),ID),(ITAB(2),IY),(ITAB(3),IW),
     9(TAB(40),CC),(ITAB(4),KFL)                                        00770002
      open(unit=4,file='PALMER88',
     + blank='zero',status='unknown',access='sequential')
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
 7    READ   (4,5,END=42) ITAB,TAB              
 5    FORMAT(I4,3I2,F4.1,F4.0,10F6.2,4F6.4,F6.3,10F6.2,F4.0,12F6.2)     02430002
      GO TO 7
 42   continue                                                          02640002
      close(unit=4)
      STOP                                                              03680002
      END                                                               04560002
