C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  00020002
C                                                                       00030002
C     MAIN PROGRAM: 
C                                                                       00050002
C                                                                       00130002
C     ABSTRACT: THIS ROUTINE READS PARAMETERS ON MASTER FILE            00140002
C                                                                       00180002
C   USAGE:                                                              00190002
C     INPUT FILES:                                                      00200002
C     FT01F001 -  prod.wd53.wpdmastr - MASTER TAPE           
C                                                                       00600002
C$$$                                                                    00610002
C                                                                       00620000
      DIMENSION C(60),AO(5)
      open(unit=1,file='wpdmastr',
     + blank='zero',status='unknown',access='sequential')
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
 2    READ (1,1,END=888)    ID1,W,BB,HE,ALAT,C,AO             
 1    FORMAT(I4,2F5.2,F4.0,F7.4,65F11.6)             
      GO TO 2 
 888  continue                                                 
      close(unit=1)
      STOP                                                              03680002
      END                                                               04560002
