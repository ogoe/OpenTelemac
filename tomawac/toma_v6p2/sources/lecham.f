!                    *****************
                     SUBROUTINE LECHAM
!                    *****************
!
     &( ZM , DZHDT, X    , Y     , NPOIN2, NDON , BINDON, NBOR  , NPTFR,
     &  AT , DDC  , TM1  , TM2   , NP   , XRELV , YRELV , ZR   ,
     &  Z1 , Z2   , INDIM, NPMAX , IDHMA, NVAR  )
!
!***********************************************************************
! TOMAWAC   V6P3                                   21/06/2011
!***********************************************************************
!
!brief    THIS SUBROUTINE PROJECTS THE TIDE DATA ON THE
!+                COMPUTATION MESH AND INTERPOLATES TO FIRST TIME STEP.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D AMONGST OTHERS)
!
!history
!+
!+        V5P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Bug correction in the reading of the TELEMAC format file
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        16/11/2012
!+        V6P3
!+   Only SELAFIN format with same mesh kept.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINDON         |-->| DATA FILE BINARY
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| IDHMA          |-->| RANK OF THE WATER LEVEL DATA IN THE TELEMAC FILE
!| INDIM          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NP             |<->| NUMBER OF POINTS READ FROM THE FILE
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS THAT CAN BE READ
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |<--| NUMBER OF VARIABLES READ FROM THE DATA FILE
!| TM1            |<--| TIME T1 IN THE DATA FILE
!| TM2            |<--| TIME T2 IN THE DATA FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XRELV          |<->| TABLE OF THE ABSCISSES OF DATA FILE POINTS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YRELV          |<->| TABLE OF THE ORDINATES OF DATA FILE POINTS
!| Z1             |<->| DATA AT TIME TM1 AT THE MESH POINTS
!| Z2             |<->| DATA AT TIME TM2 AT THE MESH POINTS
!| ZM             |<--| DATA AT TIME AT, AT THE MESH POINTS
!| ZR             |<->| TABLE OF THE VALUES READ IN THE DATA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_LECHAM => LECHAM
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIM,NCOL,NLIG,BID,I,J
      INTEGER NVAR,NI,ISTAT,IB(10),IDHMA
!
      INTEGER NPMAX,NBOR(NPTFR,2)
!
      DOUBLE PRECISION X(NPOIN2)    , Y(NPOIN2)
      DOUBLE PRECISION ZM(NPOIN2)   , DZHDT(NPOIN2)
      DOUBLE PRECISION XRELV(NPMAX), YRELV(NPMAX)
      DOUBLE PRECISION ZR(NPMAX)   , Z1(NPMAX)   , Z2(NPMAX)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TM1,TM2
      DOUBLE PRECISION DDC,DAT1,DAT2,Z(1),ATT, ATB(1)
      DOUBLE PRECISION COE1, COE2
      REAL, ALLOCATABLE :: W(:)
!
      CHARACTER*3  BINDON,C
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
!
      ALLOCATE(W(NPOIN2))
!
!-----------------------------------------------------------------------
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF(INDIM.EQ.3) THEN
!
!      -----------------------------------------------------------------
!      TELEMAC FORMAT
!      -----------------------------------------------------------------
!
       REWIND NDON
!
!      READS TITLE
!
       CALL LIT(X,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
!
!      READS NUMBER OF VARIABLES AND THEIR NAMES
!
       CALL LIT(X,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
       NVAR=IB(1)
       DO 80 I=1,NVAR
         CALL LIT(X,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
80     CONTINUE
!
!      FORMAT AND GEOMETRY
!
       CALL LIT(X,W,IB,C,10,'I ',NDON,BINDON,ISTAT)
       IF(IB(10).EQ.1) THEN
         CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       ENDIF
       CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       NP=IB(2)
       NI=IB(1)*IB(3)
       WRITE(LU,*) '--------------------------------------------'
       IF(LNG.EQ.1) THEN
         WRITE(LU,*)'LECHAM : LECTURE DU FICHIER TELEMAC'
         WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
         WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
       ELSE
         WRITE(LU,*)'LECHAM : READING OF TELEMAC DATA FILE '
         WRITE(LU,*) '         FILE TITLE : ',TITCAS
         WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
       ENDIF
       WRITE(LU,*) '--------------------------------------------'
       IF(NP.NE.NPOIN2) THEN
         WRITE(LU,*) ' '
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'LE MAILLAGE DU'
           WRITE(LU,*) 'FICHIER DU NIVEAU DE LA MAREE EST'
           WRITE(LU,*) 'DIFFERENT DE CELUI DU FICHIER DE GEOMETRIE'
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*) 'THE MESH OF THE TIDAL WATER LEVEL FILE'
           WRITE(LU,*) 'IS DIFFERENT FROM THE GEOMETRY FILE'
         ENDIF
         WRITE(LU,*) ' '
        CALL PLANTE(1)
       ENDIF
!      ARRAY OF INTEGERS IKLE
       READ(NDON)
!      ARRAY OF INTEGERS IPOBO
       READ(NDON)
!
!      X AND Y
!
!      CALL LIT(XRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
!      CALL LIT(YRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
       READ(NDON)
       READ(NDON)
!
!      TIME STEP AND VARIABLES
!
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       TM1=ATB(1)
       IF(TM1.GT.AT) THEN
         WRITE(LU,*) '*************************************************'
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER '
           WRITE(LU,*) '   CONTENANT LE NIVEAU DE LA MAREE    '
           WRITE(LU,*) '   ',AT,' EST POSTERIEUR AU TEMPS '
           WRITE(LU,*) '   DU DEBUT DU CALCUL',TM1
         ELSE
           WRITE(LU,*) ' THE FIRST RECORDING OF THE TIDE LEVEL FILE '
           WRITE(LU,*) '   ',ATT,' DATES LATER THAN THE DEGINNING '
           WRITE(LU,*) '   OF THE COMPUTATION',DDC
         ENDIF
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
       ENDIF
!
110    CONTINUE
       DO I =1,NVAR
         IF(I.EQ.IDHMA) THEN
           CALL LIT(Z1,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
         ELSE
           READ(NDON)
         ENDIF
       ENDDO
!
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       TM2=ATB(1)
       IF(TM2.LE.AT) THEN
         TM1=TM2
         GOTO 110
       ENDIF
!
       DO I =1,NVAR
        IF(I.EQ.IDHMA) THEN
          CALL LIT(Z2,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
          READ(NDON)
        ENDIF
       ENDDO
!
       WRITE(LU,*) 'TMAREE1:',TM1
       WRITE(LU,*) 'TMAREE2:',TM2
!
      ELSEIF (INDIM.EQ.4) THEN
!       READS A USER-DEFINED FORMAT
        CALL MARUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TM1,TM2,
     &              NP,XRELV,YRELV,ZR,Z1,Z2,NPMAX)
!
      ELSE
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECHAM : INDICATEUR DE FORMAT INCONNU : ',INDIM
        WRITE(LU,*) '         POUR LE FICHIER DU NIVEAU DE LA MAREE '
        ELSE
          WRITE(LU,*)'LECHAM : UNKNOWN INDICATOR OF FORMAT : ',INDIM
          WRITE(LU,*)'         FOR THE TIDE LEVEL DATA FILE '
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!   INTERPOLATES IN TIME
!   AND COMPUTES THE TEMPORAL GRADIENT OF THE TIDE
!-----------------------------------------------------------------------
!
      COE1=(TM2-TM1)
      IF(COE1.LT.1.D-4) THEN
        WRITE(LU,*) '****************************************'
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) ' DEUX TEMPS IDENTIQUES                '
          WRITE(LU,*) ' DANS LE FICHIER DES HAUTEURS DE MAREE'
        ELSE
          WRITE(LU,*) ' TWO IDENTICAL TIMES IN THE TIDAL FILE'
        ENDIF
        WRITE(LU,*) '****************************************'
        CALL PLANTE(1)
      ENDIF
      COE2=(AT-TM1)/COE1
      DO I=1,NPOIN2
         ATT     = (Z2(I)-Z1(I))
         ZM(I)   = ATT*COE2+Z1(I)
         DZHDT(I)= ATT/COE1
      ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(W)
!
!-----------------------------------------------------------------------
!
!     FORMATS
!
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
!
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
100   CONTINUE
      WRITE(LU,*)'**********************************************'
      IF (LNG.EQ.1) THEN
         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE LA MAREE '
         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
      ELSE
         WRITE(LU,*)'  ERROR WHILE READING TIDAL DATA '
         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'**********************************************'
      CALL PLANTE(1)
!
      RETURN
      END

