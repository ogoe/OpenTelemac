!                    *****************
                     SUBROUTINE NOUMAR
!                    *****************
!
     &(ZM , DZHDT, X  , Y  , NPOIN2, NDON , BINDON, NBOR , NPTFR,
     & AT , DDC  , TM1, TM2, Z1 , Z2, INDIM, IDHMA , NVHMA )
!
!***********************************************************************
! TOMAWAC   V6P3                                   21/06/2011
!***********************************************************************
!
!brief    COMPUTES THE TIDE FOR THE CURRENT TIME STEP
!+                AND ON THE COMPUTATION MESH.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        16/11/2012
!+        V6P3
!+   Only SELAFIN format with same mesh kept. Arguments removed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINDON         |-->| DATA FILE BINARY
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DZHDT          |<--| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| IDHMA          |-->| RANK OF THE WATER LEVEL DATA IN THE TELEMAC FILE
!| INDIM          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVHMA          |-->| N.OF VARIABLES OF THE FORMATTED WATER LEVEL FILE
!| TM1            |<->| TIME T1 IN THE DATA FILE
!| TM2            |<->| TIME T2 IN THE DATA FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z1             |<->| TIDAL HEIGTH AT TIME TM1, AT THE MESH POINTS
!| Z2             |<->| TIDAL HEIGTH AT TIME TM2, AT THE MESH POINTS
!| ZM             |<--| TIDAL HEIGTH AT TIME AT, AT THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_NOUMAR => NOUMAR
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,NPTFR
      INTEGER, INTENT(IN)             :: INDIM,IDHMA,NVHMA
      INTEGER, INTENT(IN)             :: NBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2) ,Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZM(NPOIN2),DZHDT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: Z1(NPOIN2),Z2(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC
      DOUBLE PRECISION, INTENT(INOUT) :: TM1,TM2
      CHARACTER(LEN=3), INTENT(IN)    :: BINDON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISTAT,IW(1)
      DOUBLE PRECISION DAT2B(1),Z(1),C,COE1,COE2,ATT
      CHARACTER(LEN=3) C1
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2))
!
!-----------------------------------------------------------------------
!
      IF(AT.GE.TM2) THEN
!
!       ----------------------------------------------------------------
!       GOES TO NEXT RECORD : 2 BECOMES 1 AND READS A NEW 2
!       ----------------------------------------------------------------
!
        TM1=TM2
        CALL OV('X=Y     ', Z1 , Z2 , Z , C , NPOIN2)
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUMAR : LECTURE D''UN NOUVEL ENREGISTREMENT'
          WRITE(LU,*) '            DE LA HAUTEUR DE LA MAREE          '
        ELSE
          WRITE(LU,*) '   NOUMAR : READING A NEW RECORDING '
          WRITE(LU,*) '            OF THE TIDE LEVEL       '
        ENDIF
!
        IF(INDIM.EQ.3) THEN
!
!     ------------------------------------------------------------------
!       READS A SELAFIN FILE OF TYPE: TELEMAC
!     ------------------------------------------------------------------
!
 95     CONTINUE
!       READS THE DATE OF THE RECORD
        CALL LIT(DAT2B,W,IW,C1,1,'R4',NDON,BINDON,ISTAT)
        TM2=DAT2B(1)
!       READS THE DATA
        DO I =1,NVHMA
          IF(I.EQ.IDHMA) THEN
            CALL LIT(Z2,W,IW,C1,NPOIN2,'R4',NDON,BINDON,ISTAT)
          ELSE
            READ(NDON)
          ENDIF
        ENDDO
!
        IF(TM2.LE.AT) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) ' NOUMAR : ON SAUTE 1 ENREGISTREMENT ..'
          ELSE
            WRITE(LU,*) ' NOUMAR : JUMP OF 1 RECORDED DATA SERIES'
          ENDIF
          TM1=TM2
          CALL OV('X=Y     ',Z1,Z2,Z2,0.D0,NPOIN2)
          GOTO 95
        ENDIF
!
        WRITE(LU,*) 'TMENT1=',TM1
        WRITE(LU,*) 'TMENT2=',TM2
!
        ELSEIF (INDIM.EQ.4) THEN
!
!     ------------------------------------------------------------------
!        READS A USER-DEFINED FILE FORMAT
!     ------------------------------------------------------------------
!
          CALL MARUTI(X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TM1,TM2,
     &                Z1,Z2)
!
        ELSE
!
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOUMAR : INDICATEUR DE FORMAT INCONNU : ',INDIM
        ELSE
          WRITE(LU,*)'NOUMAR : UNKNOWN INDICATOR OF FORMAT : ',INDIM
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        ENDIF
!
      ENDIF
!
!     -------------------------------------------------
!       INTERPOLATES IN TIME
!       AND COMPUTES THE TEMPORAL GRADIENT OF THE TIDE
!     -------------------------------------------------
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
        STOP
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
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
100   CONTINUE
      WRITE(LU,*)'*********************************************'
      IF (LNG.EQ.1) THEN
         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE MAREE '
         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE        '
      ELSE
         WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'*********************************************'
      CALL PLANTE(1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

