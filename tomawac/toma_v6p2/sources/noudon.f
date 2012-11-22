!                    *****************
                     SUBROUTINE NOUDON
!                    *****************
!
     &(UV , VV , X  , Y  , NPOIN, NDON , BINDON, NBOR, NPTFR,
     & AT , DDC, TV1, TV2, NP   , XRELV, YRELV , UR  , VR   ,
     & TRA, U1 , V1 , U2 , V2   , INDIC, CHDON , NVAR)
!
!***********************************************************************
! TOMAWAC   V6P3                                  21/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CURRENT / WIND VELOCITY
!+                FOR THE CURRENT TIME STEP AND ON THE COMPUTATION MESH.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)
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
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| INDIC          |-->| FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NP             |<->| NUMBER OF POINTS READ FROM THE FILE
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVAR           |-->| NUMBER OF VARIABLES READ
!| TRA            |<->| WORK TABLE
!| TV1            |<->| TIME T1 IN THE DATA FILE
!| TV2            |<->| TIME T2 IN THE DATA FILE
!| U1,V1          |<->| DATA INTERPOLATED OVER THE 2D MESH AT TIME TV1
!| U2,V2          |<->| DATA INTERPOLATED OVER THE 2D MESH AT TIME TV2
!| UR,VR          |<->| TABLE OF THE VALUES READ IN THE FILE
!| UV,VV          |<--| DATA INTERPOLATED OVER THE 2D MESH AT TIME AT
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XRELV          |<--| TABLE OF THE ABSCISSAE OF THE FILE POINTS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YRELV          |<--| TABLE OF THE ORDINATES OF THE FILE POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_NOUDON => NOUDON
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NP,NDON,NPOIN,NPTFR,INDIC,I,ISTAT,NVAR,IW(1)
!
      INTEGER NBOR(NPTFR,2),ID(2)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION UV(NPOIN),VV(NPOIN),UR(NP),VR(NP)
      DOUBLE PRECISION U1(NPOIN),V1(NPOIN),U2(NPOIN),V2(NPOIN)
      DOUBLE PRECISION XRELV(NP),YRELV(NP),TRA(NP)
      DOUBLE PRECISION AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT2,DAT2B(1),Z(1),C,COEF
!
      CHARACTER*3 BINDON, C1
      CHARACTER*7 CHDON
!
!-----------------------------------------------------------------------
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN))
!
!-----------------------------------------------------------------------
!
      IF(AT.GT.TV2) THEN
!
!       ----------------------------------------------------------------
!       GOES TO NEXT RECORD : 2 BECOMES 1 AND READS A NEW 2
!       ----------------------------------------------------------------
        TV1=TV2
        CALL OV('X=Y     ', U1 , U2 , Z , C , NPOIN)
        CALL OV('X=Y     ', V1 , V2 , Z , C , NPOIN)
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUDON : LECTURE D''UN NOUVEL ENREGISTREMENT'
        ELSE
          WRITE(LU,*) '   NOUDON : READING A NEW RECORDING'
        ENDIF
!
        IF(INDIC.EQ.3) THEN
!
!     ------------------------------------------------------------------
!       READS A SELAFIN FILE OF TYPE: TELEMAC
!     ------------------------------------------------------------------
!
        ID(1)=1
        ID(2)=2
 95     CONTINUE
!       READS THE DATE OF THE RECORD
        CALL LIT(DAT2B,W,IW,C1,1,'R4',NDON,BINDON,ISTAT)
        IF(CHDON(1:1).EQ.'C') THEN
          TV2=DAT2B(1)
        ELSE
          DAT2=DAT2B(1)*1.D2
          CALL TEMP(TV2,DAT2,DDC)
        ENDIF
!      READS THE DATA
       DO I =1,NVAR
         IF(I.EQ.ID(1)) THEN
           CALL LIT(U2,W,IW,C1,NPOIN,'R4',NDON,BINDON,ISTAT)
         ELSEIF(I.EQ.ID(2)) THEN
           CALL LIT(V2,W,IW,C1,NPOIN,'R4',NDON,BINDON,ISTAT)
         ELSE
           READ(NDON)
         ENDIF
       ENDDO
!
       IF(TV2.LT.AT) THEN
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
         ENDIF
         TV1=TV2
!        INTERPOLATES IN SPACE
         CALL OV('X=Y     ',U1,U2,U2,0.D0,NPOIN)
         CALL OV('X=Y     ',V1,V2,V2,0.D0,NPOIN)
!        CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
!    &                                                   NPTFR,0.D0)
!        CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
!    &                                                   NPTFR,0.D0)
         GOTO 95
       ENDIF
!
       WRITE(LU,*) 'T',CHDON,'1:',TV1
       WRITE(LU,*) 'T',CHDON,'2:',TV2
!
       ELSEIF (INDIC.EQ.4) THEN
!
!     ------------------------------------------------------------------
!        READS A USER-DEFINED FILE FORMAT
!     ------------------------------------------------------------------
!
          IF(CHDON(1:1).EQ.'C') THEN
            CALL COUUTI(X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                  NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
          ELSE
            CALL VENUTI(X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &                  NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
          ENDIF
!
!
        ELSE
!
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOUDON : INDICATEUR DE FORMAT INCONNU : ',INDIC
        ELSE
          WRITE(LU,*)'NOUDON : UNKNOWN INDICATOR OF FORMAT : ',INDIC
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        ENDIF
!
      ENDIF
!
!       --------------------------------------------------------------
!          INTERPOLATES
!       --------------------------------------------------------------
!
      COEF=(AT-TV1)/(TV2-TV1)
      DO I=1,NPOIN
        UV(I)=(U2(I)-U1(I))*COEF+U1(I)
        VV(I)=(V2(I)-V1(I))*COEF+V1(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     FORMATS
!
20    FORMAT (10F6.2)
!
      DEALLOCATE(W)
      RETURN
!
!     IF FAILED TO READ THE FILE ...
!
100   CONTINUE
      WRITE(LU,*)'*********************************************'
      IF (LNG.EQ.1) THEN
         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE DONNEES  '
         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
      ELSE
         WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'*********************************************'
      CALL PLANTE(1)
!
      RETURN
      END

