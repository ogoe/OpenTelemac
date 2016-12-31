!                    *****************
                     SUBROUTINE LECBUS
!                    *****************
!
     &(RELAXB,NBUSE,ENTBUS,SORBUS,LRGBUS,HAUBUS,CLPBUS,
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,IFIC,MESH,
     & CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS,CIRC)
!
!***********************************************************************
! TELEMAC2D   V7P2                                   20/11/2015
!***********************************************************************
!
!brief    READS THE DATA FOR CULVERTS/TUBES/BRIDGES.
!
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Creation
!
!history  J-M HERVOUET (LNHE)
!+        30/07/2012
!+        V6P2
!+   Parallelism
!
!history S SMOLDERS
!+       20/11/15
!+       V7P1
!+   Adding global variables
!+   CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALTBUS         |<--| ELEVATION OF ENTRY AND EXIT OF CULVERTS
!| ANGBUS         |<--| ANGLE OF CULVERTS WITH AXIS OX
!|                |   |   AUTOMATICALLY COMPUTED BY DEFAULT
!| C5             |<--| CORRECTION COEFFICIENT FOR FLOW TYPE 5
!| C56            |<--| COEFFICIENT TO DIFFERENTIATE BETWEEN FLOW TYPE 5
!|                |   | AND 6
!| CEBUS          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CIRC           |<--| CULVERT ROUND (=1) OR RECTANGULAR (=0)
!| CLPBUS         |<--| INTEGER FLAG FOR FLOW DIRECTION (VALVE)
!|                |   |   0 - BOTH DIRECTIONS
!|                |   |   1 - ONLY FROM ENTRY TO EXIT
!|                |   |   2 - ONLY FROM EXIT TO ENTRY
!|                |   |   3 - NO FLOW
!| CSBUS          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| CTRASH         |<--| HEAD LOSS COEFFICIENT FOR TRASH SCREEN
!| CV             |<--| HEAD LOSS COEFFICIENT OF VALVE
!| CV5            |<--| CORRECTION COEFFICIENT FOR FLOW TYPE 5
!| ENTBUS         |<--| INDICES OF ENTRY OF CULVERTS IN GLOBAL NUMBERING
!| FRICBUS        |<--| MANNING COEFFICIENT FOR WATER FLOWING
!|                |   | OVER CULVERT MATERIAL
!| HAUBUS         |<--| HEIGHT OF CULVERTS
!| IFIC           |-->| LOGICAL UNIT OF CULVERTS DATA FILE
!| LBUS           |<--| LINEAR HEAD LOSS OF CULVERTS
!| LONGBUS        |<--| LENGTH OF CULVERTS
!| LRGBUS         |<--| WIDTH OF CULVERTS
!| MESH           |-->| MESH STRUCTURE
!| NBUSE          |-->| NUMBER OF CULVERTS
!| RELAXB         |<--| RELAXATION COEFFICIENT.
!| SORBUS         |<--| INDICES OF CULVERTS EXITS IN GLOBAL MESH NUMBERING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: IFIC,NBUSE
      INTEGER          , INTENT(INOUT) :: ENTBUS(NBUSE),SORBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: RELAXB
      DOUBLE PRECISION , INTENT(INOUT) :: HAUBUS(NBUSE,2),LRGBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: ALTBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: ANGBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: CEBUS(NBUSE,2),CSBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: LBUS(NBUSE)
      INTEGER          , INTENT(INOUT) :: CLPBUS(NBUSE),CIRC(NBUSE)
      TYPE(BIEF_MESH)  , INTENT(IN)    :: MESH
      DOUBLE PRECISION , INTENT(INOUT) :: CV(NBUSE),C56(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: CV5(NBUSE),C5(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: CTRASH(NBUSE),FRICBUS(NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: LONGBUS(NBUSE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NUMBUSE
!
      DOUBLE PRECISION XSOR,YSOR,XENT,YENT
!     DOUBLE PRECISION ANG1,ANG2
      DOUBLE PRECISION DX,DY,ANG,CE1,CE2,CS1,CS2
      DOUBLE PRECISION HAU1,HAU2,ALT1,ALT2
!
      DOUBLE PRECISION PI
!
      DOUBLE PRECISION, EXTERNAL :: P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
      PI = 4.D0 * ATAN( 1.D0 )
!      PARAMETER(PI=3.14159265358979323846D0)
!< SEB @ HRW
!
!-----------------------------------------------------------------------
!
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=998) RELAXB,NUMBUSE
      READ(IFIC,*,END=900)
!
      IF (NUMBUSE.NE.NBUSE) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECBUS : NOMBRE DE BUSES : ',NUMBUSE
          WRITE(LU,*) '         DIFFERENT DE LA VALEUR DONNEE DANS LE'
          WRITE(LU,*) '         FICHIER DES PARAMETRES :',NBUSE
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECBUS: NUMBER OF CULVERTS:',NUMBUSE
          WRITE(LU,*) '        DIFFERENT FROM THE ONE GIVEN IN THE'
          WRITE(LU,*) '        STEERING FILE: ',NBUSE
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO N=1,NBUSE
        READ(IFIC,*,ERR=997) ENTBUS(N),SORBUS(N),
     &                       CE1,CE2,CS1,CS2,
     &                       LRGBUS(N),HAU1,
     &                       CLPBUS(N),LBUS(N),
     &                       ALT1,ALT2,
     &                       CV(N),C56(N),CV5(N),
     &                       C5(N),CTRASH(N),HAU2,
     &                       FRICBUS(N),LONGBUS(N),CIRC(N)
        CEBUS(N,1)=CE1
        CEBUS(N,2)=CE2
        CSBUS(N,1)=CS1
        CSBUS(N,2)=CS2
        HAUBUS(N,1)=HAU1
        HAUBUS(N,2)=HAU2
        ALTBUS(N,1)=ALT1
        ALTBUS(N,2)=ALT2
! UNCOMMENT THE FOLLOWING LINES TO IMPOSE THE DIRECTION OF FLOW FROM THE DATA FILE
!     &                      ,ANG1,ANG2,
!
!       IN // GLOBAL VALUES REPLACED BY THE LOCAL VALUES FOR FURTHER USE
!
        ENTBUS(N)=GLOBAL_TO_LOCAL_POINT(ENTBUS(N),MESH)
        SORBUS(N)=GLOBAL_TO_LOCAL_POINT(SORBUS(N),MESH)
!
        IF(ENTBUS(N).GT.0) THEN
          XENT=MESH%X%R(ENTBUS(N))
          YENT=MESH%Y%R(ENTBUS(N))
        ELSE
          XENT=0.D0
          YENT=0.D0
        ENDIF
        IF(SORBUS(N).GT.0) THEN
          XSOR=MESH%X%R(SORBUS(N))
          YSOR=MESH%Y%R(SORBUS(N))
        ELSE
          XSOR=0.D0
          YSOR=0.D0
        ENDIF
        IF(NCSIZE.GT.1) THEN
          XENT=P_DMAX(XENT)+P_DMIN(XENT)
          YENT=P_DMAX(YENT)+P_DMIN(YENT)
          XSOR=P_DMAX(XSOR)+P_DMIN(XSOR)
          YSOR=P_DMAX(YSOR)+P_DMIN(YSOR)
        ENDIF
        DX  = XSOR-XENT
        DY  = YSOR-YENT
        ANG = ATAN(DY/DX)
        ANGBUS(N,1) = ANG
        ANGBUS(N,2) = ANG
! UNCOMMENT THE FOLLOWING LINES TO IMPOSE THE DIRECTION OF FLOW
! FROM THE DATA FILE
!        ANGBUS(N,1) = ANG1*PI/180.D0
!        ANGBUS(N,2) = ANG2*PI/180.D0
      ENDDO !  N
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECBUS : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BUSES'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECBUS: READ ERROR ON THE'
        WRITE(LU,*) '        CULVERTS DATA FILE'
        WRITE(LU,*) '        AT LINE 2'
      ENDIF
      CALL PLANTE(1)
      STOP
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECBUS : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BUSES'
        WRITE(LU,*) '         POUR LA BUSE ',N
        WRITE(LU,*) '         DONNEES ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECBUS: READ ERROR ON THE'
        WRITE(LU,*) '        CULVERTS DATA FILE'
        WRITE(LU,*) '        FOR CULVERT NUMBER ',N
        WRITE(LU,*) '        THE DATA CANNOT BE READ'
      ENDIF
      CALL PLANTE(1)
      STOP
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECBUS : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BUSES'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECBUS: READ ERROR ON THE'
        WRITE(LU,*) '        CULVERTS DATA FILE'
        WRITE(LU,*) '        UNEXPECTED END OF FILE'
      ENDIF
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
