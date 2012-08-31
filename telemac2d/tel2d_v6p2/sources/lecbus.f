!                    ***************** 
                     SUBROUTINE LECBUS 
!                    ***************** 
! 
     &(RELAXB,NBUSE,ENTBUS,SORBUS,LRGBUS,HAUBUS,CLPBUS, 
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,IFIC,MESH) 
! 
!*********************************************************************** 
! TELEMAC2D   V6P2                                   23/05/2012 
!*********************************************************************** 
! 
!brief    READS THE DATA FOR TUBES/BRIDGES. 
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| ALTBUS         |<--| ELEVATION OF ENTRY AND EXIT OF TUBES 
!| ANGBUS         |<--| ANGLE OF TUBES WITH AXIS OX 
!|                |   |   AUTOMATICALLY COMPUTED BY DEFAULT 
!| CEBUS          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW 
!| CLPBUS         |<--| INTEGER FLAG FOR FLOW DIRECTION (VALVE) 
!|                |   |   0 - BOTH DIRECTIONS 
!|                |   |   1 - ONLY FROM ENTRY TO EXIT 
!|                |   |   2 - ONLY FROM EXIT TO ENTRY 
!|                |   |   3 - NO FLOW 
!| CSBUS          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW 
!| ENTBUS         |<--| INDICES OF ENTRY OF TUBES IN GLOBAL NUMBERING 
!| HAUBUS         |<--| HEIGHT OF TUBES 
!| IFIC           |-->| LOGICAL UNIT OF TUBES DATA FILE 
!| LBUS           |<--| LINEAR HEAD LOSS OF TUBES 
!| LRGBUS         |<--| WIDTH OF TUBES 
!| NBUSE          |<--| NUMBER OF TUBES 
!| RELAXB         |<--| RELAXATION COEFFICIENT. 
!| SORBUS         |<--| INDICES OF TUBES EXITS IN GLOBAL MESH NUMBERING 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER          , INTENT(IN)    :: IFIC,NBUSE 
      INTEGER          , INTENT(INOUT) :: ENTBUS(NBUSE),SORBUS(NBUSE) 
      DOUBLE PRECISION , INTENT(INOUT) :: RELAXB 
      DOUBLE PRECISION , INTENT(INOUT) :: HAUBUS(NBUSE),LRGBUS(NBUSE) 
      DOUBLE PRECISION , INTENT(INOUT) :: ALTBUS(NBUSE,2) 
      DOUBLE PRECISION , INTENT(INOUT) :: ANGBUS(NBUSE,2) 
      DOUBLE PRECISION , INTENT(INOUT) :: CEBUS(NBUSE,2),CSBUS(NBUSE,2) 
      DOUBLE PRECISION , INTENT(INOUT) :: LBUS(NBUSE) 
      INTEGER          , INTENT(INOUT) :: CLPBUS(NBUSE) 
      TYPE(BIEF_MESH)  , INTENT(IN)    :: MESH  
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER N,I
! 
      DOUBLE PRECISION XSOR,YSOR,XENT,YENT
      DOUBLE PRECISION ANG1,ANG2 
      DOUBLE PRECISION DX,DY,ANG 
! 
      DOUBLE PRECISION PI 
      PARAMETER(PI=3.14159265358979323846D0) 
!
      DOUBLE PRECISION, EXTERNAL :: P_DMAX,P_DMIN
! 
!----------------------------------------------------------------------- 
! 
      READ(IFIC,*,END=900) 
      READ(IFIC,*,ERR=998) RELAXB 
      READ(IFIC,*,END=900) 
! 
      DO 10 N=1,NBUSE 
        READ(IFIC,*,ERR=997) ENTBUS(N),SORBUS(N), 
     &                       CEBUS(N,1),CEBUS(N,2),
     &                       CSBUS(N,1),CSBUS(N,2), 
     &                       LRGBUS(N),HAUBUS(N), 
     &                       CLPBUS(N),LBUS(N), 
     &                       ALTBUS(N,1),ALTBUS(N,2) 
! UNCOMMENT THE FOLLOWING LINE TO IMPOSE THE DIRECTION OF FLOW FROM THE DATA FILE 
!     &                      ,ANG1,ANG2 
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
        ANG = DATAN(DY/DX) 
        ANGBUS(N,1) = ANG 
        ANGBUS(N,2) = ANG 
! UNCOMMENT THE FOLLOWING LINE TO IMPOSE THE DIRECTION OF FLOW FROM THE DATA FILE 
!        ANGBUS(N,1) = ANG1*PI/180.D0 
!        ANGBUS(N,2) = ANG2*PI/180.D0 
10    CONTINUE 
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
        WRITE(LU,*) 'LECBUS : READ ERROR ON THE' 
        WRITE(LU,*) '         TUBES DATA FILE' 
        WRITE(LU,*) '         AT LINE 2' 
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
        WRITE(LU,*) 'LECBUS : READ ERROR ON THE' 
        WRITE(LU,*) '         TUBES DATA FILE' 
        WRITE(LU,*) '         FOR TUBE NUMBER ',N 
        WRITE(LU,*) '         THE DATA CANNOT BE READ' 
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
        WRITE(LU,*) 'LECBUS : READ ERROR ON THE' 
        WRITE(LU,*) '         TUBES DATA FILE' 
        WRITE(LU,*) '         UNEXPECTED END OF FILE' 
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
