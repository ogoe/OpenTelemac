!                    *****************
                     SUBROUTINE LECSUI
!                    *****************
!
     &(F,NPLAN,NF,TETA,FREQ,NELEM2,NPOIN2,AT,UC,VC,UC1,VC1,UC2,VC2,
     & UV,VV,UV1,VV1,UV2,VV2,VENT,TV1,TV2,
     & COURAN,NPRE,BINPRE,DEPTH,TC1,TC2,ZM1,ZM2,DZHDT,TM1,TM2,MAREE)
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/06/2011
!***********************************************************************
!
!brief    READS THE DATA FOR A CONTINUATION OF COMPUTATION.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINPRE         |-->| PREVIOUS COMPUTATION FILE BINARY
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| DEPTH          |<--| WATER DEPTH
!| DZHDT          |<--| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| F              |<--| DIRECTIONAL SPECTRUM
!| FREQ           |<--| DISCRETIZED FREQUENCY
!| MAREE          |-->| LOGICAL INDICATING CONSIDERATION OF TIDE
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRE           |-->| LOIGCAL UNIT NUMBER OF PREVIOUS COMPUTATION FILE
!| TC1            |<--| TIME T1 OF CURRENT IN PREVIOUS COMPUTATION FILE
!| TC2            |<--| TIME T2 OF CURRENT IN PREVIOUS COMPUTATION FILE
!| TETA           |<--| DISCRETIZED DIRECTIONS
!| TM1            |<--| TIME T1 OF TIDE IN PREVIOUS COMPUTATION FILE
!| TM2            |<--| TIME T2 OF TIDE IN PREVIOUS COMPUTATION FILE
!| TV1            |<--| TIME T1 OF WIND IN PREVIOUS COMPUTATION FILE
!| TV2            |<--| TIME T2 OF WIND IN PREVIOUS COMPUTATION FILE
!| UC, VC         |<--| CURRENT VELOCITY COMPONENTS
!| UC1, VC1       |<--| CURRENT VELOCITY COMPONENTS AT TC1
!| UC2, VC2       |<--| CURRENT VELOCITY COMPONENTS AT TC2
!| UV,VV          |<--| WIND VELOCITY COMPONENTS
!| UV1, VV1       |<--| WIND VELOCITY COMPONENTS AT TV1
!| UV2, VV2       |<--| WIND VELOCITY COMPONENTS AT TV2
!| VENT           |-->| LOGICAL INDICATING IF THERE IS A WIND
!| ZM1            |<--| WATER DEPTH AT TM1
!| ZM2            |<--| WATER DEPTH AT TM2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NPRE,NF,NPLAN,NELEM2,NPOIN2
      INTEGER I,ISTAT,IB(2),NTOT
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),AT,ATT(1)
      DOUBLE PRECISION TETA(NPLAN+1),FREQ(NF),PI,TV1,TV2,Z(1)
      DOUBLE PRECISION UC(NPOIN2),VC(NPOIN2),UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION UV1(NPOIN2),VV1(NPOIN2),UV2(NPOIN2),VV2(NPOIN2)
      DOUBLE PRECISION UC1(NPOIN2),VC1(NPOIN2),UC2(NPOIN2),VC2(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2),ZM1(NPOIN2),ZM2(NPOIN2)
      DOUBLE PRECISION DZHDT(NPOIN2)
      DOUBLE PRECISION TC1,TC2,TM1,TM2
!
      LOGICAL COURAN,VENT,MAREE
!
      CHARACTER*3 BINPRE
      CHARACTER*72 CAR
!
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2*NPLAN*NF))
!
!***********************************************************************
!
      PI=3.141592D0
      REWIND NPRE
!
!     READS TITLE
!
      CALL LIT(F,W,IB,CAR,72,'CH',NPRE,BINPRE,ISTAT)
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) '**** SUITE DE CALCUL ****'
         WRITE(LU,*) ' '
         WRITE(LU,*) 'TITRE DU CALCUL PRECEDENT :'
         WRITE(LU,*) '     ',CAR
      ELSE
         WRITE(LU,*) '**** FOLLOWING COMPUTATION ****'
         WRITE(LU,*) ' '
         WRITE(LU,*) 'TITLE OF THE PREVIOUS COMPUTATION :'
      ENDIF
      WRITE(LU,*) '     ',CAR
!
!     READS NPLAN, NF AND PERFORMS CHECK
!
      CALL LIT(F,W,IB,CAR,2,'I ',NPRE,BINPRE,ISTAT)
      IF ((IB(1).NE.NPLAN).OR.(IB(2).NE.NF)) THEN
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) '**** ERREUR DANS LECSUI : ****'
         WRITE(LU,*) 'LE NOMBRE DE DIRECTIONS ET/OU CELUI DE FREQUENCES'
         WRITE(LU,*) ' NE CORRESPOND PAS'
         WRITE(LU,*) 'VALEURS LUES : NDIR=',IB(1),' NF=',IB(2)
         WRITE(LU,*) 'VALEURS ATTENDUES : NDIR=',NPLAN,' NF=',NF
       ELSE
         WRITE(LU,*) '**** ERROR IN LECSUI : ****'
         WRITE(LU,*) 'THE NUMBER OF DIRECTIONS AND/OR FREQUENCIES'
         WRITE(LU,*) '   IS NOT CORRESPONDING '
         WRITE(LU,*) 'READ VALUES : NDIR=',IB(1),' NF=',IB(2)
         WRITE(LU,*) 'EXPECTED VALUES : NDIR=',NPLAN,' NF=',NF
       ENDIF
       CALL PLANTE(0)
      ENDIF
!
!     READS NELEM2, NPOIN2 AND PERFORMS CHECK
!
      CALL LIT(F,W,IB,CAR,2,'I ',NPRE,BINPRE,ISTAT)
      IF ((IB(1).NE.NELEM2).OR.(IB(2).NE.NPOIN2)) THEN
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) '**** ERREUR DANS LECSUI ****'
         WRITE(LU,*) 'LE NOMBRE DE POINTS ET/OU CELUI D''ELEMENTS 2D NE'
         WRITE(LU,*) 'CORRESPOND PAS'
         WRITE(LU,*) 'VALEURS LUES : NELEM2=',IB(1),' NPOIN2=',IB(2)
         WRITE(LU,*) 'VALEURS ATTENDUES : NELEM2=',NELEM2,
     &               ' NPOIN2=',NPOIN2
       ELSE
         WRITE(LU,*) '**** ERROR IN LECSUI : ****'
         WRITE(LU,*) 'THE NUMBER OF POINTS AND/OR 2D ELEMENTS '
         WRITE(LU,*) '   IS NOT CORRESPONDING '
         WRITE(LU,*) 'READ VALUES     : NELEM2=',IB(1),' NPOIN2=',IB(2)
         WRITE(LU,*) 'EXPECTED VALUES : NELEM2=',NELEM2,
     &               ' NPOIN2=',NPOIN2
       ENDIF
       CALL PLANTE(0)
      ENDIF
!
!     READS TIME STAMP
!
      CALL LIT(ATT,W,IB,CAR,1,'R4',NPRE,BINPRE,ISTAT)
      AT = ATT(1)
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) '- REPRISE DE CALCUL AU TEMPS  ',AT
      ELSE
         WRITE(LU,*) '- COMPUTATIONAL RESUMPTION AT TIME ',AT
      ENDIF
!
!     READS TETA
!
      CALL LIT(TETA,W,IB,CAR,NPLAN,'R4',NPRE,BINPRE,ISTAT)
      TETA(NPLAN+1)=2.D0*PI+TETA(1)
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'DISTRIBUTION DES DIRECTIONS :'
      ELSE
         WRITE(LU,*) 'DISTRIBUTION OF THE DIRECTIONS:'
      ENDIF
      DO I=1,NPLAN
        WRITE(LU,*) '       ',TETA(I)*180/PI,' DEGRES'
      ENDDO
!
!     READS FREQ
!
      CALL LIT(FREQ,W,IB,CAR,NF,'R4',NPRE,BINPRE,ISTAT)
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DISTRIBUTION DES FREQUENCES :'
      ELSE
        WRITE(LU,*) 'DISTRIBUTION OF THE FREQUENCIES:'
      ENDIF
      DO I=1,NF
        WRITE(LU,*) '       ',FREQ(I),' HERTZ'
      ENDDO
!
!     READS F
!
      NTOT=NPOIN2*NPLAN*NF
      CALL LIT(F,W,IB,CAR,NTOT,'R4',NPRE,BINPRE,ISTAT)
!
!     READS U,V,UV,VV IF HAS TO
!
      IF (COURAN) THEN
      CALL LIT(UC ,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
      CALL LIT(VC ,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
!
!     SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
!
      TC1=AT
      TC2=AT
        CALL OV( 'X=Y     ' , UC1 , UC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , UC2 , UC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VC1 , VC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VC2 , VC , Z , 0.D0 , NPOIN2)
      ENDIF
!
      IF (VENT) THEN
      CALL LIT(UV,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
      CALL LIT(VV,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
!
!     SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
!
      TV1=AT
      TV2=AT
        CALL OV( 'X=Y     ' , UV1 , UV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , UV2 , UV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VV1 , VV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VV2 , VV , Z , 0.D0 , NPOIN2)
      ENDIF
!
      IF (MAREE) THEN
      CALL LIT(DEPTH ,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
!
!     SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
!
      TM1=AT
      TM2=AT
        CALL OV( 'X=Y     ' , ZM1 , DEPTH , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , ZM2 , DEPTH , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=C     ' , DZHDT , DEPTH , Z , 0.D0 , NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(W)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
