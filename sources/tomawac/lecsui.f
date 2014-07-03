!                    *****************
                     SUBROUTINE LECSUI
!                    *****************
!
     &(F,NPLAN,NF,TETA,FREQ,NELEM2,NPOIN2,AT,UC,VC,UC1,VC1,UC2,VC2,
     & UV,VV,UV1,VV1,UV2,VV2,VENT,TV1,TV2,COURAN,NPRE,BINPRE,DEPTH,
     & TC1,TC2,ZM1,ZM2,DZHDT,TM1,TM2,MAREE,TRA01)
!
!***********************************************************************
! TOMAWAC   V6P3                                   21/06/2011
!***********************************************************************
!
!brief    READS THE DATA FOR A CONTINUATION OF COMPUTATION.
!
!warning  Parameters MAREE, COURAN or VENT must not be changed between
!+        2 runs. This would require an extra implementation consisting
!+        of checking the names of variables.
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
!history  J-M HERVOUET (EDF - LNHE)
!+        26/11/2012
!+        V6P3
!+   Correction of bugs and double precision. Changing file format
!+   for domain decomposition.
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
!| TRA01          |<--| DOUBLE PRECISION WORK TABLE OF SIZE NPOIN2*NPLAN
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPRE,NF,NPLAN,NELEM2,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NPLAN,NF),AT
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2,TC1,TC2,TM1,TM2
      DOUBLE PRECISION, INTENT(INOUT) :: TETA(NPLAN+1),FREQ(NF)
      DOUBLE PRECISION, INTENT(INOUT) :: UC(NPOIN2),VC(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: UV1(NPOIN2),VV1(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: UV2(NPOIN2),VV2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: UC1(NPOIN2),VC1(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: UC2(NPOIN2),VC2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DEPTH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZM1(NPOIN2),ZM2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DZHDT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2*NPLAN)
      LOGICAL, INTENT(IN)             :: COURAN,VENT,MAREE
      CHARACTER(LEN=3), INTENT(IN)    :: BINPRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISTAT,NPOIN,NVAR,NPL,IB(1)
      CHARACTER(LEN=72) CAR
!
      INTEGER, PARAMETER :: NFMAX = 200
      CHARACTER(LEN=32) TEXTE(NFMAX+2)
!
      DOUBLE PRECISION Z(1),ATT(1)
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2*NPLAN))
!
!***********************************************************************
!
      CALL SKIPGEO(NPRE,CAR,NPOIN,NVAR,TEXTE,NPL)
!
      IF(NPL.NE.NPLAN) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSUI : MAUVAIS NOMBRE DE PLANS DANS LE FICHIER'
          WRITE(LU,*) '         DU CALCUL PRECEDENT : ',NPL,' TROUVE'
          WRITE(LU,*) '                               ',NPLAN,' ATTENDU'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSUI: BAD NUMBER OF PLANES IN THE PREVIOUS'
          WRITE(LU,*) '        COMPUTATION FILE : ',NPL,' FOUND'
          WRITE(LU,*) '                           ',NPLAN,' EXPECTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NPOIN.NE.NPLAN*NPOIN2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'LECSUI : MAUVAIS NOMBRE DE POINTS DANS LE FICHIER'
          WRITE(LU,*)'         DU CALCUL PRECEDENT : ',NPOIN,' TROUVE'
          WRITE(LU,*)'                               ',NPOIN2,' ATTENDU'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)'LECSUI: BAD NUMBER OF POINTS IN THE PREVIOUS'
          WRITE(LU,*)'        COMPUTATION FILE : ',NPOIN,' FOUND'
          WRITE(LU,*)'                           ',NPOIN2,' EXPECTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(COURAN.OR.VENT) THEN
        I=NVAR-2
      ELSE
        I=NVAR-1
      ENDIF
!
      IF(I.NE.NF) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSUI : MAUVAIS NOMBRE DE FREQUENCES'
          WRITE(LU,*) '         DANS LE FICHIER'
          WRITE(LU,*) '         DU CALCUL PRECEDENT : ',I,' TROUVE'
          WRITE(LU,*) '                               ',NF,' ATTENDU'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSUI : BAD NUMBER OF FREQUENCIES'
          WRITE(LU,*) '         IN THE PREVIOUS'
          WRITE(LU,*) '         COMPUTATION FILE : ',I,' FOUND'
          WRITE(LU,*) '                            ',NF,' EXPECTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     PRINTS TITLE
!
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) '**** SUITE DE CALCUL ****'
        WRITE(LU,*) ' '
        WRITE(LU,*) 'TITRE DU CALCUL PRECEDENT : ',CAR
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) '**** FOLLOWING COMPUTATION ****'
        WRITE(LU,*) ' '
        WRITE(LU,*) 'TITLE OF THE PREVIOUS COMPUTATION :',CAR
      ENDIF
!
!     READS TIME
!
      CALL LIT(ATT,W,IB,CAR,1,'R8',NPRE,BINPRE,ISTAT)
      AT = ATT(1)
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) '- REPRISE DE CALCUL AU TEMPS  ',AT
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) '- COMPUTATIONAL RESUMPTION AT TIME ',AT
      ENDIF
!
!     READS F
!
      DO I=1,NF
        CALL LIT(F(1,1,I),W,IB,CAR,NPOIN2*NPLAN,
     &           'R8',NPRE,BINPRE,ISTAT)
      ENDDO
!
!     READS DEPTH (ALWAYS WRITTEN, EVEN IF NOT RELEVANT)
!
      IF(MAREE) THEN
        CALL LIT(DEPTH,W,IB,CAR,NPOIN2,'R8',NPRE,BINPRE,ISTAT)
!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TM1=AT
        TM2=AT
        CALL OV( 'X=Y     ' , ZM1 , DEPTH , Z , 0.D0   , NPOIN2)
        CALL OV( 'X=Y     ' , ZM2 , DEPTH , Z , 0.D0   , NPOIN2)
        CALL OV( 'X=C     ' , DZHDT , DEPTH , Z , 0.D0 , NPOIN2)
      ELSE
        CALL LIT(TRA01,W,IB,CAR,1,'R8',NPRE,BINPRE,ISTAT)
      ENDIF
!
!     READS UC,VC,UV,VV IF HAS TO
!
      IF(COURAN.OR.VENT) THEN
        IF(VENT) THEN
          CALL LIT(TRA01,W,IB,CAR,4*NPOIN2,'R8',NPRE,BINPRE,ISTAT)
        ELSE
          CALL LIT(TRA01,W,IB,CAR,2*NPOIN2,'R8',NPRE,BINPRE,ISTAT)
        ENDIF
      ENDIF
!
      IF(COURAN) THEN
        CALL OV('X=Y     ',UC,TRA01(       1:  NPOIN2),Z,0.D0,NPOIN2)
        CALL OV('X=Y     ',VC,TRA01(NPOIN2+1:2*NPOIN2),Z,0.D0,NPOIN2)
!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TC1=AT
        TC2=AT
        CALL OV( 'X=Y     ' , UC1 , UC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , UC2 , UC , Z , 0.D0 , NPOIN2)    
        CALL OV( 'X=Y     ' , VC1 , VC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VC2 , VC , Z , 0.D0 , NPOIN2)
      ENDIF
!
      IF(VENT) THEN
        CALL OV('X=Y     ',UV,TRA01(2*NPOIN2+1:3*NPOIN2),Z,0.D0,NPOIN2)
        CALL OV('X=Y     ',VV,TRA01(3*NPOIN2+1:4*NPOIN2),Z,0.D0,NPOIN2)
!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TV1=AT
        TV2=AT
        CALL OV( 'X=Y     ' , UV1 , UV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , UV2 , UV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VV1 , VV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VV2 , VV , Z , 0.D0 , NPOIN2)
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
