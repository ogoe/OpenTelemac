!                    *****************
                     SUBROUTINE LECSUI
!                    *****************
!
     &(F,NPLAN,NF,NPOIN2,AT,UC,VC,UC1,VC1,UC2,VC2,
     & UV,VV,UV1,VV1,UV2,VV2,VENT,TV1,TV2,COURAN,NPRE,FFORMAT,DEPTH,
     & TC1,TC2,ZM1,ZM2,DZHDT,TM1,TM2,MAREE,TRA01)
!
!***********************************************************************
! TOMAWAC   V7P1
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| DEPTH          |<--| WATER DEPTH
!| DZHDT          |<--| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| F              |<--| DIRECTIONAL SPECTRUM
!| MAREE          |-->| LOGICAL INDICATING CONSIDERATION OF TIDE
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRE           |-->| LOIGCAL UNIT NUMBER OF PREVIOUS COMPUTATION FILE
!| TC1            |<--| TIME T1 OF CURRENT IN PREVIOUS COMPUTATION FILE
!| TC2            |<--| TIME T2 OF CURRENT IN PREVIOUS COMPUTATION FILE
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
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPRE,NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NPLAN,NF),AT
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2,TC1,TC2,TM1,TM2
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
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISTAT,NPOIN,NVAR,NPL
      INTEGER NELEM, NPTFR, NPTIR2, NDP, TYP_ELEM
      INTEGER NTIMESTEP
      CHARACTER(LEN=80) CAR
!
      INTEGER NVA3
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:),VAR_UNIT(:)
!, VAR_UNIT(:)
!      INTEGER, PARAMETER :: NFMAX = 200
!      CHARACTER(LEN=32) TEXTE(NFMAX+2)
!
      DOUBLE PRECISION Z(1)
!
!***********************************************************************
!
      CALL READ_MESH_INFO(FFORMAT,NPRE,CAR,NVAR,NPOIN,TYP_ELEM,
     &                    NELEM,NPTFR,NPTIR2,NDP,NPL)
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
      CALL GET_DATA_NTIMESTEP(FFORMAT,NPRE,NTIMESTEP,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_NTIMESTEP')
      CALL GET_DATA_TIME(FFORMAT,NPRE,NTIMESTEP-1,AT,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_TIME')
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) '- REPRISE DE CALCUL AU TEMPS  ',AT
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) '- COMPUTATIONAL RESUMPTION AT TIME ',AT
      ENDIF
!
!
! Get the number of variables
      CALL GET_DATA_NVAR(FFORMAT,NPRE,NVA3,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_NVAR')

      ALLOCATE(VAR_NAME(NVA3),STAT=ISTAT)
      CALL CHECK_ALLOCATE(ISTAT,'LECSUI:VAR_NAME')
      ALLOCATE(VAR_UNIT(NVA3),STAT=ISTAT)
      CALL CHECK_ALLOCATE(ISTAT,'LECSUI:VAR_UNIT')
!
      CALL GET_DATA_VAR_LIST(FFORMAT,NPRE,NVA3,VAR_NAME,VAR_UNIT,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_VAR_LIST')
!READS F
!

      DO I=1,NF
        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(I),F(1,1,I),NPOIN,
     &                 ISTAT,RECORD=-1)
        CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
      ENDDO
!
!     READS DEPTH (ALWAYS WRITTEN, EVEN IF NOT RELEVANT)
!
      IF(MAREE) THEN
!        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(I),F(1,1,I),NPOIN2,
        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+1),TRA01,NPOIN,
     &                 ISTAT,RECORD=-1)
        CALL OV('X=Y     ',DEPTH,TRA01(       1:  NPOIN2),Z,0.D0,NPOIN2)
        
!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TM1=AT
        TM2=AT
        CALL OV( 'X=Y     ' , ZM1 , DEPTH , Z , 0.D0   , NPOIN2)
        CALL OV( 'X=Y     ' , ZM2 , DEPTH , Z , 0.D0   , NPOIN2)
        CALL OV( 'X=C     ' , DZHDT , DEPTH , Z , 0.D0 , NPOIN2)
      ELSE
        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+1),TRA01,1,
     &                 ISTAT,RECORD=-1)
        CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
      ENDIF
!
!     READS UC,VC,UV,VV IF HAS TO
!
      IF(COURAN.OR.VENT) THEN
        IF(VENT) THEN
          CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+2),TRA01,4*NPOIN2,
     &                   ISTAT,RECORD=-1)
          CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
        ELSE
          CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+2),TRA01,2*NPOIN2,
     &                   ISTAT,RECORD=-1)
          CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
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
      RETURN
      END
