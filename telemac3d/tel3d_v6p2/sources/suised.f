!                    *****************
                     SUBROUTINE SUISED
!                    *****************
!
     &(IVIDE,EPAI,HDEP,CONC,TEMP,FLUER,PDEPOT,ZR,ZF,NPF,
     & NPOIN2,NPOIN3,NPFMAX,NCOUCH,TASSE,GIBSON,NSUIS,BISUIS)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS SEDIMENTOLOGICAL DATA
!+                FOR A RESUMING COMPUTATION.
!
!warning  IMPLEMENTATION WITH SELAFIN ONLY WORKS WITH GIBSON
!
!history  C LE NORMANT (LNH)
!+        05/05/93
!+
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  S.E.BOURBAN AND N.DURAND (NRC-CHC)
!+        27/03/06
!+        V5P7
!+   SELAFIN IMPLEMENTATION
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BISUIS         |-->| BINARY OF PREVIOUS SEDIMENT COMPUTATION FILE
!| CONC           |<--| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| EPAI           |<--| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ TOTAL BED THICKNESS)
!|                |   | WITH GIBSON MODEL
!| FLUER          |<--| EROSION FLUX FOR EACH 2D POINT
!| GIBSON         |-->| LOGICAL FOR GIBSON SETTLING MODEL
!| HDEP           |<--| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |<--| VOID INDEX OF MESH POINTS
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPF            |<--| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUD BED (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NSUIS          |-->| LOGICAL UNIT NUMBER OF THE PREVIOUS SEDIMENT
!|                |   | COMPUTATION FILE
!| PDEPOT         |<--| PROBABILITY OF DEPOSITION FOR EACH 2D POINT
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TEMP           |<->| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| ZF             |<--| ELEVATION OF MUD BOTTOM
!| ZR             |<--| ELEVATION OF RIDIG BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      USE DECLARATIONS_TELEMAC3D, ONLY: RHOS
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-CHANGES
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)           :: NPOIN2, NPOIN3, NPFMAX, NCOUCH
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!      DOUBLE PRECISION, INTENT(OUT) :: IVIDE(NPFMAX*NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: IVIDE(NPFMAX,NPOIN2)
!      DOUBLE PRECISION, INTENT(OUT) :: EPAI((NPFMAX-1)*NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: CONC(NCOUCH)
!      DOUBLE PRECISION, INTENT(OUT) :: TEMP(NCOUCH*NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-CHANGES
      DOUBLE PRECISION, INTENT(OUT) :: ZR(NPOIN2), ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: FLUER(NPOIN2), PDEPOT(NPOIN2)
      INTEGER, INTENT(OUT)          :: NPF(NPOIN2)
      INTEGER, INTENT(IN)           :: NSUIS
      CHARACTER(LEN=3), INTENT(IN)  :: BISUIS
      LOGICAL, INTENT(IN)           :: TASSE, GIBSON
!
!----------------------------------------------------------------------
!
      INTEGER IPOIN, IC
      INTEGER IENRE  , ITER   , NENRE  , ISTAT
!
      INTEGER IB(10), N, ERR
      DOUBLE PRECISION, ALLOCATABLE :: XB(:)
      REAL, ALLOCATABLE :: RB(:)
      CHARACTER(LEN=80) CB
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      INTEGER NVAR, I, IPLAN,JPLAN
      DOUBLE PRECISION UNITCONV, ECOUCH,ZPLAN
! READS THE HEADER AND CHECKS QUANTITIES / OPTIONS AND ALLOCATES MEMORY
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*)
     &   'LECTURE DU FICHIER DE SEDIMENT PRECEDENT'
      IF(LNG.EQ.2) WRITE(LU,*)
     &   'READING PREVIOUS COMPUTATION FILE FOR SEDIMENT'
      WRITE(LU,*) ' '
!
!   LEC/ECR 1: NAME OF THE GEOMETRY FILE
      CALL LIT(XB,RB,IB,CB,80,'CH',NSUIS,BISUIS,ISTAT)
      IF(LNG.EQ.1) WRITE(LU,*) 'TITRE : ',CB(1:72)
      IF(LNG.EQ.2) WRITE(LU,*) 'TITTLE: ',CB(1:72)
!
!   LEC/ECR 2: NUMBER OF 1 AND 2 DISCRETISATION FUNCTIONS
      CALL LIT(XB,RB,IB,CB,2,'I',NSUIS,BISUIS,ISTAT)
      IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE VARIABLES : ',IB(1)
      IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF VARIABLES: ',IB(1)
      IF(LNG.EQ.1) WRITE(LU,*) 'ET DE VARIABLES CLANDESTINES: ',IB(2)
      IF(LNG.EQ.2) WRITE(LU,*) 'AND OF CLANDESTINE VARIABLES: ',IB(2)
      IF( (IB(1).NE.4 .AND.GIBSON).OR.(IB(1).NE.4 .AND.TASSE) ) THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     &'SUISED : CONSOLIDATION NON COMPATIBLE AVEC NOMBRE DE VARIABLES'
         IF(LNG.EQ.2) WRITE(LU,*)
     &'SUISED: CONSOLIDATION NOT COMPATIBLE WITH NUMBER OF VARIABLES'
         STOP
      ENDIF
      NVAR = IB(1)+IB(2)
!
!   LEC/ECR 3: VARIABLES NAMES AND UNITS
      DO I = 1,NVAR
         CALL LIT(XB,RB,IB,CB,32,'CH',NSUIS,BISUIS,ISTAT)
         IF(LNG.EQ.1) WRITE(LU,*) I,".- ",CB(1:16)
         IF(LNG.EQ.2) WRITE(LU,*) I,".- ",CB(1:16)
      ENDDO
!
!   LEC/ECR 4: LIST OF 10 INTEGER PARAMETERS (AND DATE)
      CALL LIT(XB,RB,IB,CB,10,'I',NSUIS,BISUIS,ISTAT)
      WRITE(LU,*) IB(1),IB(1),IB(2),IB(3),IB(4),IB(5),IB(6),IB(7),IB(8)
      IF( (IB(7).NE.NPFMAX .AND.GIBSON).OR.
     &                           (IB(7).NE.NCOUCH .AND.TASSE) ) THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     &'SUISED : NOMBRE DE COUCHES NON COMPATIBLE AVEC CONSOLIDATION'
         IF(LNG.EQ.2) WRITE(LU,*)
     &'SUISED: NUMBER OF LAYERS NOT COMPATIBLE WITH CONSOLIDATION'
         STOP
      ENDIF
!
!   LEC/ECR 5: 4 INTEGERS
      CALL LIT(XB,RB,IB,CB,4,'I',NSUIS,BISUIS,ISTAT)
      WRITE(LU,*) IB(1),IB(1),IB(2),IB(3),IB(4)
      IF( (IB(2).NE.(IB(7)*NPOIN2)) .OR. (IB(3).NE.6) ) THEN
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'SUISED : NOMBRE DE NOEUDS NON COMPATIBLE'
       ENDIF
       IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'SUISED: NUMBER OF NODES NOT COMPATIBLE'
       ENDIF
       CALL PLANTE(1)
       STOP
      ENDIF
!
!   LEC/ECR 6: IKLE
      READ(NSUIS)
!
!   LEC/ECR 7: IPOBO (NOT IN PARALLEL MODE)
      READ(NSUIS)
!
!   LEC/ECR 8 AND 9: X AND Y COORDINATES OF THE GRID NODES
      READ(NSUIS)
      READ(NSUIS)
!
!#####< SEB-CHANGES
!
!-----------------------------------------------------------------------
! ALLOCATES A (SIMPLE) REAL VECTOR, LENGTH N
!
!#####> SEB-CHANGES
      IF (TASSE) THEN
         ALLOCATE(XB(NCOUCH*NPOIN2),STAT=ERR)
         ALLOCATE(RB(NCOUCH*NPOIN2),STAT=ERR)
      ELSEIF (GIBSON) THEN
         ALLOCATE(XB(NPFMAX*NPOIN2),STAT=ERR)
         ALLOCATE(RB(NPFMAX*NPOIN2),STAT=ERR)
      ENDIF
!      N = MAX(NPFMAX*NPOIN2, NPOIN3)
!      ALLOCATE(RB(N),STAT=ERR)
!#####< SEB-CHANGES
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SUISED : ALLOCATION DE RB DEFECTUEUSE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SUISED : WRONG ALLOCATION OF RB'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!#####> SEB-CHANGES
! CASE OF SELAFIN 3D FORMAT - THE HEADER HAS ALREADY BEEN READ
      NENRE = 0
      DO
         READ(NSUIS,END=1901)          ! AT
         DO I = 1,NVAR
            READ(NSUIS)
         ENDDO
         NENRE = NENRE + 1
      ENDDO
 1901 CONTINUE
!                          > MOVE TO BEFORE-LAST RECORD
      REWIND NSUIS
      DO I = 1,2+NVAR+4+2     ! HEADER
         READ(NSUIS)
      ENDDO
      DO ITER = 1,NENRE-1     ! RECORDS
         READ(NSUIS)
         DO I = 1,NVAR
            READ(NSUIS)
         ENDDO
      ENDDO
      UNITCONV = 1.D0                      ! VARIABLES ARE SCALED
!                          > READS LAST RECORD
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*)
     &   'LECTURE DU DERNIER DES ENREGISTREMENTS PRECEDENTS'
      IF(LNG.EQ.2) WRITE(LU,*)
     &   'READING THE LAST OF THE PREVIOUS RECORDS'
      WRITE(LU,*) ' '
      READ(NSUIS)             ! AT
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  /!\ THIS PART SHOULD BE ENTIRELY REVISITED ...
      IF (TASSE) THEN
! ELEVATION Z (MOSTLY FOR PLOTTING PURPOSES)
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,NPOIN2
!>            EPAI(IPOIN) = DBLE( RB(IPOIN) ) - ZR(IPOIN)
!            ZF(IPOIN) = DBLE( RB(IPOIN+(NCOUCH-1)*NPOIN2) )
         ENDDO
! THICKNESS
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,(NCOUCH-1)*NPOIN2
!>            EPAI(IPOIN+NPOIN2) = DBLE( RB(IPOIN) ) * UNITCONV
         ENDDO
         DO IPOIN = 1,NPOIN2
            HDEP(IPOIN) = DBLE( RB(IPOIN+(NCOUCH-1)*NPOIN2) )*UNITCONV
         ENDDO
! CONCENTRATION
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPLAN = 1,NCOUCH
            CONC(IPLAN) = DBLE( RB(1+(IPLAN-1)*NPOIN2) ) * UNITCONV
         ENDDO
! TIME OF CONSOLIDATION
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,NCOUCH*NPOIN2
!>            TEMP(IPOIN) = DBLE( RB(IPOIN) )
         ENDDO
         DEALLOCATE(XB)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ELSEIF (GIBSON) THEN
!> ELEVATION Z (TEMPORARILY STORES TRUE LAYER THICKNESS)
         CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,NPOIN2
            ZR(IPOIN) = DBLE( RB(IPOIN) )
            ZPLAN = ZR(IPOIN)
            DO IPLAN = 1,NPFMAX-1
               EPAI(IPLAN,IPOIN) =
     &            DBLE( RB(IPOIN+(IPLAN-1)*NPOIN2) ) - ZPLAN
               ZPLAN = ZPLAN + EPAI(IPLAN,IPOIN)
            ENDDO
            ZF(IPOIN) = DBLE( RB(IPOIN+(NPFMAX-1)*NPOIN2) )
!            HDEB(IPOIN) = ZF(IPOIN) - ZPLAN
         ENDDO
!> TRUE THICKNESS AND NUMBER OF LAYERS
         CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,NPOIN2
            NPF(IPOIN) = INT( RB(IPOIN) )
            IF( NPF(IPOIN).GT.NPFMAX ) THEN
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'SUISED : NOMBRE DE COUCHE MAXIMAL NON VALIDE'
         ENDIF
         IF(LNG.EQ.2) THEN
           WRITE(LU,*) 'SUISED: MAXIMUM NUMBER OF LAYERS NOT VALID'
         ENDIF
           CALL PLANTE(1)
           STOP
           ENDIF
           HDEP(IPOIN)=DBLE(RB(IPOIN+(NPFMAX-1)*NPOIN2))*UNITCONV
         ENDDO
!> TRUE DENSITY AND RENUMBERING
         CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1, NPOIN2
            DO IPLAN = NPFMAX,NPF(IPOIN)+1,-1
               IVIDE(IPLAN,IPOIN) = 0.D0
            ENDDO
            JPLAN = NPFMAX
            DO IPLAN = NPF(IPOIN),1,-1
               IVIDE(IPLAN,IPOIN) =
     &            RHOS/DBLE( RB(IPOIN+(JPLAN-1)*NPOIN2) ) - 1.D0
               JPLAN = JPLAN - 1
            ENDDO
         ENDDO
!> VIRTUAL THICKNESS EPAI AND RENUMBERING
         DO IPOIN = 1,NPOIN2
            JPLAN = NPFMAX-NPF(IPOIN)+1
            DO IPLAN = 1,NPF(IPOIN)-1
               ECOUCH =
     &         ( IVIDE(IPLAN,IPOIN) + IVIDE(IPLAN+1,IPOIN) )/2.D0
               EPAI(IPLAN,IPOIN) = EPAI(JPLAN,IPOIN)/( 1.D0+ECOUCH )
               JPLAN = JPLAN + 1
            ENDDO
            DO IPLAN = NPF(IPOIN),NPFMAX-1
               EPAI(IPLAN,IPOIN) = 0.D0
            ENDDO
         ENDDO
!> LAYER IPF: ONLY USEFUL FOR PLOTTING PURPOSES
!         CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DEALLOCATE(XB)
      ENDIF
!#####< SEB-CHANGES
!#####> SEB-CHANGES
!!-----------------------------------------------------------------------
!!
!! COUNTS THE NUMBER OF RECORDS TO GO TO LAST
!             REWIND NSUIS
!
!! READS THE FIRST 3 RECORDS
!             DO IENRE = 1,3
!                READ(NSUIS,ERR=50)
!             END DO
!!
!! READS THE RECORDS CORRESPONDING TO 1 TIMESTEP
!             NENRE  = 0
!! THERE ARE ...  RECORDS PER TIMESTEP
!             NCOMPT = 2
!             IF ( (TASSE).OR.(GIBSON) ) NCOMPT = 5
!30           CONTINUE
!             DO ICOMPT = 1,NCOMPT
!                READ(NSUIS,ERR=50,END=60)
!             END DO
!             NENRE = NENRE + 1
!             GOTO 30
!50           CONTINUE
!             IF (LNG==1) THEN
!               WRITE(LU,*) 'SOUS-PROG SUISED'
!               WRITE(LU,*)
!     &          'ERREUR LECTURE DU FICHIER SUITE SEDIMENTOLOGIQUE'
!             ELSE
!               WRITE(LU,*) 'SUBROUTINE SUISED'
!               WRITE(LU,*)
!     &          'ERROR READING THE PREV. COMPUTATION FILE FOR SEDIMENT'
!             ENDIF
!             CALL PLANTE(1)
!             STOP
!60           CONTINUE
!!
!             REWIND NSUIS
!!
!! SKIPS THE TITLE AND NEXT 2 RECORDS
!             READ(NSUIS)
!             READ(NSUIS)
!             READ(NSUIS)
!
! LOOP ON THE RECORDS TO GO TO LAST
!             DO ITER=1,NENRE
!!
!             IF (TASSE) THEN
!!
!              CALL LIT
!     &        (EPAI,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
!              CALL LIT
!     &        (CONC,RB,IB,CB,NCOUCH,'R4',NSUIS,BISUIS,ISTAT)
!!
!              CALL LIT
!     &        (TEMP,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
!             ELSEIF (GIBSON) THEN
!!
!              CALL LIT (XB,RB,NPF,CB,NPOIN2,'I',NSUIS,BISUIS,ISTAT)
!!
!              CALL LIT
!     &          (IVIDE,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
!              CALL LIT
!     &        (EPAI,RB,IB,CB,(NPFMAX-1)*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
!             ENDIF
!!
!             CALL LIT(HDEP,RB,IB,CB,NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
!             CALL LIT(ZR,RB,IB,CB,NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
!             END DO
!!
!!     UPDATES THE BOTTOM ELEVATION (HDEP=0 POUR LE MODELE
!!
!!C$DC$
!!      *BUG* IN FOLLOWING LINE IN CALL TO OV() :
!!         ARGUMENT   CHARACTER*3 C
!!         INSTEAD OF DOUBLE PRECISION
!!
!!     CALL OV( 'X=Y+Z   ' , ZF  , ZR , HDEP , 0.D0, NPOIN2)
!      CALL OV( 'X=Y+Z   ' , ZF  , ZR , HDEP , 0.D0, NPOIN2)
!
!      IF (TASSE) THEN
!         DO IC = 1 , NCOUCH
!            DO IPOIN = 1 , NPOIN2
!               ZF(IPOIN) = ZF(IPOIN) + EPAI((IPOIN-1)*NCOUCH+IC)
!            END DO
!         END DO
!!
!      ELSEIF (GIBSON) THEN
!!
!         CALL ACTUZF(IVIDE , EPAI , ZF , NPOIN2, NPFMAX , NPF )
!      ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-CHANGES
!
!     INITIALISES FLUER (EROSION FLUX) AND
!     PDEPOT (PROBABILITY OF DEPOSITION)
!
      CALL OV( 'X=C     ', FLUER,  FLUER,  FLUER,  0.D0, NPOIN2)
      CALL OV( 'X=C     ', PDEPOT, PDEPOT, PDEPOT, 0.D0, NPOIN2)
!
      DEALLOCATE(RB)
!
      RETURN
      END
