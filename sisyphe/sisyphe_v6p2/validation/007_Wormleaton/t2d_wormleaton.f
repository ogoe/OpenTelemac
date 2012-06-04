
C                       ************************
                        SUBROUTINE FRICTION_USER
C                       ************************
C
C***********************************************************************
C  TELEMAC-2D VERSION 5.4                 J-M HERVOUET (LNH) 30 87 80 18
C***********************************************************************
C
C 15/04/04 : subroutine written by F. Huvelin
C
C***********************************************************************
C
C     FUNKTION  : DEFINITION VON RAUHEITSZONEN (FÜR JEDEN KNOTEN) ANHAND 
C                 EINER TABELLE (NFO2) (SURFINI BELEGT SCHON NFO1)
C
C***********************************************************************
C
          ! ----------------------------------------------- !
          !       Friction zones definition (by node)       !
          ! ----------------------------------------------- !
C
C
C               TTTTT EEEEE L     EEEEE M   M   AA  CCCCC
C                 T   E     L     E     MM MM  A  A C
C                 T   EEE   L     EEE   M M M  AAAA C
C                 T   E     L     E     M   M  A  A C
C                 T   EEEEE LLLLL EEEEE M   M  A  A CCCCC
C
C
C----------------------------------------------------------------------C
C                             ARGUMENTS                                C
C .________________.____.______________________________________________C
C |      NOM       |MODE|                   ROLE                       C
C |________________|____|______________________________________________C
C |                |    |                                              C
C |________________|____|______________________________________________C
C                    <=  input value                                   C
C                    =>  output value                                  C 
C ---------------------------------------------------------------------C

      ! Special for BAW
      ! ---------------
      ! In order to use the special BAW numbering
      ! A little computation have to be done :
      ! 
      ! 1/ Change BAW numbering
      ! For each node, cut zone code as :
      ! I1 = TYP (WALD, FLUSS, ...)      (3 digits)
      ! I2 = ORT (RECHTS, MITTEL, LINKS) (1 digit )
      ! I3 = km                          (5 digits)
      ! I = I3&I2&I1 becomes I = I1&I2&I3 for treatment
      !
      ! 2/ Find in wich zone or sub-zone(von..bis) is the node
      !
      ! 3/ Set the BAW numbering of the 1st sub-zone (GNUMB(1)) to the node

!=======================================================================!
!=======================================================================!
!                    DECLARATION DES TYPES ET DIMENSIONS                !
!=======================================================================!
!=======================================================================!

      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      IMPLICIT NONE      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      !1/ Global variables
      !-------------------

      !2/ Local variables
      !------------------
      INTEGER           :: I,K,IVAL1,IVAL2,NFILE
      INTEGER           :: NPOIN_GLOB,NPTFR_INT_GLOB
      INTEGER, EXTERNAL :: P_ISUM
      CHARACTER*144     :: NOMFILE

      ! Special BAW
      INTEGER :: J, I0, I1, I2, I3
      INTEGER :: ITEMP, IERR, IERR1, IERR2
      INTEGER :: ULOOP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IZ
      ! Special BAW

      ALLOCATE(IZ(NZONES,2))
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!      NFILE   = NFO1
!      NOMFILE = NOMFO1

      NFILE   = T2D_FILES(T2DFO1)%LU
      NOMFILE = T2D_FILES(T2DFO1)%NAME

      ! File Read in order to know the number of node in the mesh
      ! (this information is unknown for a parallel computation
      ! ---------------------------------------------------------
      IF (NCSIZE > 1) THEN
         NPOIN_GLOB = 0
         REWIND(NFILE)
         DO 
            READ(NFILE,*,END=666)
            NPOIN_GLOB = NPOIN_GLOB + 1
         ENDDO
      ELSE
         NPOIN_GLOB = NPOIN
      ENDIF
666   REWIND(NFILE)


      ! NEW ZONE NUMBERING
      ! ------------------
      IERR = 0
      DO I = 1, NZONES

         ! New zone numbering for each sub-zone
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         DO J = 1, 2
            IZ(I,J) = FRTAB%ADR(I)%P%GNUMB(J)
            I1      = MOD(IZ(I,J),1000)
            IZ(I,J) = IZ(I,J) - I1
            I2      = MOD(IZ(I,J),10000)
            I3      = IZ(I,J) - I2
            I1      = I1*1000000
            I2      = I2*100
            I3      = I3 / 10000
            IZ(I,J) = I1 + I2 + I3
         ENDDO

         IF (IZ(I,1) > IZ(I,2)) THEN
            IERR = 1
            CALL FLUSH(LU)
            WRITE(LU,001) I, FRTAB%ADR(I)%P%GNUMB(1),
     &                       FRTAB%ADR(I)%P%GNUMB(2)
         ENDIF
      ENDDO

      IF (IERR == 1) CALL PLANTE(0)


      ! NEW "NODE" NUMBERING
      ! --------------------
      ULOOP = 0
      DO I = 1, NPOIN_GLOB

         READ(NFILE,*,END=999,ERR=998) IVAL1, IVAL2

         IF (NCSIZE>1) THEN
            K = MESH%KNOGL%I(I)
         ELSE
            K = I
         ENDIF

         IF (K == 0) GOTO 777

         KFROPT%I(K) = IVAL2

         ! New zone numbering for each node
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         I0    = IVAL2
         I1    = MOD(IVAL2,1000)
         IVAL2 = IVAL2 - I1
         I2    = MOD(IVAL2,10000)
         I3    = IVAL2 - I2
         I1    = I1 * 1000000
         I2    = I2 * 100
         I3    = I3 / 10000
         IVAL2 = I1 + I2 + I3

         ! Search if the node is in a sub-zone
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ITEMP = 0
         IERR1 = 0
         IERR2 = 0

         DO J = 1, NZONES
            IF ((IZ(J,1) <= IVAL2).AND.(IVAL2 <= IZ(J,2))) THEN
                  IERR1 = IERR1 + 1
                  ITEMP = J
            ENDIF
         ENDDO

         DO J = 1, NZONES
            IF ((IZ(J,1) == IVAL2).AND.(IVAL2 == IZ(J,2))) THEN
                  IERR2 = IERR2 + 1
                  ITEMP = J
            ENDIF
         ENDDO

         ! Store the mode where a zone is not defined
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF ((IERR2 == 0).AND.(IERR1 == 0)) THEN
            ULOOP = ULOOP + 1
            IT1%I(ULOOP) = I

         ! Warning message for multi-zone definition
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ELSE IF (IERR2 > 1) THEN
            WRITE(LU,003) KFROPT%I(K), I, IERR2,
     &      FRTAB%ADR(ITEMP)%P%GNUMB(1), FRTAB%ADR(ITEMP)%P%GNUMB(2)

         ! Warning message for multi-zone definition
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ELSE IF ((IERR1 > 1).AND.(IERR2/=1)) THEN
            WRITE(LU,004) KFROPT%I(K), I, IERR1,
     &      FRTAB%ADR(ITEMP)%P%GNUMB(1), FRTAB%ADR(ITEMP)%P%GNUMB(2)
         ENDIF

         ! Set the value of the 1st sub-zone
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF (ITEMP > 0) KFROPT%I(K) = FRTAB%ADR(ITEMP)%P%GNUMB(1)

777      CONTINUE
      ENDDO


      ! STOP THE COMPUTATION IF NODE WITHOUT ZONE
      ! -----------------------------------------
      IF (ULOOP > 0) THEN
         DO I = 1, ULOOP   
            IF (NCSIZE>1) THEN
               K = MESH%KNOGL%I(IT1%I(I))
            ELSE
               K = I
            ENDIF
            CALL FLUSH(LU)
            WRITE(LU,002) KFROPT%I(K), IT1%I(I)
         ENDDO
         WRITE(LU,005)
         CALL PLANTE(0)
      ENDIF

      GOTO 997


      ! --------------------------------------------------------------- !
      !             ERROR MESSAGE DURING THE READING                    !
      ! --------------------------------------------------------------- !

001   FORMAT(/'DEFINITION ERROR FOR ZONE : ',i9,
     &       /'YOU MUST HAVE ',i9,' < ',i9)
002   FORMAT(/'=================================================',
     &       /'STOP IN TELEMAC-2D',
     &       /'ERROR FRICTION DEFINITION',
     &       /'ZONE : ',i9,' AND NODE : ',i9,' NOT DEFINED')
003   FORMAT(/'WARNING FRICTION DEFINITION',
     &       /'ZONE : ',i9,' AND NODE : ',i9,' IS DEFINED ',i9,
     &        ' TIMES AS SINGLE ZONE',
     &  /'THE ZONE : FROM ',i9,' TO ',i9,' IS KEPT FOR THE COMPUTATION')
004   FORMAT(/'WARNING FRICTION DEFINITION',
     &       /'THE SUB-ZONE : ',i9,' AND NODE : ',i9,' IS DEFINED IN ',
     &        i9,' ZONES'
     &  /'THE ZONE : FROM ',i9,' TO ',i9,' IS KEPT FOR THE COMPUTATION')
005   FORMAT('=================================================')

      ! --------------------------------------------------------------- !

999   CONTINUE
      WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
      WRITE(LU,*) 'ABNORMAL END OF FILE'
      CALL PLANTE(0)

998   CONTINUE
      WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
      WRITE(LU,*) 'READ ERROR'
      CALL PLANTE(0)

      ! --------------------------------------------------------------- !
      ! --------------------------------------------------------------- !

997   CONTINUE
!=======================================================================!
!=======================================================================!
      DEALLOCATE(IZ)
      RETURN
      END SUBROUTINE FRICTION_USER
!
!
