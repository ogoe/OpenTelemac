C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS SEDIMENTOLOGICAL DATA
!>                FOR A RESUMING COMPUTATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IMPLEMENTATION WITH SELAFIN ONLY WORKS WITH GIBSON

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BISUIS, CONC, EPAI, FLUER, GIBSON, HDEP, IVIDE, NCOUCH, NPF, NPFMAX, NPOIN2, NPOIN3, NSUIS, PDEPOT, TASSE, TEMP, ZF, ZR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::RHOS RHOS@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CB, ECOUCH, ERR, I, IB, IC, ICOMPT, IENRE, IPLAN, IPOIN, ISTAT, ITER, JPLAN, N, NCOMPT, NENRE, NVAR, RB, UNITCONV, XB, ZPLAN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LIT(), OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.7                                       </center>
!> </td><td> 27/03/06
!> </td><td> S.E.BOURBAN AND N.DURAND (NRC-CHC)
!> </td><td> SELAFIN IMPLEMENTATION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 05/05/93
!> </td><td> C LE NORMANT (LNH) 30 87 83 53
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BISUIS
!></td><td>--></td><td>BINAIRE DU FICHIER DU CALCUL
!>                  SEDIMENTOLOGIQUE PRECEDENT.
!>    </td></tr>
!>          <tr><td>CONC
!></td><td><--</td><td>CONCENTRATIONS DES COUCHES DE VASE
!>                  (MODELE MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><--</td><td>TAILLE DES MAILLES DU FOND
!>                  EN COORDONNEES MATERIELLES (=DZ/(1+IVIDE))
!>                  AVEC LE MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td><--</td><td>FLUX DE DEPOT EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGIQUE POUR MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><--</td><td>HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><--</td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!>                  (MODELE DE TASSEMENT MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>NPF
!></td><td><--</td><td>NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NSUIS
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER DU CALCUL
!>                  SEDIMENTOLOGIQUE PRECEDENT.
!>    </td></tr>
!>          <tr><td>PDEPOT
!></td><td><--</td><td>PROBABILITE DE DEPOT EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TEMP
!></td><td><--</td><td>COMPTEUR DE TEMPS POUR LE TASSEMENT
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><--</td><td>COTE DU FOND VASEUX
!>    </td></tr>
!>          <tr><td>ZR
!></td><td><--</td><td>COTE DU FOND RIGIDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SUISED
     &(IVIDE,EPAI,HDEP,CONC,TEMP,FLUER,PDEPOT,ZR,ZF,NPF,
     & NPOIN2,NPOIN3,NPFMAX,NCOUCH,TASSE,GIBSON,NSUIS,BISUIS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BISUIS         |-->| BINAIRE DU FICHIER DU CALCUL
C|                |   | SEDIMENTOLOGIQUE PRECEDENT.
C| CONC           |<--| CONCENTRATIONS DES COUCHES DE VASE
C|                |   | (MODELE MULTICOUCHES)
C| EPAI           |<--| TAILLE DES MAILLES DU FOND
C|                |   | EN COORDONNEES MATERIELLES (=DZ/(1+IVIDE))
C|                |   | AVEC LE MODELE DE GIBSON
C| FLUER          |<--| FLUX DE DEPOT EN CHAQUE POINT 2D
C| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
C| HDEP           |<--| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
C| IVIDE          |<--| INDICE DES VIDES AUX POINTS DU MAILLAGE
C| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
C|                |   | (MODELE DE TASSEMENT MULTICOUCHES)
C| NPF            |<--| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON)
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NSUIS          |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DU CALCUL
C|                |   | SEDIMENTOLOGIQUE PRECEDENT.
C| PDEPOT         |<--| PROBABILITE DE DEPOT EN CHAQUE POINT 2D
C| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
C| TEMP           |<--| COMPTEUR DE TEMPS POUR LE TASSEMENT
C| ZF             |<--| COTE DU FOND VASEUX
C| ZR             |<--| COTE DU FOND RIGIDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      USE DECLARATIONS_TELEMAC3D, ONLY: RHOS
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< SEB-CHANGES
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)           :: NPOIN2, NPOIN3, NPFMAX, NCOUCH
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      DOUBLE PRECISION, INTENT(OUT) :: IVIDE(NPFMAX*NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: IVIDE(NPFMAX,NPOIN2)
C      DOUBLE PRECISION, INTENT(OUT) :: EPAI((NPFMAX-1)*NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT) :: CONC(NCOUCH)
C      DOUBLE PRECISION, INTENT(OUT) :: TEMP(NCOUCH*NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< SEB-CHANGES
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
      INTEGER IENRE  , ITER   , NENRE  , NCOMPT , ICOMPT , ISTAT
!
      INTEGER IB(10), N, ERR
      DOUBLE PRECISION, ALLOCATABLE :: XB(:)
      REAL, ALLOCATABLE :: RB(:)
      CHARACTER(LEN=80) CB
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      INTEGER NVAR, I, IPLAN,JPLAN
      DOUBLE PRECISION UNITCONV, ECOUCH,ZPLAN
C READS THE HEADER AND CHECKS QUANTITIES / OPTIONS AND ALLOCATES MEMORY
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*)
     &   'LECTURE DU FICHIER DE SEDIMENT PRECEDENT'
      IF(LNG.EQ.2) WRITE(LU,*)
     &   'READING PREVIOUS COMPUTATION FILE FOR SEDIMENT'
      WRITE(LU,*) ' '
!
C   LEC/ECR 1: NAME OF THE GEOMETRY FILE
      CALL LIT(XB,RB,IB,CB,80,'CH',NSUIS,BISUIS,ISTAT)
      IF(LNG.EQ.1) WRITE(LU,*) 'TITRE : ',CB(1:72)
      IF(LNG.EQ.2) WRITE(LU,*) 'TITTLE: ',CB(1:72)
!
C   LEC/ECR 2: NUMBER OF 1 AND 2 DISCRETISATION FUNCTIONS
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
C   LEC/ECR 3: VARIABLES NAMES AND UNITS
      DO I = 1,NVAR
         CALL LIT(XB,RB,IB,CB,32,'CH',NSUIS,BISUIS,ISTAT)
         IF(LNG.EQ.1) WRITE(LU,*) I,".- ",CB(1:16)
         IF(LNG.EQ.2) WRITE(LU,*) I,".- ",CB(1:16)
      ENDDO
!
C   LEC/ECR 4: LIST OF 10 INTEGER PARAMETERS (AND DATE)
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
C   LEC/ECR 5: 4 INTEGERS
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
C   LEC/ECR 6: IKLE
      READ(NSUIS)
!
C   LEC/ECR 7: IPOBO (NOT IN PARALLEL MODE)
      READ(NSUIS)
!
C   LEC/ECR 8 AND 9: X AND Y COORDINATES OF THE GRID NODES
      READ(NSUIS)
      READ(NSUIS)
!
C#####< SEB-CHANGES
!
!-----------------------------------------------------------------------
C ALLOCATES A (SIMPLE) REAL VECTOR, LENGTH N
!
C#####> SEB-CHANGES
      IF (TASSE) THEN
         ALLOCATE(XB(NCOUCH*NPOIN2),STAT=ERR)
         ALLOCATE(RB(NCOUCH*NPOIN2),STAT=ERR)
      ELSEIF (GIBSON) THEN
         ALLOCATE(XB(NPFMAX*NPOIN2),STAT=ERR)
         ALLOCATE(RB(NPFMAX*NPOIN2),STAT=ERR)
      ENDIF
C      N = MAX(NPFMAX*NPOIN2, NPOIN3)
C      ALLOCATE(RB(N),STAT=ERR)
C#####< SEB-CHANGES
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
C#####> SEB-CHANGES
C CASE OF SELAFIN 3D FORMAT - THE HEADER HAS ALREADY BEEN READ
      NENRE = 0
      DO
         READ(NSUIS,END=1901)          ! AT
         DO I = 1,NVAR
            READ(NSUIS)
         ENDDO
         NENRE = NENRE + 1
      ENDDO
 1901 CONTINUE
C                          > MOVE TO BEFORE-LAST RECORD
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
C                          > READS LAST RECORD
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*)
     &   'LECTURE DU DERNIER DES ENREGISTREMENTS PRECEDENTS'
      IF(LNG.EQ.2) WRITE(LU,*)
     &   'READING THE LAST OF THE PREVIOUS RECORDS'
      WRITE(LU,*) ' '
      READ(NSUIS)             ! AT
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  /!\ THIS PART SHOULD BE ENTIRELY REVISITED ...
      IF (TASSE) THEN
C ELEVATION Z (MOSTLY FOR PLOTTING PURPOSES)
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,NPOIN2
C>            EPAI(IPOIN) = DBLE( RB(IPOIN) ) - ZR(IPOIN)
C            ZF(IPOIN) = DBLE( RB(IPOIN+(NCOUCH-1)*NPOIN2) )
         ENDDO
C THICKNESS
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,(NCOUCH-1)*NPOIN2
C>            EPAI(IPOIN+NPOIN2) = DBLE( RB(IPOIN) ) * UNITCONV
         ENDDO
         DO IPOIN = 1,NPOIN2
            HDEP(IPOIN) = DBLE( RB(IPOIN+(NCOUCH-1)*NPOIN2) )*UNITCONV
         ENDDO
C CONCENTRATION
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPLAN = 1,NCOUCH
            CONC(IPLAN) = DBLE( RB(1+(IPLAN-1)*NPOIN2) ) * UNITCONV
         ENDDO
C TIME OF CONSOLIDATION
         CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DO IPOIN = 1,NCOUCH*NPOIN2
C>            TEMP(IPOIN) = DBLE( RB(IPOIN) )
         ENDDO
         DEALLOCATE(XB)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ELSEIF (GIBSON) THEN
C> ELEVATION Z (TEMPORARILY STORES TRUE LAYER THICKNESS)
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
C            HDEB(IPOIN) = ZF(IPOIN) - ZPLAN
         ENDDO
C> TRUE THICKNESS AND NUMBER OF LAYERS
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
C> TRUE DENSITY AND RENUMBERING
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
C> VIRTUAL THICKNESS EPAI AND RENUMBERING
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
C> LAYER IPF: ONLY USEFUL FOR PLOTTING PURPOSES
C         CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
         DEALLOCATE(XB)
      ENDIF
C#####< SEB-CHANGES
C#####> SEB-CHANGES
!!-----------------------------------------------------------------------
!!
C! COUNTS THE NUMBER OF RECORDS TO GO TO LAST
C             REWIND NSUIS
C
C! READS THE FIRST 3 RECORDS
C             DO IENRE = 1,3
C                READ(NSUIS,ERR=50)
C             END DO
!!
C! READS THE RECORDS CORRESPONDING TO 1 TIMESTEP
C             NENRE  = 0
C! THERE ARE ...  RECORDS PER TIMESTEP
C             NCOMPT = 2
C             IF ( (TASSE).OR.(GIBSON) ) NCOMPT = 5
C30           CONTINUE
C             DO ICOMPT = 1,NCOMPT
C                READ(NSUIS,ERR=50,END=60)
C             END DO
C             NENRE = NENRE + 1
C             GOTO 30
C50           CONTINUE
C             IF (LNG==1) THEN
C               WRITE(LU,*) 'SOUS-PROG SUISED'
C               WRITE(LU,*)
C     &          'ERREUR LECTURE DU FICHIER SUITE SEDIMENTOLOGIQUE'
C             ELSE
C               WRITE(LU,*) 'SUBROUTINE SUISED'
C               WRITE(LU,*)
C     &          'ERROR READING THE PREV. COMPUTATION FILE FOR SEDIMENT'
C             ENDIF
C             CALL PLANTE(1)
C             STOP
C60           CONTINUE
!!
C             REWIND NSUIS
!!
C! SKIPS THE TITLE AND NEXT 2 RECORDS
C             READ(NSUIS)
C             READ(NSUIS)
C             READ(NSUIS)
!
C LOOP ON THE RECORDS TO GO TO LAST
C             DO ITER=1,NENRE
!!
C             IF (TASSE) THEN
!!
C              CALL LIT
C     &        (EPAI,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
C              CALL LIT
C     &        (CONC,RB,IB,CB,NCOUCH,'R4',NSUIS,BISUIS,ISTAT)
!!
C              CALL LIT
C     &        (TEMP,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
C             ELSEIF (GIBSON) THEN
!!
C              CALL LIT (XB,RB,NPF,CB,NPOIN2,'I',NSUIS,BISUIS,ISTAT)
!!
C              CALL LIT
C     &          (IVIDE,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
C              CALL LIT
C     &        (EPAI,RB,IB,CB,(NPFMAX-1)*NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
C             ENDIF
!!
C             CALL LIT(HDEP,RB,IB,CB,NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
C             CALL LIT(ZR,RB,IB,CB,NPOIN2,'R4',NSUIS,BISUIS,ISTAT)
!!
C             END DO
!!
C!     UPDATES THE BOTTOM ELEVATION (HDEP=0 POUR LE MODELE
!!
C!C$DC$
C!      *BUG* IN FOLLOWING LINE IN CALL TO OV() :
C!         ARGUMENT   CHARACTER*3 C
C!         INSTEAD OF DOUBLE PRECISION
!!
C!     CALL OV( 'X=Y+Z   ' , ZF  , ZR , HDEP , 0.D0, NPOIN2)
C      CALL OV( 'X=Y+Z   ' , ZF  , ZR , HDEP , 0.D0, NPOIN2)
!
C      IF (TASSE) THEN
C         DO IC = 1 , NCOUCH
C            DO IPOIN = 1 , NPOIN2
C               ZF(IPOIN) = ZF(IPOIN) + EPAI((IPOIN-1)*NCOUCH+IC)
C            END DO
C         END DO
!!
C      ELSEIF (GIBSON) THEN
!!
C         CALL ACTUZF(IVIDE , EPAI , ZF , NPOIN2, NPFMAX , NPF )
C      ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< SEB-CHANGES
!
C     INITIALISES FLUER (EROSION FLUX) AND
C     PDEPOT (PROBABILITY OF DEPOSITION)
!
      CALL OV( 'X=C     ', FLUER,  FLUER,  FLUER,  0.D0, NPOIN2)
      CALL OV( 'X=C     ', PDEPOT, PDEPOT, PDEPOT, 0.D0, NPOIN2)
!
      DEALLOCATE(RB)
!
      RETURN
      END
C
C#######################################################################
C