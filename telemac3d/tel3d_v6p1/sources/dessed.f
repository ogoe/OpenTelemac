C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PROVIDES GRAPHICAL OUTPUTS
!>                FOR THE VARIABLES DESCRIBING THE MUDDY BED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  ONLY WORKS WITH THE GIBSON MODEL

!>  @warning  ASSUMES THAT DTC IS IN FACT AT

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BIRSED, CONC, DTC, EPAI, GIBSON, GRADEB, GRAPRD, HDEP, IVIDE, LT, NCOUCH, NIT, NPF, NPFMAX, NPOIN2, NRSED, TASSE, TEMP, TITCAS, ZR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::MESH2D MESH2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RHOS RHOS@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CB, ECOUCH, ERR, I, IB, IBID1, IELEM, IKLES, IPLAN, IPOBO, IPOIN, ISTAT, JPLAN, NDP, NELEM2, NELEM3, NPLAN, NPOIN3, NPTFR2, TITSEL, UNITCONV, WSEB, XB, ZPLAN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ECRI2(), PLANTE()
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
!> </td><td> 6/05/93
!> </td><td>
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 12/06/92
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BIRSED
!></td><td>--></td><td>BINAIRE DU FICHIER DES RESULTATS SEDIMENTO
!>    </td></tr>
!>          <tr><td>CONC
!></td><td><--</td><td>CONCENTRATIONS DES COUCHES DE VASE
!>                  (TASSEMENT MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>DTC
!></td><td>--></td><td>PAS DE TEMPS POUR LA CONSOLIDATION
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><--</td><td>TAILLE DES MAILLES DU FOND VASEUX
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGIQUE POUR MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>GRADEB
!></td><td>--></td><td>PREMIER PAS DE TEMPS A PARTIR DUQUEL ON
!>                  ECRIT LES RESULTATS.
!>    </td></tr>
!>          <tr><td>GRAPRD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><--</td><td>HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><--</td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!>                  (MODELE DE TASSEMENT MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NOMBRE DE PAS DE TEMPS
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
!>          <tr><td>NRSED
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER RESULTAT
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TEMP
!></td><td><--</td><td>COMPTEUR DE TEMPS (TASSEMENT MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>TITCAS
!></td><td>--></td><td>TITRE DU CAS TEST
!>    </td></tr>
!>          <tr><td>ZR
!></td><td><--</td><td>COTE DU FOND RIGIDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DESSED
     & (NPF,IVIDE,EPAI,HDEP,CONC,TEMP,ZR,NPOIN2,NPFMAX,NCOUCH,
     &  NIT,GRAPRD,LT,DTC,TASSE,GIBSON,NRSED,TITCAS,BIRSED,GRADEB)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BIRSED         |-->| BINAIRE DU FICHIER DES RESULTATS SEDIMENTO
C| CONC           |<--| CONCENTRATIONS DES COUCHES DE VASE
C|                |   | (TASSEMENT MULTICOUCHES)
C| DTC            |-->| PAS DE TEMPS POUR LA CONSOLIDATION
C| EPAI           |<--| TAILLE DES MAILLES DU FOND VASEUX
C| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
C| GRADEB         |-->| PREMIER PAS DE TEMPS A PARTIR DUQUEL ON
C|                |   | ECRIT LES RESULTATS.
C| GRAPRD         |---| 
C| HDEP           |<--| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
C| IVIDE          |<--| INDICE DES VIDES AUX POINTS DU MAILLAGE
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
C|                |   | (MODELE DE TASSEMENT MULTICOUCHES)
C| NIT            |-->| NOMBRE DE PAS DE TEMPS
C| NPF            |<--| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON)
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NRSED          |-->| NUMERO D'UNITE LOGIQUE DU FICHIER RESULTAT
C| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
C| TEMP           |<--| COMPTEUR DE TEMPS (TASSEMENT MULTICOUCHES)
C| TITCAS         |-->| TITRE DU CAS TEST
C| ZR             |<--| COTE DU FOND RIGIDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, ONLY: NCSIZE,NPTIR
      USE DECLARATIONS_TELEMAC3D, ONLY: MESH2D,RHOS
!#####< SEB-changes
      IMPLICIT NONE
C#####> SEB-CHANGES
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER ERR, I, IPLAN,JPLAN, IPOIN, IELEM
      INTEGER NPLAN,NPOIN3,NELEM3,NELEM2,NPTFR2,NDP
      CHARACTER*80 TITSEL
      DOUBLE PRECISION UNITCONV, ECOUCH,ZPLAN
!
      INTEGER, ALLOCATABLE :: IPOBO(:),IKLES(:)       ! THESE WILL BE 3D
      DOUBLE PRECISION, ALLOCATABLE :: WSEB(:)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-changes
!
      INTEGER, INTENT(IN)          :: NPOIN2, NPFMAX, NRSED
      INTEGER, INTENT(IN)          :: LT, NIT , NCOUCH
      INTEGER, INTENT(IN)          :: GRAPRD, GRADEB
      INTEGER, INTENT(IN)          :: NPF(NPOIN2)
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      DOUBLE PRECISION, INTENT(IN) :: EPAI((NPFMAX-1)*NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: EPAI(NPFMAX-1,NPOIN2)
C      DOUBLE PRECISION, INTENT(IN) :: IVIDE(NPFMAX*NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HDEP(NPOIN2), ZR(NPOIN2)
C      DOUBLE PRECISION, INTENT(IN) :: CONC(NCOUCH), TEMP(NCOUCH*NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: CONC(NCOUCH), TEMP(NCOUCH,NPOIN2)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-changes
      DOUBLE PRECISION, INTENT(IN) :: DTC
      LOGICAL, INTENT(IN)          :: TASSE,GIBSON
      CHARACTER(LEN=72), INTENT(IN):: TITCAS
      CHARACTER(LEN=3), INTENT(IN) :: BIRSED
!
      DOUBLE PRECISION XB(2)
      INTEGER IB(10), ISTAT
      CHARACTER(LEN=2) CB
!
!----------------------------------------------------------------------
!
      IF((LT/GRAPRD)*GRAPRD.NE.LT) RETURN
!
      IF(LT.LT.GRADEB) RETURN
!
      IF(LT.EQ.0) THEN
!
      REWIND NRSED
!
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        NELEM2 = MESH2D%NELEM
        NPTFR2 = MESH2D%NPTFR
C       LEC/ECR 1: NAME OF GEOMETRY FILE
        TITSEL = TITCAS // 'SERAPHIN'
        CALL ECRI2(XB,IB,TITSEL,80,'CH',NRSED,BIRSED,ISTAT)
!
C       LEC/ECR 2: NUMBER OF 1 AND 2 DISCRETISATION FUNCTIONS
        IF(TASSE) THEN
          IB(1)=4
          IB(2)=0
        ELSEIF (GIBSON) THEN
          IB(1)=4
          IB(2)=0
        ELSE
          IF(LNG.EQ.1) WRITE(LU,*) "OPTION DE CONSOLIDATION NON PREVUE"
          IF(LNG.EQ.2) WRITE(LU,*) "UNKNOWN CONSOLIDATION OPTION"
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL ECRI2(XB,IB,CB,2,'I ',NRSED,BIRSED,ISTAT)
!
C   LEC/ECR 3: NAMES AND UNITS OF THE VARIABLES
      IF(TASSE) THEN
           TITSEL(1:32) = 'ELEVATION Z     M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'EPAISSEUR VRAIE M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'CONC. VASE      KG/M3           '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'COMPTEUR TEMPS  S               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
      ELSEIF(GIBSON) THEN
           TITSEL(1:32) = 'ELEVATION Z     M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'EPAISSEUR VRAIE M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'DENSITE VRAIE   KG/M3           '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'LAYER IPF                       '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
      ENDIF
!
C   LEC/ECR 4: LIST OF 10 INTEGER PARAMETERS (AND DATE)
        IB(1) = 1
        DO I = 2,10
          IB(I) = 0
        ENDDO
        IF (TASSE) THEN
           IB(7) = NCOUCH
        ELSEIF (GIBSON) THEN
           IB(7) = NPFMAX
        ENDIF
        NPLAN = IB(7)
        IF(NCSIZE.GT.1) THEN       ! CAN YOU MAKE SURE THESE ARE 3D
           IB(8) = NPTFR2*NPLAN    ! 3D -> TO BE CALCULATED FROM 2D
           IB(9) = NPTIR           ! CAN THIS ONLY BE 2D ?
        ENDIF
C        IF(DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3).NE.0) THEN
C           IB(10) = 1
C        ENDIF
        CALL ECRI2(XB,IB,CB,10,'I ',NRSED,BIRSED,ISTAT)
!
C   DATE
C       IF(IB(10).EQ.1) THEN
C          IB(1)=DATE(1)
C          IB(2)=DATE(2)
C          IB(3)=DATE(3)
C          IB(4)=TIME(1)
C          IB(5)=TIME(2)
C          IB(6)=TIME(3)
C          CALL ECRI2(XB,IB,CB,6,'I ',NRSED,BIRSED,ISTAT)
C       ENDIF
!
C   LEC/ECR 5: 4 INTEGERS
        IB(1) = NELEM2*(NPLAN-1)    ! 3D -> TO BE CALCULATED FROM 2D
        NELEM3 = IB(1)
        IB(2) = NPOIN2*NPLAN        ! 3D -> TO BE CALCULATED FROM 2D
        NPOIN3 = IB(2)
        IB(3) = 6                   ! PARTICULAR CASE OF PRISMS NDP=6
        NDP = IB(3)
        IB(4) = 1
        CALL ECRI2(XB,IB,CB,4,'I ',NRSED,BIRSED,ISTAT)
!
C   LEC/ECR 6: IKLE
C   BUILDS 3D LAYERED PRISMATIC MESH OUT OF 2D IMPRINT
        ALLOCATE(IKLES(NELEM3*NDP),STAT=ERR)  ! PARTICULAR CASE OF PRISMS
        IF(ERR.NE.0) STOP 'DESSED : ALLOCATION DE IKLES'
        DO IPLAN = 1,NPLAN-1
         DO IELEM = 1,NELEM2
          I = ((IPLAN-1)*NELEM2+IELEM-1)*NDP
          IKLES(I+1)=MESH2D%IKLE%I(IELEM)+(IPLAN-1)*NPOIN2
          IKLES(I+2)=MESH2D%IKLE%I(IELEM+NELEM2)+(IPLAN-1)*NPOIN2
          IKLES(I+3)=MESH2D%IKLE%I(IELEM+2*NELEM2)+(IPLAN-1)*NPOIN2
          IKLES(I+4)=MESH2D%IKLE%I(IELEM)+IPLAN*NPOIN2
          IKLES(I+5)=MESH2D%IKLE%I(IELEM+NELEM2)+IPLAN*NPOIN2
          IKLES(I+6)=MESH2D%IKLE%I(IELEM+2*NELEM2)+IPLAN*NPOIN2
         ENDDO
        ENDDO
        CALL ECRI2(XB,IKLES,CB,NELEM3*NDP,'I ',NRSED,BIRSED,ISTAT)
        DEALLOCATE(IKLES)
C
C   LEC/ECR 7: IPOBO (CASE OF FILES WITHOUT PARALLELISM)
C
        IF( IB(8).EQ.0.AND.IB(9).EQ.0 ) THEN
           ALLOCATE(IPOBO(NPLAN*NPOIN2),STAT=ERR)
           IF(ERR.NE.0) STOP 'DESSED : ALLOCATION DE IPOBO'
           DO IPOIN = 1,NPLAN*NPOIN2            ! THIS IS INDEED 3D
             IPOBO(IPOIN) = 0
           ENDDO
           DO IPLAN = 1,NPLAN
             DO IPOIN = 1,NPTFR2
               IPOBO(MESH2D%NBOR%I(IPOIN)+(IPLAN-1)*NPOIN2) =
     &         IPOIN+(IPLAN-1)*NPTFR2
             ENDDO
           ENDDO
           CALL ECRI2(XB,IPOBO,CB,NPLAN*NPOIN2,'I ',NRSED,BIRSED,ISTAT)
           DEALLOCATE(IPOBO)
        ENDIF
C   LEC/ECR 7.1: KNOLG (ONLY IN THE EVENT OF PARALLEL MODE)
        IF(IB(8).NE.0.OR.IB(9).NE.0) THEN
           ALLOCATE(IPOBO(NPLAN*NPOIN2),STAT=ERR)
           IF(ERR.NE.0) STOP 'DESSED : ALLOCATION DE IPOBO'
           DO IPOIN = 1,NPLAN*NPOIN2            ! THIS IS INDEED 3D
             IPOBO(IPOIN) = 0
           ENDDO
           DO IPLAN = 1,NPLAN
              DO IPOIN = 1,NPOIN2
                 IPOBO(IPOIN+(IPLAN-1)*NPOIN2) =
     &              MESH2D%KNOLG%I(IPOIN)+(IPLAN-1)*NPOIN2
              ENDDO
           ENDDO
           CALL ECRI2(XB,IPOBO,CB,NPOIN3,'I ',NRSED,BIRSED,ISTAT)
        ENDIF
C
C   LEC/ECR 8 AND 9: X AND Y COORDINATES OF THE MESH NODES
C
        ALLOCATE(WSEB(NPLAN*NPOIN2),STAT=ERR)
        IF(ERR.NE.0) THEN
           IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
           IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
           STOP
        ENDIF
        DO IPOIN = 1, NPOIN2
          DO IPLAN = 1,NPLAN
             WSEB(IPOIN+(IPLAN-1)*NPOIN2) = MESH2D%X%R(IPOIN)
          ENDDO
        ENDDO
        CALL ECRI2(WSEB,IB,CB,NPOIN3,'R4',NRSED,BIRSED,ISTAT)
        DO IPOIN = 1, NPOIN2
          DO IPLAN = 1,NPLAN
            WSEB(IPOIN+(IPLAN-1)*NPOIN2) = MESH2D%Y%R(IPOIN)
          ENDDO
        ENDDO
        CALL ECRI2(WSEB,IB,CB,NPOIN3,'R4',NRSED,BIRSED,ISTAT)
        DEALLOCATE(WSEB)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-changes
      ENDIF
!
C A TRICK TO WRITE ONE NUMBER
!
      XB(1) = DTC
      CALL ECRI2(XB,IB,CB,1,'R4',NRSED,BIRSED,ISTAT)
!
      IF (TASSE) THEN
!
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  /!\ THIS PART SHOULD BE ENTIRELY REVISITED ...
         UNITCONV = 1.D0                     ! VARIABLES CAN BE ENLARGED
         ALLOCATE(WSEB(NCOUCH*NPOIN2),STAT=ERR)
         IF(ERR.NE.0) THEN
            IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
            IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
            STOP
         ENDIF
C THIS IS THE Z FOR THE LAYERING -
         DO IPOIN = 1, NPOIN2
C>            WSEB(IPOIN) = ZR(IPOIN) + EPAI(IPOIN)
         ENDDO
         DO IPLAN = 2,NCOUCH
            DO IPOIN = 1, NPOIN2
C>               WSEB(IPOIN+NPOIN2*(IPLAN-1)) =
C>     & WSEB(IPOIN+NPOIN2*(IPLAN-2)) + EPAI(IPOIN+NPOIN2*(IPLAN-1))
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!
         DO IPOIN = 1, (NCOUCH-1)*NPOIN2
C>            WSEB(IPOIN) = EPAI(IPOIN+NPOIN2) * UNITCONV
         ENDDO
C        DO IPOIN = 1, NPOIN2
C            WSEB(IPOIN+(NCOUCH-1)*NPOIN2) = HDEP(IPOIN) * UNITCONV
C        ENDDO
         CALL ECRI2(WSEB,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C        CALL ECRI2(EPAI,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C!
         DO IPLAN = 1,NCOUCH
            DO IPOIN = 1, NPOIN2
C>               WSEB(IPOIN+NPOIN2*(IPLAN-1)) = CONC(IPLAN) * UNITCONV
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C        CALL ECRI2(CONC,IB,CB,NCOUCH,'R4',NRSED,BIRSED,ISTAT)
!#####< SEB-changes
!
        CALL ECRI2(TEMP,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C#####> SEB-CHANGES
         DEALLOCATE(WSEB)
!#####< SEB-changes
!
      ELSEIF (GIBSON) THEN
!
C#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
C ASSUMPTIONS - Z-LEVELS:
C  * B KENUE'S BOTTOM Z-LEVEL IS ZR (1), TOP Z-LEVEL IS ZF (NPFMAX)
C  * SEDI3D'S NON-EMPTY LAYERS ARE B KENUE'S LAYERS UNDER ZF
C  * B KENUE'S PLANES FROM 1 TO NPFMAX-NPF ARE EMPTY (EPAI=0) AND
C      CORRESPOND TO SEDI3D'S PLANES FROM NPF+2 TO NPFMAX
C  * ALL EMPTY PLANES (EXCEPT FOR HDEP) ARE SET TO COINCIDENT WITH ZR
C      (ROCK BOTTOM), WHILE SEDI3D'S 1ST PLANE IS ZR
C  * B KENUE'S NON-EMPTY TOP PLANES FROM NPFMAX-NPF+1 TO NPFMAX-1
C      CORRESPOND TO SEDI3D'S PLANES FROM 2 TO NPF IN THE SAME ORDER
C  * B KENUE'S VERY TOP PLANE AT NPFMAX CORRESPONDS TO
C      SEDI3D'S NPF+1-TH PLANE, WHICH IS ALSO HDEP - EVEN IF EMPTY !
!
C ASSUMPTIONS - VARIABLE THICKNESS:
C  * B KENUE'S THICKNESS BETWEEN TWO PLANES IS STORED ON THE UPPER PLANE
C      WHICH IS CONTRARY TO SEDI3D'S CONVENTION
C  * B KENUE'S NPFMAX-TH THICKNESS STORES HDEP
C  * FOR STORAGE PURPOSES, B KENUE'S 1ST PLANE HOLDS THE NPF
!
         UNITCONV = 1.D0                     ! VARIABLES CAN BE ENLARGED
         ALLOCATE(WSEB(NPFMAX*NPOIN2),STAT=ERR)
         IF(ERR.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
          IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
          STOP
         ENDIF
C> TRUE LAYERING - ELEVATION Z
         DO IPOIN = 1, NPOIN2
            JPLAN = 0
            ZPLAN = ZR(IPOIN)
            DO IPLAN = 1,NPFMAX-NPF(IPOIN)
               JPLAN = JPLAN + 1
               WSEB(IPOIN+(JPLAN-1)*NPOIN2) = ZPLAN
            ENDDO
            DO IPLAN = 1,NPF(IPOIN)-1
               JPLAN = JPLAN + 1
               ECOUCH=(IVIDE(IPLAN,IPOIN)+IVIDE(IPLAN+1,IPOIN))/2.D0
               ZPLAN = ZPLAN +
     &               ( 1.D0+ECOUCH ) * EPAI(IPLAN,IPOIN)
               WSEB(IPOIN+(JPLAN-1)*NPOIN2) = ZPLAN
            ENDDO
            WSEB(IPOIN+(NPFMAX-1)*NPOIN2) = ZPLAN +
     &               HDEP(IPOIN)
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C> TRUE THICKNESS - THICKNESS DZ
         DO IPOIN = 1, NPOIN2
            JPLAN = 0
            DO IPLAN = 1,NPFMAX-NPF(IPOIN)
               JPLAN = JPLAN + 1
               WSEB(IPOIN+(JPLAN-1)*NPOIN2) = 0.D0
            ENDDO
            DO IPLAN = 1,NPF(IPOIN)-1
              JPLAN = JPLAN + 1
              ECOUCH=(IVIDE(IPLAN,IPOIN)+IVIDE(IPLAN+1,IPOIN))/2.D0
              WSEB(IPOIN+(JPLAN-1)*NPOIN2) =
     &            (1.D0+ECOUCH)*EPAI(IPLAN,IPOIN)*UNITCONV
            ENDDO
            WSEB(IPOIN+(NPFMAX-1)*NPOIN2) = HDEP(IPOIN) *UNITCONV
            WSEB(IPOIN) = 1.D0 * NPF(IPOIN) ! RESET THIS ONE ! OR NOT ?
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C> TRUE DENSITY
         DO IPOIN = 1, NPOIN2
            JPLAN = NPFMAX
            DO IPLAN = NPF(IPOIN),1,-1
          WSEB(IPOIN+(JPLAN-1)*NPOIN2)=RHOS/(1.D0+IVIDE(IPLAN,IPOIN))
              JPLAN = JPLAN - 1
            ENDDO
            DO IPLAN = NPFMAX,NPF(IPOIN)+1,-1
              WSEB(IPOIN+(JPLAN-1)*NPOIN2) = 0.D0
              JPLAN = JPLAN - 1
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C> LAYERING - LAYER IPF
         DO IPOIN = 1, NPOIN2
            DO IPLAN = 1,NPFMAX-1
               WSEB(IPOIN+(IPLAN-1)*NPOIN2) = IPLAN
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C        CALL ECRI2(IVIDE,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
C        CALL ECRI2(XB,NPF,CB,NPOIN2,'I',NRSED,BIRSED,ISTAT)
C        CALL ECRI2(EPAI,IB,CB,(NPFMAX-1)*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
         DEALLOCATE(WSEB)
!#####< SEB-changes
!
      ENDIF
!
C      CALL ECRI2(HDEP,IB,CB,NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!
C      CALL ECRI2(ZR,IB,CB,NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!
!----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
