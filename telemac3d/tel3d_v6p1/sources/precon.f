
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE ADVECTION STEP BY COMPUTING THE
!>                PARAMETERS COMMON TO ALL THE VARIABLES TO ADVECT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 26/04/2010
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18; JM JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/02/2010
!> </td><td> JMH
!> </td><td> COMPUTES ZCHAR TO CALL CHARAC
!>          (ALLOWS SIMPLIFICATION OF CHAR41 AND STREAMLINE.F)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 18/08/2009
!> </td><td> JMH
!> </td><td> UCONVC AND VCONVC FOR ADVECTION FIELD
!>           GIVEN TO SUPG AND CHARACTERISTICS (DONE IN WAVE_EQUATION)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 29/06/2009
!> </td><td> JMH
!> </td><td> POINT TO POINT FLUXES COMPUTED IN FLODEL
!>           FINITE VOLUMES ADVECTION SCHEMES OR 9 ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 13/08/2008
!> </td><td> JMH
!> </td><td> IMMEDIATE INTERPOLATION IN CHARAC
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 07/08/2008
!> </td><td> JMH
!> </td><td> CALLS CHARAC INSTEAD OF CARACT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE PRECON
     &(WP,WPS,ZPROPS,ISOUSI,LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AM1,AM2        |<->| MATRICES DE TRAVAIL
C| CONV           |-->| TABLEAU DE LOGIQUES INDIQUANT POUR CHAQUE
C|                |   | SCHEMA DE CONVECTION SI AU MOINS UNE
C|                |   | VARIABLE EST TRAITEE PAR CE SCHEMA
C| DT             |-->| PAS DE TEMPS
C| ELT            |<--| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| ETA            |<--| NUMEROS DES ETAGES AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| FLUEXT         |<--| FLUX EXTERIEUR PAR NOEUD
C| FLUINT         |<--| FLUX INTERIEUR PAR NOEUD
C| IBOR           |-->| TABLEAU DES ELEMENTS ADJACENTS AUX FACES(3D)
C| IELM2H         |-->| TYPE DE DISCRETISATION 2DH
C| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IKLE2          |-->| CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
C| INFO           |-->| INFORMATIONS SUR LES SOLVEURS
C| ISOUSI         |-->| RANG DE LA SOUS-ITERATION EN COURS
C| ITRAV3         |<->| STRUCTURE DE TABLEAUX DE TRAVAIL D'ENTIERS
C| LIWBOF,L,S     |-->| TYPE DE CONDITIONS LIMITES POUR WS
C| LT             |---| 
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASK           |<--| MASQUES POUR LES SEGMENTS 2D
C|                |   | MASK(NPTFR,1): 1. SI KDIR SUR U 0. SINON
C|                |   | MASK(NPTFR,2): 1. SI KDIR SUR V 0. SINON
C|                |   | MASK(NPTFR,3): 1. SI KDDL SUR U 0. SINON
C|                |   | MASK(NPTFR,4): 1. SI KDDL SUR V 0. SINON
C|                |   | MASK(NPTFR,5): 1. SI KNEU SUR U 0. SINON
C|                |   | MASK(NPTFR,6): 1. SI KNEU SUR V 0. SINON
C|                |   | MASK(NPTFR,7): 1. SI KOND 0. SINON
C|                |   | (KOND N'EST PAS DEFINI DANS TELEMAC-3D,
C|                |   | CAR IL N Y A PAS D'ONDE INCIDENTE. EN
C|                |   | CONSEQUENCE, MASK(*,7)=0)
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MASKPT         |-->| MASQUAGE DES POINTS
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| MURD           |-->| MATRICE MURD NON SYMETRIQUE
C| NBOR           |-->| ADRESSES GLOBALES DES POINTS FRONTIERES.
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPLINT         |-->| NUMERO DU PLAN INTERMEDIAIRE DE REFERENCE
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
C| NSOUSI         |-->| NOMBRE TOTAL DE SOUS-ITERATIONS
C| NULONE         |-->| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C| SHP            |<--| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHZ            |<--| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
C|                |   | DES COURBES CARACTERISTIQUES.
C| SM             |<->| SECOND MEMBRE POUR LA VITESSE VERTICALE
C| SUPG           |-->| MATRICE SUPG NON SYMETRIQUE
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| SVIDE          |-->| STRUCTURE VIDE
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C| TE1            |<->| TABLEAU DE TRAVAIL PAR ELEMENT 2D
C| TETAU          |-->| TAUX D'IMPLICITATION SUR U ET V
C| TRAV2          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 2D
C| TRAV3          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 3D
C| UCONV,VCONV    |<--| COMPOSANTES HORIZONTALES DU CHAMP CONVECTEUR
C| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
C| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
C| WP             |---| 
C| WPS            |-->| VITESSE W DANS LE MAILLAGE TRANSFORME
C| WSCON2         |<--| VITESSE VERTICALE MOYENNEE PAR ETAGE
C| WSCONV         |<--| VITESSE VERTICALE AUX NOEUDS 3D
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C| ZPROP          |-->| COORDONNEE VERTICALE A L'ETAPE DE CONTINUITE
C| ZPROPS         |---| 
C| ZSTAR          |<--| HAUTEURS RELATIVES DES PLANS HORIZONTAUX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PRECON => PRECON
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WP,WPS,ZPROPS
C
      INTEGER, INTENT(IN) :: ISOUSI,LT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ::I,IS,IP,OPTHNEG,IWS,NSEG3D,IPLAN,OPT_TRID
      CHARACTER(LEN=16) FORMUL
      CHARACTER(LEN=8) OPER
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!=======================================================================
!
C     MESH MODIFIED TO BE EQUIVALENT TO THE DEPTH USED IN THE 2D
C     CONTINUITY EQUATION, TO CALL : FLUX3D, VECTOR, MATRIX
!
C     ZPROPS IS TEMPORARILY PUT IN MESH3D%Z
      SAVEZ=>MESH3D%Z%R
      MESH3D%Z%R=>ZPROPS%R
      NSEG3D=MESH3D%NSEG
!
!=======================================================================
!
C COMPUTES INTERNAL AND EXTERNAL FLUXES AND ADVECTION FIELDS
!
!=======================================================================
!
      OPTHNEG=OPT_HNEG
      IF(LT.EQ.0) OPTHNEG=0
!
      CALL FLUX3D
     & (FLUINT,FLUEXT,FLUEXTPAR,UCONV,VCONV,T3_01,T3_02,T3_03,MESH3D%W,
     &  NETAGE,NPLAN,NELEM3,IELM3,IELM2H,IELM2V,SVIDE,MESH3D,
     &  MASK%ADR(8)%P,MSK,MASKEL,MASKBR,
     &  LIMPRO%I,KDIR,NPTFR2,DT,VOLU,VOLUN,MESH2D,
     &  GRAPRD,SIGMAG,T2_01,NPOIN2,NPOIN3,DM1,ZCONV,FLBOR,
     &  PLUIE,RAIN,FLODEL,FLOPAR,OPTHNEG,FLULIM,
     &  (N_ADV(ADV_LPO).GT.0.OR.N_ADV(ADV_LPO_TF).GT.0),
     &  LT,BYPASS,N_ADV,MTRA1)
!
!=======================================================================
C   COMPUTES (DZW*)JH,IV+1/2 AND ACCUMULATES IN WSCONV
!=======================================================================
!
!     HARDCODED OPTION !!!!!!!!!!!!
!
!     2: DIVERGENCE-FREE FLUXES OBTAINED BY MODIFYING VERTICAL FLUXES
!
!     3: DIVERGENCE-FREE FLUXES OBTAINED BY MODIFYING ALL FLUXES
!        WITH THE HELP OF WCONV AND A PRESSURE EQUATION
!
      OPT_TRID=2
!
      IF(OPT_TRID.EQ.2) THEN
        CALL TRIDW2(WSCONV)
      ELSEIF(OPT_TRID.EQ.3) THEN 
!       OTHERWISE WCONV DONE IN WAVE_EQUATION
        IF(LT.EQ.0) CALL OS('X=Y     ',X=WCONV,Y=W) 
        CALL TRIDW3(WSCONV,T3_01,T3_02,T3_03,T3_04,T3_05,MTRA1%D,LT)
      ENDIF
!
!=======================================================================
!     FOR DEBUGGING: SUMMARY OF ADVECTED VARIABLES AND THEIR SCHEME
!=======================================================================
!
!     DO I=1,15
!       IF(N_ADV(I).GT.0) THEN
!         DO IS=1,N_ADV(I)
!           WRITE(LU,*) 'ADVECTION OF ',
!    &                  BL_FN%ADR(LIST_ADV(IS,I))%P%NAME,
!    &                  ' BY SCHEME ',I
!         ENDDO
!       ENDIF
!     ENDDO
!
!=======================================================================
!     PREPARES ADVECTION BY MURD METHOD
!     STORAGE IS ALWAYS EBE
!=======================================================================
!
      IF(N_ADV(ADV_NSC).GT.0.OR.N_ADV(ADV_PSI).GT.0) THEN
!
C       NOTE: THE MATRIX IS THE SAME IN BOTH CASES BUT
C             WITH PSI SCHEME THE DIAGONAL IS NOT ASSEMBLED BECAUSE
C             IT IS ASSEMBLED IN MURD3D
        IF(N_ADV(ADV_NSC).GT.0.AND..NOT.(OPT_HNEG.EQ.2.OR.SIGMAG)) THEN
          FORMUL = 'MAMURD 2     N  '
        ELSE
          FORMUL = 'MAMURD 2     PSI'
        ENDIF
        CALL MATRIX
C                                                           !!!!!!!
     &  (MMURD,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,MTRA1%X,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
C       HERE THE BYPASS IS NOT OPTIONAL, OTHERWISE
C       THE SCHEMES ARE NOT MASS-CONSERVATIVE
C       IF(BYPASS) THEN
        IF(OPT_HNEG.EQ.2.OR.SIGMAG) THEN
          CALL BYPASS_CRUSHED_POINTS_EBE(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   MMURD%X%R,T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,NELEM2,NELEM3,NPLAN,
     &                                   MESH3D%IKLE%I)
          IF(N_ADV(ADV_NSC).GT.0) THEN
            CALL DIAG_MURD(MMURD%D%R,MMURD%X%R,NELEM3,MESH3D%NELMAX,
     &                     NPOIN3,MESH3D%IKLE%I)
          ENDIF
        ENDIF
C       ENDIF
!
      ENDIF
!
!=======================================================================
C     PREPARES ADVECTION BY MURD METHOD IN EDGE-BASED FORM
C     STORAGE IS ALWAYS EDGE-BASED
!=======================================================================
!
      IF(N_ADV(ADV_NSC_TF).GT.0) THEN
!
C       NOTE: THE MATRIX IS THE SAME IN BOTH CASES BUT
C             WITH PSI SCHEME THE DIAGONAL IS NOT ASSEMBLED
C             IT IS WHAT WE WANT HERE
        FORMUL = 'MAMURD 2     PSI'
        CALL MATRIX 
C                                                             !!!!!!!
     &  (MURD_TF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,MTRA1%X,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
C       FROM 30 SEGMENTS WITH POSITIVE FLUXES, WE GO TO 15 WITH
C       POSITIVE OR NEGATIVE FLUXES
        DO I=1,NSEG3D
          MURD_TF%X%R(I) = MURD_TF%X%R(I) - MURD_TF%X%R(I+NSEG3D)
        ENDDO
C       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   MURD_TF%X%R,
     &                                   T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,ADV_NSC_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
        IF(NCSIZE.GT.1) THEN
C         ASSEMBLED FORM OF FLUXES STORED IN SECOND PART
C         OF MATRIX WHICH OTHERWISE IS NOT USED
          CALL OV('X=Y     ',MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                       MURD_TF%X%R(       1:  NSEG3D),
     &                       MURD_TF%X%R(       1:  NSEG3D),
     &                       0.D0,NSEG3D)
          CALL PARCOM2_SEG(MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,2)
        ENDIF
!
      ENDIF
!
!=======================================================================
C     PREPARES LEO POSTMA ADVECTION SCHEMES
!=======================================================================
!
C     RETRIEVES VERTICAL FLUXES FROM WSCONV
C     VERTICAL FLUXES ARE STORED IN FLODEL AFTER
C     THE HORIZONTAL FLUXES (THERE ARE NSEG*NPLAN HORIZONTAL FLUXES)
C     USEFUL SIZE OF WSCONV IS (NPOIN2,NPLAN-1)
!
      IF(N_ADV(ADV_LPO).GT.0.OR.N_ADV(ADV_LPO_TF).GT.0) THEN
        IS=MESH2D%NSEG*NPLAN
        DO IP=1,NPLAN-1
          DO I=1,NPOIN2
            IWS=I+(IP-1)*NPOIN2
C           NOTE 1: WSCONV IS ALREADY ASSEMBLED
C                   USING VOLU2D FLODEL WILL BE THE NON ASSEMBLED FORM
C           NOTE 2: WE COULD KEEP THE ORIGINAL RIGHT HAND SIDE IN
C                   TRIDW2
C           NOTE 3: AGAIN CONVENTION REVERSED, HERE FLOW FROM
C                   POINT 2 (UP) TO POINT 1 (DOWN)
            FLODEL%R(IS+IWS)=-WSCONV%R(IWS)*VOLU2D%R(I)
          ENDDO
        ENDDO
C       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   FLODEL%R,
     &                                   T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,ADV_LPO_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
      ENDIF
!
!     FLOPAR = FLODEL ASSEMBLED IN PARALLEL MODE
!
      IF(OPTHNEG.EQ.2.OR.N_ADV(ADV_LPO)   .GT.0
     &               .OR.N_ADV(ADV_LPO_TF).GT.0) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=FLOPAR,Y=FLODEL)
          CALL PARCOM2_SEG(FLOPAR%R,FLOPAR%R,FLOPAR%R,
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,1)
        ELSE
          FLOPAR%R=>FLODEL%R
        ENDIF
      ENDIF
!
!=======================================================================
!     PREPARES ADVECTION BY SUPG METHOD
!=======================================================================
!
      IF(N_ADV(ADV_SUP).GT.0) THEN
!
         IF(OPTSUP(1).EQ.2) THEN
C          HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=CFL)
           FORMUL = 'MAUGUG2         '
           CALL MATRIX
     &     (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,0.5D0*DT,SVIDE,SVIDE,
     &      SVIDE,UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
C          MSUPG IS SYMMETRICAL
         ELSEIF(OPTSUP(1).EQ.1) THEN
C          HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=1)
           FORMUL = 'MAUGUG1         '
           CALL MATRIX
     &     (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,1.D0,SVIDE,SVIDE,
     &      SVIDE,UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
C          MSUPG IS NOT SYMMETRICAL
         ELSEIF(OPTSUP(1).NE.0) THEN
           CALL PLANTE(1)
           STOP 'UNEXPECTED VALUE OF OPTSUP IN PRECON'
         ENDIF
!
C        MSUPG TRANSFORMED INTO NON SYMMETRICAL MATRIX
         IF(OPTSUP(1).EQ.2) THEN
           CALL OM('M=X(M)  ',MSUPG,MSUPG,SVIDE,0.D0,MESH3D)
           OPER='M=M+N   '
         ELSEIF(OPTSUP(1).EQ.1) THEN
           OPER='M=M+N   '
         ELSE
           OPER='M=N     '
         ENDIF
!
C        ADDS CENTRED ADVECTION TERM
!
         FORMUL = 'MATVGR          '
         FORMUL(8:8) = '2'
         CALL MATRIX
     &   (MSUPG,OPER,FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &    UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
C        VERTICAL UPWIND (SUBROUTINE UPWIND EXPECTS SYMMETRICAL MATRICES)
C        HERE UPWIND COEFFICIENT = 1, BUT WSCONV USED INSTEAD OF W
!
         CALL UPWIND(MSUPG,WSCONV,1.D0,MESH2D,MESH3D,NPLAN)
!
      ENDIF
!
!=======================================================================
!
C     RESTORES MESH3D%Z
!
      MESH3D%Z%R=>SAVEZ
!
!=======================================================================
!
!     COMPUTES DELTAZ*WSTAR (IN WPS) AT NODES
!
      CALL WSTAR(WPS,WSCONV,Z,NPOIN2,NPLAN)
!
!=======================================================================
!
!     COMPUTES W FROM  (DZW*)JH,IV+1/2
!
!        (WITH HYDROSTATIC ASSUMPTION, W IS NEVER USED,
!                  IT IS DONE HERE FOR OUTPUTS)
!        HOWEVER IT IS ALWAYS USED WITH THE K-EPSILON OR K-OMEGA MODELS
!
      IF(.NOT.NONHYD) THEN
        IF(((LT/GRAPRD)*GRAPRD.EQ.LT.AND.LT.GE.GRADEB).OR.
     &      (ITURBV.EQ.3.OR.ITURBV.EQ.7)) THEN
          CALL WSTARW(WP,WSCONV,T3_03%R,T3_04%R,T3_05%R)
        ENDIF
      ENDIF
!
!=======================================================================
!
! ADVECTION BY METHOD OF CHARACTERISTICS
!
!=======================================================================
!
      IF(N_ADV(ADV_CAR).GT.0) THEN
!
!       NOTES:
!
!       IN BLOCK FN3D THERE IS U,V,W INSTEAD OF UN,VN,WN
!       BECAUSE ADVECTION IS DONE FOR THE NEXT TIME STEP
!
!       FN3D%ADR(ADV_CAR)%P IS THE BLOCK OF VARIABLES ADVECTED WITH
!       SCHEME ADV_CAR (SEE POINT_TELEMAC3D)
!
        CALL CHARAC(FN3D,FC3D,FC3D%N,UCONVC,VCONVC,WPS,ZCHAR,
     &              DT,MESH3D%IFABOR,IELM3,NPOIN2,NPLAN,NPLINT,
     &              MSK,MASKEL,MTRA2%X,MTRA2%D,TRAV3,
     &              IT1%I,IT2%I,IT3%I,IT4%I,
     &              MESH3D,NELEM2,MESH2D%NELMAX,IKLE2,MESH2D%SURDET)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
