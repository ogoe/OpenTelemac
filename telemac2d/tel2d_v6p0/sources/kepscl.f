C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!>                MODEL IS K-EPSILON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AUBOR, C2, CF, CFBOR, CMU, DISBOR, EBOR, EMIN, ESTAR, HN, KADH, KARMAN, KBOR, KDIR, KENT, KENTU, KLOG, KMIN, KNEU, LIMKEP, LIMPRO, LISRUG, LIUBOR, NBOR, NPTFR, PROPNU, SCHMIT, UN, VN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CEPS, DENOM, DIST, EBORD, EFOND, IT, K, KBORD, KFOND, N, SSQCMU, TIERS, UETUTA, USTAR, UTANG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 26/04/1994
!> </td><td> L. VAN HAREN (LNH) 30 87 84 14
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/11/1992
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AUBOR
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT SUR LES PAROIS
!>    </td></tr>
!>          <tr><td>C1,C2
!></td><td>--></td><td>CONSTANTES DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT POUR K-EPSILON
!>    </td></tr>
!>          <tr><td>CFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CMU
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>DISBOR
!></td><td>--></td><td>DISTANCE AU BORD DES POINTS VOISINS DU BORD
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td><--</td><td>DISSIPATION TURBULENTE IMPOSEE AU BORD
!>    </td></tr>
!>          <tr><td>EMIN,EMAX
!></td><td>--></td><td>EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>ESTAR
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>CONVENTION POUR UNE PAROI AVEC ADHERENCE
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KBOR
!></td><td><--</td><td>ENERGIE TURBULENTE IMPOSEE AU BORD
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>CONDITION A LA LIMITE DE DEGRE DE LIBERTE
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE DIRICHLET
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>CONVENTION POUR UNE ENTREE LIQUIDE
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>--></td><td>CONVENTION POUR DES VITESSES IMPOSEES
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>--></td><td>CONVENTION POUR UNE ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR UNE PAROI LOGARITHMIQUE
!>    </td></tr>
!>          <tr><td>KMIN,KMAX
!></td><td>--></td><td>K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE NEUMANN
!>    </td></tr>
!>          <tr><td>KOND
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMERO DU POINT SUIVANT SUR LE BORD
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>CONVENTION POUR UNE SORTIE LIQUIDE
!>    </td></tr>
!>          <tr><td>LIMKEP
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR K ET EPSILON
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>CONDITIONS AUX LIMITES EN PROPAGATION
!>    </td></tr>
!>          <tr><td>LISRUG
!></td><td>--></td><td>REGIME DE TURBULENCE 1: LISSE 2: RUGUEUX
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>PROPNU
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION MOLECULAIRE
!>    </td></tr>
!>          <tr><td>SCHMIT
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SIGMAE
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SIGMAK
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>UN , VN
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>--></td><td>DIFFUSION TURBULENTE (FAITE DANS VISTUR)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KEPSCL
     &(KBOR,EBOR,AUBOR,CF,CFBOR,DISBOR,
     & UN,VN,HN,LIMKEP,LIUBOR,LIMPRO,NBOR,NPTFR,
     & KARMAN,CMU,C2,ESTAR,SCHMIT,LISRUG,PROPNU,KMIN,EMIN,
     & KNEU,KDIR,KENT,KENTU,KADH,KLOG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AUBOR          |<--| COEFFICIENT DE FROTTEMENT SUR LES PAROIS
C| C1,C2          |-->| CONSTANTES DU MODELE K-EPSILON
C| CF             |-->| COEFFICIENT DE FROTTEMENT POUR K-EPSILON
C| CFBOR          |---| 
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
C| EBOR           |<--| DISSIPATION TURBULENTE IMPOSEE AU BORD
C| EMIN,EMAX      |-->| EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| ESTAR          |-->| CONSTANTE DU MODELE K-EPSILON
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| HN             |-->| HAUTEUR D'EAU AU TEMPS N
C| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KBOR           |<--| ENERGIE TURBULENTE IMPOSEE AU BORD
C| KDDL           |-->| CONDITION A LA LIMITE DE DEGRE DE LIBERTE
C| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
C| KENT           |-->| CONVENTION POUR UNE ENTREE LIQUIDE
C| KENTU          |-->| CONVENTION POUR DES VITESSES IMPOSEES
C| KINC           |-->| CONVENTION POUR UNE ONDE INCIDENTE
C| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
C| KMIN,KMAX      |-->| K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| KNEU           |-->| CONDITION A LA LIMITE DE TYPE NEUMANN
C| KOND           |-->| CONDITION A LA LIMITE DE TYPE ONDE INCIDENTE
C| KP1BOR         |-->| NUMERO DU POINT SUIVANT SUR LE BORD
C| KSORT          |-->| CONVENTION POUR UNE SORTIE LIQUIDE
C| LIMKEP         |-->| CONDITIONS AUX LIMITES SUR K ET EPSILON
C| LIMPRO         |-->| CONDITIONS AUX LIMITES EN PROPAGATION
C| LISRUG         |-->| REGIME DE TURBULENCE 1: LISSE 2: RUGUEUX
C| LIUBOR         |-->| CONDITIONS AUX LIMITES SUR U
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| PROPNU         |-->| COEFFICIENT DE DIFFUSION MOLECULAIRE
C| SCHMIT         |-->| CONSTANTE DU MODELE K-EPSILON
C| SIGMAE         |-->| CONSTANTE DU MODELE K-EPSILON
C| SIGMAK         |-->| CONSTANTE DU MODELE K-EPSILON
C| UN , VN        |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
C| VISC           |-->| DIFFUSION TURBULENTE (FAITE DANS VISTUR)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,LISRUG
      INTEGER, INTENT(IN) :: KNEU,KDIR,KENT,KADH,KLOG,KENTU
      INTEGER, INTENT(IN) :: LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: LIMKEP(NPTFR,2),LIUBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: KMIN,EMIN
      DOUBLE PRECISION, INTENT(IN)    :: CF(*),CFBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: UN(*),VN(*),HN(*),DISBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: KBOR(*),EBOR(*),AUBOR(*)
      DOUBLE PRECISION, INTENT(IN) :: KARMAN,CMU,C2,ESTAR,SCHMIT,PROPNU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,K,IT
C
      DOUBLE PRECISION KFOND,EFOND,TIERS,UETUTA,CEPS,USTAR
      DOUBLE PRECISION SSQCMU,UTANG,DIST,DENOM,EBORD,KBORD
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT,MAX,LOG
C
C-----------------------------------------------------------------------
C
      TIERS  = 1.D0/3.D0
      SSQCMU = 1.D0/ SQRT(CMU)
C
C=======================================================================
C
C  LOOP ON THE BOUNDARY NODES
C
C  COMPUTES THE FRICTION VELOCITY ON THE WALL
C
C  COMPUTES KBOR,EBOR, AND AUBOR
C
C=======================================================================
C
      DO 1000 K=1,NPTFR
C
         KBOR(K) = 0.D0
         EBOR(K) = 0.D0
         N     = NBOR(K)
         UTANG = SQRT( UN(N)**2 + VN(N)**2 )
C        BEWARE : MODIFIED FROM PRINCIPLE NOTE
C        DIST  = DISBOR(K)*0.1D0
         DIST  = DISBOR(K)*0.33D0
C
C        COMPUTES UETOIL FOR SOLID BOUNDARIES
C        ----------------------------------------
C
C                           UETOIL
C        UETUTA REPRESENTS  ------ EVERYWHERE
C                           UTANG
C
C        UETUTA HAS THE ADVANTAGE OF BEING MEANINGFUL EVEN IF UTANG=0
C
C        ********************
         IF(LISRUG.EQ.1) THEN
C        ********************
C
C           INITIAL GUESS; THEN 5 ITERATIONS
            UETUTA = 6.D-2
            DO 60 IT=1,5
C
             IF(DIST*UETUTA*UTANG/PROPNU .LT. 30.D0) THEN
               UETUTA = 7.25D-2
             ELSE
               UETUTA=1.D0/(5.5D0+LOG(DIST*UETUTA*UTANG/PROPNU)/KARMAN)
             ENDIF
C
60          CONTINUE
C
C        ************************
         ELSEIF(LISRUG.EQ.2) THEN
C        ************************
C
            UETUTA = SQRT( 0.5D0 * CFBOR(K) )
C
C        ****
         ELSE
C        ****
C
            IF(LNG.EQ.1) WRITE(LU,400) LISRUG
            IF(LNG.EQ.2) WRITE(LU,401) LISRUG
400         FORMAT(1X,'KEPSCL : REGIME DE TURBULENCE INCONNU : ',1I6)
401         FORMAT(1X,'KEPSCL : UNKNOWN TURBULENCE MODEL : ',1I6)
            CALL PLANTE(1)
            STOP
C
C        *****
         ENDIF
C        *****
C
C                                                        DIRICHLET ON K
C                                                        ---------------
C
C
         IF(LIMKEP(K,1).EQ.KDIR) THEN
C        ----------------------------
C
C           ************************************************
            IF(LIUBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU) THEN
C           ************************************************
C
C              INPUT BOUNDARY: TURBULENCE DUE TO THE BOTTOM
C
               CEPS    = C2*SQRT(CMU)/SQRT(ESTAR*SCHMIT) /
     &                   (0.5D0*CF(N))**0.75D0
               DENOM   = CEPS * 0.5D0*CF(N)
               USTAR = SQRT( 0.5D0 * CF(N) * ( UN(N)**2 + VN(N)**2 ) )
               KBOR(K) = C2 * USTAR**2 / MAX(DENOM,1.D-10)
C
C           ***************************************************
            ELSEIF(LIUBOR(K).EQ.KLOG.OR.LIUBOR(K).EQ.KADH) THEN
C           ***************************************************
C
C              WALL
C
               CEPS    = C2*SQRT(CMU)/SQRT(ESTAR*SCHMIT) /
     &                   (0.5D0*CF(N))**0.75D0
               DENOM   = CEPS * 0.5D0*CF(N)
               USTAR = SQRT( 0.5D0 * CF(N) * ( UN(N)**2 + VN(N)**2 ) )
               KFOND   = C2 * USTAR**2 / MAX(DENOM,1.D-10)
               KBORD   = SSQCMU*(UETUTA*UTANG)**2
               KBOR(K) = KBORD + KFOND
C
C           ****
            ELSE
C           ****
C
               IF(LNG.EQ.1) WRITE(LU,500) K,LIUBOR(K)
               IF(LNG.EQ.2) WRITE(LU,501) K,LIUBOR(K)
500            FORMAT(1X,'KEPSCL: POINT DE BORD ',1I6,
     &                   'CAS NON PREVU POUR KBOR',1X,'LIUBOR=',1I6)
501            FORMAT(1X,'KEPSCL: BOUNDARY POINT ',1I6,
     &                   'UNKNOWN CASE FOR KBOR',1X,'LIUBOR=',1I6)
               CALL PLANTE(1)
               STOP
C
C           *****
            ENDIF
C           *****
C
         ENDIF
C        -----
C
C                                                  DIRICHLET ON EPSILON
C                                                  ---------------------
C
         IF(LIMKEP(K,2).EQ.KDIR) THEN
C        ----------------------------
C
C           ************************************************
            IF(LIUBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU) THEN
C           ************************************************
C
C              INPUT BOUNDARY: TURBULENCE DUE TO THE BOTTOM
C
               DENOM   = SQRT(0.5D0*CF(N)) * HN(N)
               USTAR   = SQRT(0.5D0*CF(N) * ( UN(N)**2 + VN(N)**2 ) )
               EFOND   = USTAR**3 / MAX(DENOM,1.D-10)
               EBOR(K) = MAX( EFOND , EMIN )
C
C           ***************************************************
            ELSEIF(LIUBOR(K).EQ.KLOG.OR.LIUBOR(K).EQ.KADH) THEN
C           ***************************************************
C
C              WALL
C
               DENOM   = SQRT(0.5D0*CF(N)) * HN(N)
               USTAR   = SQRT(0.5D0*CF(N) * ( UN(N)**2 + VN(N)**2 ) )
               EFOND   = USTAR**3 / MAX(DENOM,1.D-10)
               EBORD   = (UETUTA*UTANG)**3 / ( KARMAN*DIST )
               EBOR(K) = MAX( EBORD + EFOND, EMIN )
C
C           ****
            ELSE
C           ****
C
C              OTHER
C
               IF(LNG.EQ.1) WRITE(LU,600) K,LIUBOR(K)
               IF(LNG.EQ.2) WRITE(LU,601) K,LIUBOR(K)
600            FORMAT(1X,'KEPSCL: POINT DE BORD ',1I6,
     &                   'CAS NON PREVU POUR EBOR',1X,'LIUBOR=',1I6)
601            FORMAT(1X,'KEPSCL: BOUNDARY POINT ',1I6,
     &                   'UNKNOWN CASE FOR EBOR',1X,'LIUBOR=',1I6)
               CALL PLANTE(1)
               STOP
C
C           *****
            ENDIF
C           *****
C
         ENDIF
C        -----
C
C                                                        COMPUTES AUBOR
C                                                        ---------------
C
C
C  AUBOR COUNTS FOR THE SEGMENT BETWEEN K AND KP1BOR(K)
C
C  LAW        : NUT * DU/DN = UETOIL**2 = -AUBOR*U(N+1)
C  CHANGED TO : NUT * DU/DN = UETOIL**2  *  U(N+1) / U(N)
C                           = UETOIL * (UETOIL/UTANG) * U(N+1)
C
         IF (LIMPRO(K,5).EQ.KNEU) THEN
            AUBOR(K) = - UTANG * UETUTA**2
         ELSE
            AUBOR(K) = 0.D0
         ENDIF
C
1000  CONTINUE
C
C=======================================================================
C
C                   /* END OF LOOP ON BOUNDARY NODES */
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C