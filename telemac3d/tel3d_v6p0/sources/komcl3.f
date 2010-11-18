C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!>                MODEL IS K-OMEGA.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, ALPHA, BETA, BETAS, DISBOR, DNUVIH, DNUVIV, EBORF, EBORL, EBORS, EMAX, EMIN, EP, GRAV, H, KADH, KARMAN, KBORF, KBORL, KBORS, KENT, KENTU, KLOG, KMAX, KMIN, KSORT, LIEBOF, LIEBOL, LIEBOS, LIKBOF, LIKBOL, LIKBOS, LISRUF, LISRUL, LIUBOF, LIUBOL, LIUBOS, NBOR, NPLAN, NPOIN2, NPTFR, OMSTAR, RUGOL, SCHMIT, U, UETCAL, UETCAR, V, VIRT, W, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CEPS, DENOM, DIST, DISTFOND, EFOND, ESURF, FICTIFOM, FICTIFUET, HAUT, IPLAN, IPOIN2, IPTFR, ITER, KFOND, KRPLUS, MAXITEREICH, NIVTURB, PROPNU, SBETAS, SR, TEST, TESTREICH, UETREICH, UETUTA, UTANG, YPLUS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
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
!>      <td><center>                                           </center>
!> </td><td> **/02/2001
!> </td><td> HOLGER WEILBEER   ISEB/UHA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AK
!></td><td>--></td><td>ENERGIE TURBULENTE
!>    </td></tr>
!>          <tr><td>ALPHA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ALPHA ETC
!></td><td>--></td><td>CONSTANTE DU MODELE K-OMEGA
!>    </td></tr>
!>          <tr><td>AUBOR
!></td><td><--</td><td>CONTRAINTE IMPLICITE POUR U ET V AUX BORDS
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETAS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT POUR K-EPSILON
!>    </td></tr>
!>          <tr><td>DISBOR
!></td><td>--></td><td>DISTANCE AU BORD DES POINTS VOISINS DU BORD
!>    </td></tr>
!>          <tr><td>DNUVIH
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION HORIZONTALE
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION VERTICALE
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td><--</td><td>DISSIPATION TURBULENTE IMPOSEE AUX BORDS
!>    </td></tr>
!>          <tr><td>EBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EBORL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EMIN,EMAX
!></td><td>--></td><td>EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>EP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>CONVENTION POUR UNE PAROI AVEC ADHERENCE
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KBOR
!></td><td><--</td><td>ENERGIE TURBULENTE IMPOSEE AUX BORDS
!>    </td></tr>
!>          <tr><td>KBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KBORL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT, KENTU
!></td><td>--></td><td>CONVENTION POUR UN POINT A VALEUR IMPOSEE
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR UNE PAROI LOGARITHMIQUE
!>    </td></tr>
!>          <tr><td>KMIN,KMAX
!></td><td>--></td><td>K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>CONVENTION POUR UN POINT A VALEUR LIBRE
!>    </td></tr>
!>          <tr><td>LIEBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIEBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIEBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES POUR EP
!>    </td></tr>
!>          <tr><td>LIEBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIKBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIKBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIKBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES POUR AK
!>    </td></tr>
!>          <tr><td>LIKBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LISRUF
!></td><td>--></td><td>REGIME DE TURBULENCE 1: LISSE
!>                  POUR LE FOND         2: RUGUEUX
!>                  3: RUGUEUX (CHEZY)
!>    </td></tr>
!>          <tr><td>LISRUL
!></td><td>--></td><td>REGIME DE TURBULENCE 1: LISSE
!>                  DES BORDS            2: RUGUEUX
!>                  3: RUGUEUX (CHEZY)
!>    </td></tr>
!>          <tr><td>LIUBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES POUR U
!>    </td></tr>
!>          <tr><td>LIUBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS  DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>OMSTAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RUGO
!></td><td>--></td><td>COEFFICIENTS DE RUGOSITE
!>    </td></tr>
!>          <tr><td>RUGOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SCHMIT
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>COMPOSANTES X DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>UETCAL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UETCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USTAR
!></td><td><--</td><td>VITESSE DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>COMPOSANTES Y DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>VIRT
!></td><td>--></td><td>ORIGIN VIRTUEL POUR EPSILON (TELEMAC 3D)
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTES DES POINTS DU MAILLAGE 3D REEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KOMCL3
     & (KBORF,EBORF,LIKBOF,LIEBOF,LIUBOF,
     &  KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,RUGOL,
     &  KBORS,EBORS,LIKBOS,LIEBOS,LIUBOS,
     &  DISBOR,AK,EP,U,V,W,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     &  DNUVIH,DNUVIV,KARMAN, ALPHA,BETA,BETAS,OMSTAR,SCHMIT,LISRUF,
     &  LISRUL,VIRT,GRAV,KMIN,KMAX,EMIN,EMAX,KENTU,KENT,KSORT,KADH,KLOG,
     &  UETCAR,UETCAL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |-->| ENERGIE TURBULENTE
C| ALPHA          |---| 
C| ALPHA ETC      |-->| CONSTANTE DU MODELE K-OMEGA
C| AUBOR          |<--| CONTRAINTE IMPLICITE POUR U ET V AUX BORDS
C| BETA           |---| 
C| BETAS          |---| 
C| CF             |<--| COEFFICIENT DE FROTTEMENT POUR K-EPSILON
C| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
C| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
C| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
C| EBOR           |<--| DISSIPATION TURBULENTE IMPOSEE AUX BORDS
C| EBORF          |---| 
C| EBORL          |---| 
C| EBORS          |---| 
C| EMIN,EMAX      |-->| EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| EP             |---| 
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| H             |-->| HAUTEUR D'EAU AU TEMPS N
C| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KBOR           |<--| ENERGIE TURBULENTE IMPOSEE AUX BORDS
C| KBORF          |---| 
C| KBORL          |---| 
C| KBORS          |---| 
C| KENT, KENTU    |-->| CONVENTION POUR UN POINT A VALEUR IMPOSEE
C| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
C| KMIN,KMAX      |-->| K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| KSORT          |-->| CONVENTION POUR UN POINT A VALEUR LIBRE
C| LIEBOF         |---| 
C| LIEBOL         |---| 
C| LIEBOR         |-->| TYPES DE CONDITIONS AUX LIMITES POUR EP
C| LIEBOS         |---| 
C| LIKBOF         |---| 
C| LIKBOL         |---| 
C| LIKBOR         |-->| TYPES DE CONDITIONS AUX LIMITES POUR AK
C| LIKBOS         |---| 
C| LISRUF         |-->| REGIME DE TURBULENCE 1: LISSE
C|                |   | POUR LE FOND         2: RUGUEUX
C|                |   | 3: RUGUEUX (CHEZY)
C| LISRUL         |-->| REGIME DE TURBULENCE 1: LISSE
C|                |   | DES BORDS            2: RUGUEUX
C|                |   | 3: RUGUEUX (CHEZY)
C| LIUBOF         |---| 
C| LIUBOL         |---| 
C| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES POUR U
C| LIUBOS         |---| 
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
C| OMSTAR         |---| 
C| RUGO           |-->| COEFFICIENTS DE RUGOSITE
C| RUGOL          |---| 
C| SCHMIT         |-->| CONSTANTE DU MODELE K-EPSILON
C| U             |-->| COMPOSANTES X DE LA VITESSE AU TEMPS N
C| UETCAL         |---| 
C| UETCAR         |---| 
C| USTAR          |<--| VITESSE DE FROTTEMENT
C| V             |-->| COMPOSANTES Y DE LA VITESSE AU TEMPS N
C| VIRT           |-->| ORIGIN VIRTUEL POUR EPSILON (TELEMAC 3D)
C| W             |---| 
C| Z             |-->| COTES DES POINTS DU MAILLAGE 3D REEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN) :: LISRUF, LISRUL,KENTU,KENT,KSORT,KADH,KLOG
!
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: AK(NPOIN2,NPLAN),EP(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2),UETCAR(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: KBORF(NPOIN2), KBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: KBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORF(NPOIN2), EBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: UETCAL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN)    :: DISBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: RUGOL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN) :: VIRT, DNUVIH, DNUVIV, KARMAN
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA,BETAS,OMSTAR
      DOUBLE PRECISION, INTENT(IN) :: SCHMIT,GRAV
      DOUBLE PRECISION, INTENT(IN) :: KMIN, KMAX, EMIN, EMAX
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION KRPLUS, SR
      DOUBLE PRECISION YPLUS, DISTFOND
      DOUBLE  PRECISION, PARAMETER :: FICTIFUET = 3.7D0
      DOUBLE  PRECISION, PARAMETER :: FICTIFOM  = 2.D0
      DOUBLE  PRECISION, PARAMETER :: NIVTURB = 0.02D0
      DOUBLE  PRECISION, PARAMETER :: TESTREICH = 1.D-4
      DOUBLE  PRECISION UETREICH, TEST
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR, IPLAN, IPOIN2, ITER
!
      DOUBLE PRECISION EFOND, KFOND, ESURF, HAUT, DENOM, CEPS
      DOUBLE PRECISION SBETAS,UTANG, UETUTA, DIST, PROPNU
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      SBETAS = 1.D0/SQRT(BETAS)
      PROPNU = (2.D0*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
!
C                        /* LOOP ON THE BOTTOM */
!
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
C        DIRICHLET ON K
!        ---------------
!
         IF(LIKBOF(IPOIN2).EQ.KENT) THEN
!        -----------------------------
!
          IF(LIUBOF(IPOIN2).EQ.KLOG) THEN
            KBORF(IPOIN2) = MAX(SBETAS*UETCAR(IPOIN2),KMIN)
          ELSE
            KBORF(IPOIN2) = KMIN
          ENDIF
!
         ENDIF
!
!
C        DIRICHLET ON EPSILON
!        ---------------------
!
         IF(LIEBOF(IPOIN2).EQ.KENT) THEN
!        -------------------------------
!
          IF(LIUBOF(IPOIN2).EQ.KLOG) THEN
!
C           VINCENT BOYER CHOSE 3.7D0
            DIST  = (Z(IPOIN2,2)-Z(IPOIN2,1)) / 3.7D0
            EBORF(IPOIN2) = MAX(SBETAS*SQRT(UETCAR(IPOIN2))
     &                                 /(KARMAN*DIST),EMIN)
!
          ENDIF
!
         ENDIF
!
      ENDDO
!
!=======================================================================
!
C                        /* LOOP ON THE SURFACE */
!
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
         HAUT = MAX(H(IPOIN2),1.D-7)
!
C        DIRICHLET ON EPSILON
!        ---------------------
!
         IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!        -------------------------------
!
           EBORS(IPOIN2) = OMSTAR*AK(IPOIN2,NPLAN)**0.5D0/HAUT
!
         ENDIF
!
      ENDDO
!
!=======================================================================
!
C                        /* LOOP ON THE LATERAL BOUNDARIES */
!
!
C  COMPUTES KBOR,EBOR, AND AUBOR
!
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
         IPOIN2 = NBOR(IPTFR)
         DIST   = DISBOR(IPTFR) / FICTIFUET
         HAUT   = MAX( H(IPOIN2), 1.D-7 )
!
         DO IPLAN=1,NPLAN
!
CBOY COMPUTES THE DISTANCE TO THE BOTTOM
            IF (IPLAN .EQ. 1) THEN
              DISTFOND =  (Z(IPOIN2,2)-Z(IPOIN2,1)) / FICTIFUET
            ELSE
              DISTFOND =  (Z(IPOIN2,IPLAN)-Z(IPOIN2,1))
            ENDIF
C COMPUTES THE TANGENTIAL SPEED
            UTANG = SQRT(U(IPOIN2,IPLAN)**2+V(IPOIN2,IPLAN)**2)
!
C           DIRICHLET ON K
!           ---------------
!
            IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!           ------------------------------------
!              ************************************
               IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU) THEN
!              ************************************
!
C              COMING INTO THE DOMAIN
!
               KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN),KMIN)
!
C              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG) THEN
C              ****************************************
C
C                WALL
C
                 KBORL(IPTFR,IPLAN)=MAX(SBETAS*UETCAL(IPTFR,IPLAN),KMIN)
C
C              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
C              ****************************************
C
                  KBORL(IPTFR,IPLAN) = KMIN
!
!              ****
               ELSE
!              ****
!
                  IF (LNG.EQ.1) WRITE(LU,111) IPTFR,LIUBOL(IPTFR,IPLAN)
                  IF (LNG.EQ.2) WRITE(LU,112) IPTFR,LIUBOL(IPTFR,IPLAN)
                  CALL PLANTE(1)
                  STOP
!
!              *****
               ENDIF
!              *****
!
            ENDIF
!           -----
!
C           DIRICHLET ON EPSILON
!           ---------------------
!
            IF(LIEBOL(IPTFR,IPLAN).EQ.KENT) THEN
!           ------------------------------------
!
!              ************************************
               IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU) THEN
!              ************************************
!
C                 COMING INTO THE DOMAIN : TURBULENCE DUE TO BOTTOM
!
C BOUNDARY CONDITIONS COMING RFOM THE K-EPSILON MODEL:
C                  EBORL(IPTFR,IPLAN) = CMU**0.75*KBORL(IPTFR,1)**1.5
C     &                      /KARMAN/DISTFOND
C AUXILIARY RELATION EPSILON=BETAS*K*OMEGA LEADS TO:
C
                  EBORL(IPTFR,IPLAN)=BETAS**(-0.25)*SQRT(KBORL(IPTFR,1))
     &                              /KARMAN/DISTFOND
                  EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG) THEN
!              ****************************************
!
C               WALL
!
                EBORL(IPTFR,IPLAN) =
     &          MAX(SBETAS*SQRT(UETCAL(IPTFR,IPLAN))/(KARMAN*DIST),EMIN)
!
C              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
C              ****************************************
C
CHOL OMEGA = USTAR**2*SR/NUE ...
C
                 EBORL(IPTFR,IPLAN) = 2500.D0
CHOL
!              ****
               ELSE
!              ****
!
C                 OTHER
!
                  IF (LNG.EQ.1) WRITE(LU,121) IPTFR,LIUBOL(IPTFR,IPLAN)
                  IF (LNG.EQ.2) WRITE(LU,122) IPTFR,LIUBOL(IPTFR,IPLAN)
                  CALL PLANTE(1)
                  STOP
!
!              *****
               ENDIF
!              *****
!
            ENDIF
!           -----
!
         ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
101   FORMAT(' KOMCL3 : REGIME DE TURBULENCE INCONNU : ',I6)
102   FORMAT(' KOMCL3 : UNKNOWN TURBULENCE MODEL : ',I6)
111   FORMAT(' KOMCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR KBOR : LIUBOR =',I6)
112   FORMAT(' KOMCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
121   FORMAT(' KOMCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR EBOR : LIUBOR =',I6)
122   FORMAT(' KOMCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE KOMCL3
C
C#######################################################################
C