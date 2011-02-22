C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!>                MODEL IS K-EPSILON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, CMU, DISBOR, DNUVIH, DNUVIV, EBORF, EBORL, EBORS, EMAX, EMIN, H, KADH, KARMAN, KBORF, KBORL, KBORS, KENT, KENTU, KLOG, KMAX, KMIN, KSORT, LIEBOF, LIEBOL, LIEBOS, LIKBOF, LIKBOL, LIKBOS, LISRUF, LISRUL, LIUBOF, LIUBOL, LIUBOS, NBOR, NPLAN, NPOIN2, NPTFR, RUGOL, U, UETCAL, UETCAR, V, VIRT, Z
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::IPBOT IPBOT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DENOM, DIST, DISTFOND, EFOND, ESURF, FICTIFEPS, FICTIFUET, HAUT, IBOT, IP, IPLAN, IPOIN2, IPOIN3, IPTFR, ITER, KFOND, MAXITEREICH, NIVTURB, PROPNU, SSQCMU, TEST, TESTREICH, UETREICH, UETUTA, UTANG, YPLUS
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 04/01/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; V. BOYER UMIST
!> </td><td>
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
                        SUBROUTINE KEPCL3
     &(KBORF,EBORF,LIKBOF,LIEBOF,LIUBOF,
     & KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,RUGOL,
     & KBORS,EBORS,LIKBOS,LIEBOS,LIUBOS,
     & DISBOR,AK,U,V,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     & DNUVIH,DNUVIV,KARMAN,CMU,LISRUF,LISRUL,
     & VIRT,KMIN,KMAX,EMIN,EMAX,KENT,KENTU,KSORT,KADH,KLOG,
     & UETCAR,UETCAL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |-->| ENERGIE TURBULENTE
C| C2             |-->| CONSTANTE DU MODELE K-EPSILON
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
C| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
C| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
C| EBOR           |<--| DISSIPATION TURBULENTE IMPOSEE AUX BORDS
C| EBORF          |---| 
C| EBORL          |---| 
C| EBORS          |---| 
C| EMIN,EMAX      |-->| EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| ESTAR          |-->| CONSTANTE DU MODELE K-EPSILON
C| H             |-->| HAUTEUR D'EAU AU TEMPS N
C| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KBOR           |<--| ENERGIE TURBULENTE IMPOSEE AUX BORDS
C| KBORF          |---| 
C| KBORL          |---| 
C| KBORS          |---| 
C| KENT           |-->| CONVENTION POUR UN POINT A VALEUR IMPOSEE
C| KENTU          |---| 
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
C| RUGOL          |---| 
C| SCHMIT         |-->| CONSTANTE DU MODELE K-EPSILON
C| U             |-->| COMPOSANTES X DE LA VITESSE AU TEMPS N
C| UETCAL         |---| 
C| UETCAR         |---| 
C| V             |-->| COMPOSANTES Y DE LA VITESSE AU TEMPS N
C| VIRT           |-->| ORIGIN VIRTUEL POUR EPSILON (TELEMAC 3D)
C| Z             |-->| COTES DES POINTS DU MAILLAGE 3D REEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT,AEBORF,BEBORF,SIGMAE
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2,KENTU
      INTEGER, INTENT(IN) :: LISRUF,LISRUL,KENT,KSORT,KADH,KLOG
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
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN), AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2)      , UETCAR(NPOIN2)
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
      DOUBLE PRECISION, INTENT(IN) :: CMU
      DOUBLE PRECISION, INTENT(IN) :: KMIN, KMAX, EMIN, EMAX
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2,IPOIN3,ITER,IP,IBOT
!
      DOUBLE PRECISION EFOND, KFOND, ESURF, HAUT, DENOM
      DOUBLE PRECISION SSQCMU, UTANG, UETUTA, DIST, PROPNU
      DOUBLE PRECISION YPLUS, DISTFOND
C                                    VINCENT BOYER'S CHOICE
      DOUBLE PRECISION, PARAMETER :: FICTIFUET = 2.D0
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
      DOUBLE PRECISION, PARAMETER :: NIVTURB = 0.005D0
      DOUBLE PRECISION, PARAMETER :: TESTREICH = 1.D-4
      DOUBLE PRECISION UETREICH, TEST
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      SSQCMU = 1.D0 /SQRT(CMU)
      PROPNU = (2*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
C     BOTTOM
!=======================================================================
!
C     THIS IS DONE IN LIMI3D
C     AEBORF%TYPR='0'
C     BEBORF%TYPR='0'
C     AKBORF%TYPR='0'
C     BKBORF%TYPR='0'
!
      DO IPOIN2=1,NPOIN2
!
        IF(IPBOT%I(IPOIN2).EQ.0) THEN
C         NORMAL CASE
          DIST =(Z(IPOIN2,2)-Z(IPOIN2,1))/FICTIFUET
          IF(LIEBOF(IPOIN2).EQ.KENT) THEN
            EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*SQRT(UETCAR(IPOIN2))
     &                                             /(KARMAN*DIST),EMIN)
          ENDIF
          IF(LIKBOF(IPOIN2).EQ.KENT) THEN
            KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
          ENDIF
        ELSE
C         RISK OF SMASHED PLANES OR TIDAL FLATS
          IPLAN=IPBOT%I(IPOIN2)+1
          IF(IPLAN.EQ.NPLAN) THEN
C           CASE OF TIDAL FLATS
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=EMIN
            ENDIF
C           IN THIS CASE KBORF COMPUTED ABOVE MAY YIELD
C           ABNORMAL VALUES OF VISCOSITY
            IF(LIKBOF(IPOIN2).EQ.KENT) KBORF(IPOIN2)=KMIN
          ELSE
C           CASE OF SMASHED PLANES : DIST COMPUTED ON FIRST FREE LAYER
            DIST =(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN))/FICTIFUET
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*
     &                         SQRT(UETCAR(IPOIN2))/(KARMAN*DIST),EMIN)
            ENDIF
            IF(LIKBOF(IPOIN2).EQ.KENT) THEN
              KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!=======================================================================
C     FREE SURFACE
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
C       DIRICHLET ON EPSILON
!       ---------------------
!
        IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!
C         NEZU & NAKAGAWA: TURBULENCE IN OPEN CHANNEL FLOWS
!
C         SEE KEPICL: THIS VALUE IS NOT USED IF CONDITION
C                     IS NOT SET TO KENT ON EPSILON
          HAUT = MAX(H(IPOIN2),1.D-7)
          ESURF = SQRT(AK(IPOIN2,NPLAN)**3) / (0.18D0*HAUT)
C
C         5.9
C         ESURF = CMU**0.75D0 / KARMAN
C    &          * AK(IPOIN2,NPLAN)**1.5D0 / (VIRT*HAUT)
          EBORS(IPOIN2) = MAX(ESURF,EMIN)
!
        ENDIF
!
      ENDDO
!
!=======================================================================
C     LATERAL BOUNDARIES
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
         IPOIN2 = NBOR(IPTFR)
         DIST   = DISBOR(IPTFR) / FICTIFUET
         HAUT   = MAX(H(IPOIN2),1.D-7)
!
         DO IPLAN=1,NPLAN
!
            IP=MAX(IPLAN,2)
            IBOT=MIN(IPBOT%I(IPOIN2)+1,NPLAN-1)
C           DISTANCE TO BOTTOM (WILL BE 0 WITH TIDAL FLATS)
            DISTFOND = (Z(IPOIN2,IP)-Z(IPOIN2,IBOT))
!
C           DIRICHLET ON K
!           ---------------
!
            IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!           ------------------------------------
!
!              ************************************
               IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!              ************************************
!
C            THEORY BY VINCENT BOYER (SEE ALSO KEPINI)
!
C            KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN)**2,KMIN)
!
C            NO TURBULENCE
!
             KBORL(IPTFR,IPLAN) = KMIN
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &                LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!              ****************************************
!
C                WALL
!
C                KBORL(IPTFR,IPLAN)=MAX(SSQCMU*UETCAL(IPTFR,IPLAN),KMIN)
                 KBORL(IPTFR,IPLAN)=KMIN
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
     &            LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &            LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!              ************************************
!
C                 COMING IN THE DOMAIN: TURBULENCE DUE TO THE
C                 BOTTOM AS IN KEPINI; COMPUTES EBORL ACCORDING
C                 TO KBORL AT THE BOTTOM
!
C                 EBORL(IPTFR,IPLAN)=CMU**0.75*SQRT(KBORL(IPTFR,1)**3)
C    &                              /KARMAN/MAX(DISTFOND,1.D-6)
C                 EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
                  EBORL(IPTFR,IPLAN)=EMIN
!
!              ****************************************
               ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &                LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!              ****************************************
!
C                 WALL
!
C                 EBORL(IPTFR,IPLAN) =
C    &            MAX(UETCAL(IPTFR,IPLAN)*SQRT(UETCAL(IPTFR,IPLAN))/
C    &            (KARMAN*DIST*FICTIFUET/FICTIFEPS),EMIN)
                  EBORL(IPTFR,IPLAN)=EMIN
!
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
101   FORMAT(' KEPCL3 : REGIME DE TURBULENCE INCONNU : ',I6)
102   FORMAT(' KEPCL3 : UNKNOWN TURBULENCE MODEL : ',I6)
111   FORMAT(' KEPCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR KBOR : LIUBOR =',I6)
112   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
121   FORMAT(' KEPCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR EBOR : LIUBOR =',I6)
122   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
