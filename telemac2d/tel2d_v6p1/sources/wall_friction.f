
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

C
C#######################################################################
C
                        SUBROUTINE WALL_FRICTION
     &(UETUTA,AUBOR,CFBOR,DISBOR,UN,VN,LIMPRO,NBOR,NPTFR,
     & KARMAN,PROPNU,LISRUG,KNEU,KDIR,KENT,KENTU,KADH,KLOG,IELMU,KP1BOR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AUBOR          |<--| COEFFICIENT DE FROTTEMENT SUR LES PAROIS
C| CFBOR          |---| 
C| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KDDL           |-->| CONDITION A LA LIMITE DE DEGRE DE LIBERTE
C| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
C| KENT           |-->| CONVENTION POUR UNE ENTREE LIQUIDE
C| KENTU          |-->| CONVENTION POUR DES VITESSES IMPOSEES
C| KINC           |-->| CONVENTION POUR UNE ONDE INCIDENTE
C| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
C| KNEU           |-->| CONDITION A LA LIMITE DE TYPE NEUMANN
C| KOND           |-->| CONDITION A LA LIMITE DE TYPE ONDE INCIDENTE
C| KSORT          |-->| CONVENTION POUR UNE SORTIE LIQUIDE
C| LIMPRO         |-->| CONDITIONS AUX LIMITES EN PROPAGATION
C| LISRUG         |-->| REGIME DE TURBULENCE 1: LISSE 2: RUGUEUX
C| LIUBOR         |-->| CONDITIONS AUX LIMITES SUR U
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| PROPNU         |-->| COEFFICIENT DE DIFFUSION MOLECULAIRE
C| UN , VN        |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,LISRUG,KNEU,KDIR,KENT,KADH,KLOG,KENTU
      INTEGER, INTENT(IN) :: IELMU
      INTEGER, INTENT(IN) :: LIMPRO(NPTFR,6),NBOR(NPTFR),KP1BOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: CFBOR(*),UN(*),VN(*),DISBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: AUBOR(*),UETUTA(*)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,PROPNU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,K,IT
C
      DOUBLE PRECISION UTANG,DIST
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT,LOG
C
C=======================================================================
C
C  LOOP ON THE BOUNDARY NODES
C
C  COMPUTES THE FRICTION VELOCITY ON THE WALL, AND AUBOR
C
C=======================================================================
C
C     ********************
      IF(LISRUG.EQ.1) THEN
C     ********************
C
        DO K=1,NPTFR
          N     = NBOR(K)
          UTANG = SQRT(UN(N)**2+VN(N)**2)
C         BEWARE : MODIFIED FROM PRINCIPLE NOTE
C         DIST  = DISBOR(K)*0.1D0
          DIST  = DISBOR(K)*0.33D0
C         COMPUTES UETOIL FOR SOLID BOUNDARIES
C         ----------------------------------------
C
C                            UETOIL
C         UETUTA REPRESENTS  ------ EVERYWHERE
C                            UTANG
C
C         UETUTA HAS THE ADVANTAGE OF BEING MEANINGFUL EVEN IF UTANG=0
C
C         INITIAL GUESS; THEN 5 ITERATIONS
          UETUTA(K) = 6.D-2
          DO IT=1,5
            IF(DIST*UETUTA(K)*UTANG/PROPNU .LT. 30.D0) THEN
              UETUTA(K) = 7.25D-2
            ELSE
              UETUTA(K)=1.D0/
     *                (5.5D0+LOG(DIST*UETUTA(K)*UTANG/PROPNU)/KARMAN)
            ENDIF
          ENDDO
        ENDDO
C
C     ************************
      ELSEIF(LISRUG.EQ.2) THEN
C     ************************
C 
        DO K=1,NPTFR
          UETUTA(K) = SQRT( 0.5D0 * CFBOR(K) )
        ENDDO 
C 
C     ****
      ELSE
C     ****
C
        IF(LNG.EQ.1) WRITE(LU,400) LISRUG
        IF(LNG.EQ.2) WRITE(LU,401) LISRUG
400     FORMAT(1X,'WALL_FRICTION : REGIME DE TURBULENCE INCONNU :',1I6)
401     FORMAT(1X,'WALL_FRICTION: UNKNOWN TURBULENCE MODEL : ',1I6)
        CALL PLANTE(1)
        STOP
C
C     *****
      ENDIF
C     *****     
C
C COMPUTES AUBOR
C --------------
C
C  AUBOR COUNTS FOR THE SEGMENT BETWEEN K AND KP1BOR(K)
C
C  LAW        : NUT * DU/DN = UETOIL**2 = -AUBOR*U(N+1)
C  CHANGED TO : NUT * DU/DN = UETOIL**2  *  U(N+1) / U(N)
C                           = UETOIL * (UETOIL/UTANG) * U(N+1)
C
      DO K=1,NPTFR
        IF(LIMPRO(K,5).EQ.KNEU) THEN
          AUBOR(K) = - UTANG * UETUTA(K)**2
        ELSE
          AUBOR(K) = 0.D0
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
C     QUADRATIC ELEMENT HAS EXTRA POINTS ON THE BOUNDARY
C
      IF(IELMU.EQ.13) THEN
        DO K=1,NPTFR
          AUBOR(K+NPTFR) = (AUBOR(K)+AUBOR(KP1BOR(K)))*0.5D0
        ENDDO
      ENDIF
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
