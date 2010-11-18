C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION COEFFICIENT CF.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CHESTR, GRAV, H, KARMAN, KFROT, MESH, T1, U, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, C, HC, HH, IELMC, IELMH, INLOG, N, NPOIN, TIERS, UNORM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> HH
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), OS(), PLANTE()
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
!> </td><td> 27/07/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT POUR K-EPSILON
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>TABLEAU DES COEFFICIENTS DE FROTTEMENT SUR LE
!>                  FOND.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COEFRO
     &(CF,H,U,V,KARMAN,KFROT,CHESTR,GRAV,MESH,T1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |<--| COEFFICIENT DE FROTTEMENT POUR K-EPSILON
C| CHESTR         |-->| TABLEAU DES COEFFICIENTS DE FROTTEMENT SUR LE
C|                |   | FOND.
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| H             |-->| HAUTEUR D'EAU
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
C| MESH           |---| 
C| T1             |---| 
C| U             |---| 
C| V             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: KFROT
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,KARMAN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: CF,T1
      TYPE(BIEF_OBJ), INTENT(IN)     :: CHESTR,H,U,V
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPOIN,N,IELMC,IELMH
C
      DOUBLE PRECISION TIERS,HC,UNORM,AUX,INLOG,C
      DOUBLE PRECISION, POINTER :: HH(:)
C
      INTRINSIC SQRT,MAX,LOG
C
C-----------------------------------------------------------------------
C
      IELMC = CF%ELM
      IELMH = H%ELM
C
C  DEPTH WITH THE SAME DISCRETISATION AS CF
C  IN CASES WHERE IT IS NEEDED.
C
      IF(KFROT.NE.0.AND.KFROT.NE.2) THEN
C
        IF(IELMC.EQ.IELMH) THEN
          HH=>H%R
        ELSE
          CALL OS( 'X=Y     ' , X=T1 , Y=H )
          CALL CHGDIS( T1 , IELMH , IELMC , MESH )
          HH=T1%R
        ENDIF
C
      ENDIF
C
      NPOIN = CF%DIM1
C
C-----------------------------------------------------------------------
C
      TIERS  = 1.D0/3.D0
C
C  FRICTION COEFFICIENT
C
C     LAWS OF FRICTION:
C
C     KFROT = 0:  NO FRICTION
C     KFROT = 1:  HAALAND
C     KFROT = 2:  CHEZY
C     KFROT = 3:  STRICKLER
C     KFROT = 4:  MANNING
C     KFROT = 5:  NIKURADSE
C
C     *******************
      IF(KFROT.EQ.0) THEN
C     *******************
C
        DO N=1,NPOIN
          CF%R(N) = 0.D0
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.1) THEN
C     ***********************
C
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          UNORM = MAX(SQRT(U%R(N)**2+V%R(N)**2),1.D-6)
C                       1.D-6: LAMINAR VISCOSITY OF THE WATER
          INLOG =(6.9D0*1.D-6/4.D0/HC/UNORM)**3+
     &                                  (CHESTR%R(N)/14.8D0/HC)**3.33
          INLOG = MIN(1.D0-1.D-6,INLOG)
          AUX   = -0.6D0*LOG(INLOG)/LOG(10.D0)
          CF%R(N) = 0.25D0 / AUX**2
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.2) THEN
C     ***********************
C
        DO N=1,NPOIN
          CF%R(N) = 2 * GRAV / CHESTR%R(N)**2
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.3) THEN
C     ***********************
C
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CF%R(N) = 2 * GRAV / CHESTR%R(N)**2 / HC**TIERS
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.4) THEN
C     ***********************
C
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CF%R(N) = 2 * CHESTR%R(N)**2 * GRAV / HC**TIERS
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.5) THEN
C     ***********************
C
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CF%R(N) = 2.D0 / (LOG( 11.D0*HC/CHESTR%R(N))/KARMAN )**2
        ENDDO
C
C     ****
      ELSE
C     ****
C
        IF(LNG.EQ.1) WRITE(LU,300) KFROT
        IF(LNG.EQ.2) WRITE(LU,301) KFROT
300     FORMAT(1X,'COEFRO : LOI DE FROTTEMENT INCONNUE :',1I6)
301     FORMAT(1X,'COEFRO: UNKNOWN LAW OF BOTTOM FRICTION: ',1I6)
        CALL PLANTE(1)
        STOP
C
C     *****
      ENDIF
C     *****
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C