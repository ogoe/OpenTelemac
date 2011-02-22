C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE QUADRATIC FRICTION COEFFICIENT CF.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CHESTR, GRAV, H, HMIN, KARMAN, KFROT, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, HC, N, TIERS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TOB_SISYPHE()

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
!>      <td><center> 5.4                                       </center>
!> </td><td> 01/10/2003
!> </td><td> C. VILLARET (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>COEFFICIENTS DE FROTTEMENT SUR LE  FOND.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>--></td><td>HAUTEUR D'EAU MINIMALE
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE COEFRO_SISYPHE
     &(CF,H,KFROT,CHESTR,GRAV,NPOIN,HMIN,KARMAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |---| 
C| CHESTR         |-->| COEFFICIENTS DE FROTTEMENT SUR LE  FOND.
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| H             |-->| HAUTEUR D'EAU
C| HMIN           |-->| HAUTEUR D'EAU MINIMALE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
C| NPOIN          |-->| NOMBRE DE POINTS
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
      INTEGER, INTENT(IN):: NPOIN,KFROT
      DOUBLE PRECISION,INTENT(IN):: GRAV,KARMAN,HMIN
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CF
      TYPE(BIEF_OBJ),INTENT(IN) :: CHESTR,H
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N
      DOUBLE PRECISION HC, AUX, TIERS,ZERO
      INTRINSIC MAX,LOG
C
C-----------------------------------------------------------------------
C
      TIERS  = 1.D0/3.D0
      ZERO = 1.D-6
C
C  CONSTRUCTION OF THE FRICTION COEFFICIENT
C
C     FRICTION LAWS:
C
C     KFROT = 0 :  FLAT BOTTOM  (KS=3D50)
C     KFROT = 1 :  EQUILIBRIUM SAND RIPPLES (WAVES ONLY) KS=(MAX 3D50,ETA)
C     KFROT = 2 :  CHEZY
C     KFROT = 3 :  STRICKLER
C     KFROT = 4 :  MANNING
C     KFROT = 5 :  NIKURADSE
C
      DO N=1,NPOIN
        IF(CHESTR%R(N).LE.0.D0) THEN
          WRITE(LU,*) 'FROTTEMENT NON DEFINI DANS COEFRO AU POINT ',N
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
C
C     ***********************
      IF(KFROT.EQ.5) THEN 
C    ***********************
C          AUX=30.D0/EXP(1.D0) =11.036D0
        DO N=1,NPOIN
            AUX = MAX(1.001D0,H%R(N)*11.036D0/CHESTR%R(N))
            CF%R(N) = 2.D0 / (LOG( AUX)/KARMAN )**2
        ENDDO
C     ***********************
      ELSEIF(KFROT.EQ.2) THEN
C     ***********************
C
        DO N=1,NPOIN
           CF%R(N) = 2.D0 * GRAV / CHESTR%R(N)**2
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.3) THEN
C     ***********************
C
        DO N=1,NPOIN
           HC = MAX(H%R(N),HMIN)
           CF%R(N) = 2.D0 * GRAV / CHESTR%R(N)**2 / HC**TIERS
        ENDDO
C
C     ***********************
      ELSEIF(KFROT.EQ.4) THEN
C     ***********************
C
        DO N=1,NPOIN
           HC = MAX(H%R(N),HMIN)
           CF%R(N) = 2.D0 * CHESTR%R(N)**2 * GRAV / HC**TIERS
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
