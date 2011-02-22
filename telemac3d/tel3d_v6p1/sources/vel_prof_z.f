C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE VELOCITY VERTICAL PROFILE AT ENTRANCES.
!><br>            THIS PROFILE IS LOGARITHMIC AND DESIGNED SO THAT THE
!>                INTEGRAL ON THE VERTICAL EQUALS THE DEPTH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ENTET, I, IOPT, IPLAN, IPOIN2, LT, TIME
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::CF CF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KARMAN KARMAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, DELTAZ, DENOM, DZ, HH, KS, USTAR, Y0
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD3D()

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
!> </td><td> 01/09/06
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>--></td><td>IF YES, LISTING PRINTOUTS ALLOWED
!>    </td></tr>
!>          <tr><td>I
!></td><td>--></td><td>NUMBER OF THE LIQUID BOUNDARY.
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IPLAN
!></td><td>--></td><td>NUMERO DU PLAN
!>    </td></tr>
!>          <tr><td>IPOIN2
!></td><td>--></td><td>2D GLOBAL NUMBER OF POINT CONSIDERED
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION NUMBER
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION VEL_PROF_Z
     &( I , IPOIN2 , TIME , LT , IPLAN , ENTET , IOPT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| ENTET          |-->| IF YES, LISTING PRINTOUTS ALLOWED
C| I             |-->| NUMBER OF THE LIQUID BOUNDARY.
C| IOPT           |---| 
C| IPLAN          |-->| NUMERO DU PLAN
C| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
C| LT             |-->| ITERATION NUMBER
C| TIME           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          , INTENT(IN) :: I,IPOIN2,IPLAN,IOPT,LT
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION USTAR,Y0,HH,DENOM,AUX,KS,DELTAZ,DZ
C
C-----------------------------------------------------------------------
C
      IF(IOPT.EQ.0) THEN
C
        WRITE(LU,*) 'USER DEFINE PROFILE MISSING IN VEL_PROF_Z'
        CALL PLANTE(1)
        STOP
C
      ELSEIF(IOPT.EQ.2) THEN
C
C       FROM FRICTION COEFFICENT CF TO KS (NIKURADSE LAW...)
C
        AUX=KARMAN*SQRT(2.D0/CF%R(IPOIN2))
        HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &         -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
C
C : Y0 = KS/30
C
        Y0=HH/EXP(1.D0+AUX)
C   EXP(1)= 2.71828182845D0
        DENOM=MAX(LOG(HH/Y0/2.71828182845D0),1.D-4)
C
        IF(IPLAN.EQ.1) THEN
C
C         VELOCITY AT THE BOTTOM IS TAKEN AT A HEIGHT IN THE
C         LOGARITHMIC PROFILE THAT ENSURES THAT THE FLUX OF THE
C         FIRST LAYER WILL BE CORRECT IF COMPUTED WITH A LINEAR
C         INTERPOLATION
C         NOTE: THE CRITERION OF STRESS INSTEAD OF FLUX WOULD
C         ALSO LEAD TO A DIVISION BY E**2
C
          DZ=(MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2))
          AUX=2.D0
          DELTAZ=DZ/EXP(AUX)
        ELSE
          DELTAZ=MESH3D%Z%R(IPOIN2+(IPLAN-1)*NPOIN2)-MESH3D%Z%R(IPOIN2)
        ENDIF
        DELTAZ=MAX(DELTAZ,Y0)
        VEL_PROF_Z=LOG(DELTAZ/Y0)/DENOM
C
C     ELSEIF(IOPT.EQ.3) THEN
C
C
      ELSE
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'VEL_PROF_Z : OPTION INCONNUE POUR LE PROFIL'
          WRITE(LU,*) 'IOPT=',IOPT,' 0 ET 2 POSSIBLES SEULEMENT'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'VEL_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
          WRITE(LU,*) 'IOPT=',IOPT,' 0 AND 2 ONLY ARE POSSIBLE'
        ENDIF
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C