C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE VERTICAL PROFILE FOR TRACERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ENTET, I, IOPT, IPLAN, IPOIN2, ITRAC, LT, TIME
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::CREF CREF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DMOY DMOY@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DNUTAV DNUTAV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KARMAN KARMAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KSPRATIO KSPRATIO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEDI SEDI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UETCAR UETCAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WCHU WCHU@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, DELTAZ, DENOM, HH, IPOIN3, KS, ROUSE, USTAR, Y0, ZREFE
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
!>      <td><center> 5.8                                       </center>
!> </td><td> 12/09/07
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
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I
!></td><td>--></td><td>NUMBER OF THE LIQUID BOUNDARY.
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF YES, LISTING PRINTOUTS ALLOWED
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>--></td><td>OPTION : 0 : USER DEFINED
!>                  2 : ROUSE PROFILE FOR SEDIMENT
!>                  3 : MODIFIED ROUSE PROFILE (VISCOSITY)
!>    </td></tr>
!>          <tr><td>IPLAN
!></td><td>--></td><td>NUMERO DU PLAN
!>    </td></tr>
!>          <tr><td>IPOIN2
!></td><td>--></td><td>2D GLOBAL NUMBER OF POINT CONSIDERED
!>    </td></tr>
!>          <tr><td>ITRAC
!></td><td>--></td><td>TRACER NUMBER
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION NUMBER
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF TRACERS
!>    </td></tr>
!>          <tr><td>SEDI
!></td><td>--></td><td>IF YES, TRACER NTRAC IS SEDIMENT
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION TRA_PROF_Z
     &( I , IPOIN2 , TIME , LT , IPLAN , ENTET , IOPT , ITRAC )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| ENTET          |---| 
C| I             |-->| NUMBER OF THE LIQUID BOUNDARY.
C| INFOGR         |-->| IF YES, LISTING PRINTOUTS ALLOWED
C| IOPT           |-->| OPTION : 0 : USER DEFINED
C|                |   | 2 : ROUSE PROFILE FOR SEDIMENT
C|                |   | 3 : MODIFIED ROUSE PROFILE (VISCOSITY)
C| IPLAN          |-->| NUMERO DU PLAN
C| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
C| ITRAC          |-->| TRACER NUMBER
C| LT             |-->| ITERATION NUMBER
C| NTRAC          |-->| NUMBER OF TRACERS
C| SEDI           |-->| IF YES, TRACER NTRAC IS SEDIMENT
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
      INTEGER          , INTENT(IN) :: ITRAC
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION USTAR,Y0,HH,DENOM,AUX,KS,DELTAZ,ROUSE,ZREFE
      INTEGER IPOIN3
C
C-----------------------------------------------------------------------
C
      IF(IOPT.EQ.0) THEN
C
        WRITE(LU,*) 'USER DEFINE PROFILE MISSING IN TRA_PROF_Z'
        CALL PLANTE(1)
        STOP
C
      ELSEIF(IOPT.EQ.2) THEN
C
C       NOT SEDIMENT : SO FAR PROFILE = 1.D0
C
        IF(ITRAC.NE.NTRAC.OR..NOT.SEDI) THEN
          TRA_PROF_Z=1.D0
        ELSE
C
C       HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
C
          IF(IPLAN.EQ.1) THEN
            TRA_PROF_Z= CREF%R(IPOIN2)
          ELSE
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
            ROUSE=-WCHU%R(IPOIN2)/KARMAN/USTAR
            ZREFE=KSPRATIO*DMOY%R(IPOIN2)
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &             -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
            DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
            TRA_PROF_Z=(ZREFE/(HH-ZREFE)*(HH-DELTAZ)/DELTAZ)**ROUSE
            TRA_PROF_Z=CREF%R(IPOIN2)*TRA_PROF_Z
          ENDIF
C
        ENDIF
C
      ELSEIF(IOPT.EQ.3) THEN
C
C       NOT SEDIMENT : SO FAR PROFILE = 1.D0
C
        IF(ITRAC.NE.NTRAC.OR..NOT.SEDI) THEN
          TRA_PROF_Z=1.D0
        ELSE
C
C       HERE VALID ONLY FOR SEDIMENT :
C       MODIFIED ROUSE PROFILE FOR LAMINAR VISCOSITY
C
          IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
          USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
          ROUSE=-WCHU%R(IPOIN2)/KARMAN/USTAR
          HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
          DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
          TRA_PROF_Z=((HH-DELTAZ)/(DELTAZ+DNUTAV/KARMAN/USTAR))**ROUSE
          TRA_PROF_Z= CREF%R(IPOIN2)*TRA_PROF_Z
     &     *(DNUTAV/KARMAN/USTAR/HH)**ROUSE
C
C         NOTE: CREF%R(IPOIN2)*(DNUTAV/KARMAN/USTAR/HH)**ROUSE
C               IS C AT MID-DEPTH
C
        ENDIF
C
C      ELSEIF(IOPT.EQ.4) THEN
C
C
       ELSE

        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TRA_PROF_Z : OPTION INCONNUE POUR LE PROFIL'
          WRITE(LU,*) 'IOPT=',IOPT,' 0 ET 2 POSSIBLES SEULEMENT'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TRA_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
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