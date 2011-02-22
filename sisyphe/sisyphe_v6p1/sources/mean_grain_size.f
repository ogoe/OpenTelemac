C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GEOMETRIC MEAN GRAIN SIZES OF ACTIVE-LAYER AND UNDER-LAYER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::ACLADM ACLADM@endlink, 
!> @link DECLARATIONS_SISYPHE::AVAIL AVAIL@endlink, 
!> @link DECLARATIONS_SISYPHE::FDM FDM@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::UNLADM UNLADM@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!> </td><td> **/11/2002
!> </td><td> BUI MINH DUC
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ACLADM
!></td><td><--</td><td>MEAN DIAMETER OF THE ACTIVE LAYER
!>    </td></tr>
!>          <tr><td>AVAIL
!></td><td>--></td><td>SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
!>    </td></tr>
!>          <tr><td>AVAIL(NPOIN,NLAYER,NSICLA)
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NLAYER
!></td><td>--></td><td>NUMBER OF LAYER FOR EACH POINT
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF NODES
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>--></td><td>NUMBER OF SIZE-CLASSES OF BED MATERIAL
!>    </td></tr>
!>          <tr><td>UNLADM
!></td><td><--</td><td>MEAN DIAMETER OF THE ACTIVE STRATUM
!>                  = MEAN OF ALL DIFFERENT BED MATERIAL SIZES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE MEAN_GRAIN_SIZE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |<--| MEAN DIAMETER OF THE ACTIVE LAYER
C| AVAIL          |-->| SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
C| AVAIL(NPOIN,NLA|---| 
C| NLAYER         |-->| NUMBER OF LAYER FOR EACH POINT
C| NPOIN          |-->| NUMBER OF NODES
C| NSICLA         |-->| NUMBER OF SIZE-CLASSES OF BED MATERIAL
C| UNLADM         |<--| MEAN DIAMETER OF THE ACTIVE STRATUM
C|                |   | = MEAN OF ALL DIFFERENT BED MATERIAL SIZES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
C
      IMPLICIT NONE
      INTEGER I , J
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
C  UNLADM IS NEEDED FOR HUNZIKER
C
      DO J=1,NPOIN
        ACLADM%R(J) = 0.D0
        UNLADM%R(J) = 0.D0
        IF(NSICLA.GT.1) THEN
         DO I=1,NSICLA
          IF(AVAIL(J,1,I).GT.0.D0) THEN
            ACLADM%R(J) = ACLADM%R(J) + FDM(I)*AVAIL(J,1,I)
            UNLADM%R(J) = UNLADM%R(J) + FDM(I)*AVAIL(J,2,I)
          ENDIF
         ENDDO
        ENDIF
        IF(ACLADM%R(J).LE.0.D0) ACLADM%R(J) = FDM(1)
        IF(UNLADM%R(J).LE.0.D0) UNLADM%R(J) = ACLADM%R(J)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C