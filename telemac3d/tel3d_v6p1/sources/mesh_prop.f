C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE MESH FOR THE PROPAGATION STEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE BOTTOM OF ZPROP MUST BE DONE BEFORE.

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
!> </td><td> 20/05/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE MESH_PROP
     &(HPROP,HN,H,PROLIN,HAULIN,TETA,NSOUSI,ZPROP,
     & IPBOT,NPOIN2,NPLAN,OPTBAN,SIGMAG,OPT_HNEG,
     & MDIFF,MESH3D,VOLU3D,VOLU3DPAR,UNSV3D,MSK,MASKEL,IELM3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| H              |<--| HAUTEUR
C| HAULIN         |-->| PROFONDEUR MOYENNE POUR LA LINEARISATION
C| HN             |<--| HAUTEUR AU PAS DE TEMPS PRECEDENT
C| HPROP          |<--| HAUTEUR DE PROPAGATION
C| IELM3          |---| 
C| IPBOT          |---| 
C| MASKEL         |---| 
C| MDIFF          |---| 
C| MESH3D         |---| 
C| MSK            |---| 
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| NSOUSI         |-->| NOMBRE DE SOUS ITERATIONS
C| OPTBAN         |---| 
C| OPT_HNEG       |---| 
C| PROLIN         |-->| CORRESPOND AU MOT CLE:"PROPAGATON LINEARISEE"
C| SIGMAG         |---| 
C| TETA           |-->| SEMI-IMPLICITATION SUR H.
C| UNSV3D         |---| 
C| VOLU3D         |---| 
C| ZPROP          |---| 
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
      INTEGER, INTENT(IN)            :: NSOUSI,NPOIN2,NPLAN,OPT_HNEG
      INTEGER, INTENT(IN)            :: OPTBAN,IELM3
      LOGICAL, INTENT(IN)            :: PROLIN,SIGMAG,MSK
      DOUBLE PRECISION, INTENT(IN)   :: TETA,HAULIN
      TYPE(BIEF_OBJ),   INTENT(IN)   :: HN,H,IPBOT,MASKEL
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: HPROP,ZPROP,MDIFF,UNSV3D,VOLU3D
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: VOLU3DPAR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
C
C-----------------------------------------------------------------------
C
      IF(PROLIN) THEN
        CALL OS( 'X=C     ' , X=HPROP , C=HAULIN    )
      ELSEIF(NSOUSI.EQ.1) THEN
        CALL OS( 'X=Y     ' , X=HPROP , Y=HN )
      ELSE
        CALL OS( 'X=CY    ' , X=HPROP , Y=HN , C=1.D0-TETA )
        CALL OS( 'X=X+CY  ' , X=HPROP , Y=H  , C= TETA )
      ENDIF
C
C-----------------------------------------------------------------------
C
C     CLIPS HPROP
C
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.NE.2) THEN
        CALL OS('X=+(Y,C)',X=HPROP,Y=HPROP,C=0.D0)
      ENDIF
C
C     COMPUTES THE NEW MESH
C
      CALL CALCOT(ZPROP%R,HPROP%R)
C
C     COMPUTES THE REAL BOTTOM PLANE
C
      CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN,
     &                  HPROP%R)
C
C     COMPUTES THE INVERSE OF VOLUME OF BASIS FUNCTIONS 3D IN UNSV3D
C
C     ZPROP IS TEMPORARILY PUT IN MESH3D%Z
C
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
C
      IF(OPTBAN.EQ.1.OR.SIGMAG) THEN
C       OBTAINED BY LUMPING MASS-MATRIX (MASS-MATRIX WITH A TREATMENT
C                                        OF TIDAL FLATS)
        CALL MATRIX(MDIFF,'M=N     ','MATMAS          ',
     &              IELM3,IELM3,1.D0,
     &              ZPROP,ZPROP,ZPROP,
     &              ZPROP,ZPROP,ZPROP,MESH3D,MSK,MASKEL)
        CALL LUMP(VOLU3D,MDIFF,MESH3D,1.D0)
      ELSE
C       SIMPLEST WAY
        CALL VECTOR(VOLU3D,'=','MASBAS          ',IELM3,1.D0,ZPROP,
     &              ZPROP,ZPROP,ZPROP,ZPROP,ZPROP,MESH3D,MSK,MASKEL)
      ENDIF
C
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=VOLU3DPAR,Y=VOLU3D)
        CALL PARCOM(VOLU3DPAR,2,MESH3D)
        CALL OS('X=1/Y   ',X=UNSV3D,Y=VOLU3DPAR,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-6)
      ELSE
        CALL OS('X=1/Y   ',X=UNSV3D,Y=VOLU3D,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-6)
      ENDIF
C
C     RESTORES Z
C
      MESH3D%Z%R=>SAVEZ
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
