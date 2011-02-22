C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 29/08/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/06/2001
!> </td><td> CDG/SOGREAH
!> </td><td> TRACER SOURCES
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE SOURCE
     & (S0U,S0V,S0W,S1U,S1V,S1W,
     &  UN3,VN3,WSN3,WN3,
     &  VOLU,VOLUN,T3,NPOIN3,NTRAC,LT,AT,DT,PRIVE,NONHYD,
     &  NPOIN2,NSCE,ISCE,KSCE,QSCE,USCE,VSCE,MAXSCE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| DT             |-->| TIME STEP
C| ISCE           |---| 
C| KSCE           |---| 
C| LT             |-->| ITERATION NUMBER
C| MAXSCE         |---| 
C| NONHYD         |---| 
C| NPOIN2         |---| 
C| NPOIN3         |-->| NUMBER OF POINTS IN THE MESH
C| NPRIV          |-->| NUMBER OF ARRAYS IN BLOCK PRIVE
C| NSCE           |---| 
C| NTRAC          |-->| NUMBER OF TRACERS
C| PRIVE          |-->| BLOCK OF ARRAYS FOR USER
C| QSCE           |---| 
C| S0U,S0V        |<--| EXPLICIT SOURCE TERMS ON VELOCITIES U AND V
C| S0W            |---| 
C| S1U,S1V        |<--| IMPLICIT SOURCE TERMS ON VELOCITIES U AND V
C| S1W            |---| 
C| T3             |---| 
C| UN3,VN3,WSN3   |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
C| USCE           |---| 
C| VOLU           |---| 
C| VOLUN          |---| 
C| VSCE           |---| 
C| WN3            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NPOIN3, NTRAC, LT, MAXSCE
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: UN3, VN3, WSN3, WN3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: S0U, S0V, S1U, S1V, S0W, S1W
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T3
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU, VOLUN,PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: AT,DT
      LOGICAL, INTENT(IN)           :: NONHYD
!
      INTEGER, INTENT(IN)           :: NPOIN2
      INTEGER, INTENT(IN)           ::           NSCE
      INTEGER, INTENT(IN)           ::           ISCE(NSCE)
      INTEGER, INTENT(IN)           ::           KSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  QSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  USCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  VSCE(NSCE)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC, IS
!
!-----------------------------------------------------------------------
!
!     BEWARE : BE SURE TO DO S0U = S0U + YOUR SOURCE TERMS
!              BECAUSE S0U HAS ALREADY BEEN INITIALISED IN TRISOU
!
!
!     INITIALISES OTHER SOURCE TERMS
!
      S1U%TYPR='0'
      S1V%TYPR='0'
      IF(NONHYD) THEN
        S0W%TYPR='0'
        S1W%TYPR='0'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
