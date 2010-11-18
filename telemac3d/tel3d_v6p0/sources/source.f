C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DT, ISCE, KSCE, LT, MAXSCE, NONHYD, NPOIN2, NPOIN3, NSCE, NTRAC, PRIVE, QSCE, S0U, S0V, S0W, S1U, S1V, S1W, T3, UN3, USCE, VN3, VOLU, VOLUN, VSCE, WN3, WSN3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IS, ITRAC
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

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME STEP
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION NUMBER
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NONHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NUMBER OF ARRAYS IN BLOCK PRIVE
!>    </td></tr>
!>          <tr><td>NSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF TRACERS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>BLOCK OF ARRAYS FOR USER
!>    </td></tr>
!>          <tr><td>QSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S0U,S0V
!></td><td><--</td><td>EXPLICIT SOURCE TERMS ON VELOCITIES U AND V
!>    </td></tr>
!>          <tr><td>S0W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S1U,S1V
!></td><td><--</td><td>IMPLICIT SOURCE TERMS ON VELOCITIES U AND V
!>    </td></tr>
!>          <tr><td>S1W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UN3,VN3,WSN3
!></td><td>--></td><td>COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!>    </td></tr>
!>          <tr><td>USCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WN3
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
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
!
C     BEWARE : BE SURE TO DO S0U = S0U + YOUR SOURCE TERMS
C              BECAUSE S0U HAS ALREADY BEEN INITIALISED IN TRISOU
!
C     CALL OS ( 'X=C     ' , S0U , S0U , S0U , 0.D0 )
C     CALL OS ( 'X=C     ' , S0V , S0U , S0U , 0.D0 )
!
C     INITIALISES OTHER SOURCE TERMS
!
C     S1U AND S1V ARE THEN USED FOR TIDAL FLATS
!
C     CALL OS ( 'X=0     ' , X=S1U )
C     CALL OS ( 'X=0     ' , X=S1V )
      S1U%TYPR='0'
      S1V%TYPR='0'
      IF(NONHYD) THEN
        S0W%TYPR='0'
        S1W%TYPR='0'
C       CALL OS ( 'X=0     ' , X=S0W )
C       CALL OS ( 'X=0     ' , X=S1W )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C