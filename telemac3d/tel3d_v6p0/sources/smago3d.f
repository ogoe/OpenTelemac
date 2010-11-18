C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES TURBULENT VISCOSITY USING
!>                3D SMAGORINSKI MODEL:
!>  @code
!>                                            (1/2)
!>    NUSMAGO    =   CS2 * ( 2.0 * SIJ * SIJ )      * (MESH SIZE)**2
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM3, MASKEL, MESH3, MSK, SVIDE, TRAV1, TRAV2, TRAV3, TRAV4, TRAV5, TRAV6, U, V, W
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CS, CS2, I
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VISSMA()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 17/02/04
!> </td><td> OLIVER GOETHEL - UNI HANNOVER
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SMAGO3D
     &(U,V,W,TRAV1,TRAV2,TRAV3,TRAV4,TRAV5,TRAV6,
     & SVIDE,MESH3,IELM3,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM3          |---| 
C| MASKEL         |---| 
C| MESH3          |---| 
C| MSK            |---| 
C| SVIDE          |---| 
C| TRAV1          |---| 
C| TRAV2          |---| 
C| TRAV3          |---| 
C| TRAV4          |---| 
C| TRAV5          |---| 
C| TRAV6          |---| 
C| U             |---| 
C| V             |---| 
C| W             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: IELM3
      LOGICAL, INTENT(IN)            :: MSK
      TYPE (BIEF_OBJ), INTENT(IN)    :: U, V, W
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3, TRAV4,TRAV6
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV5
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SVIDE
      TYPE (BIEF_MESH)               :: MESH3
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION CS,CS2

      CS = 0.1D0
      CS2 = CS**2
!
!-----------------------------------------------------------------------
!
C     COMPUTES GRADIENTS (IN FACT AVERAGED GRADIENT MULTIPLIED BY
C     A SURFACE WHICH IS THE INTEGRAL OF TEST FUNCTIONS ON THE DOMAIN,
C     THIS SURFACE IS CONSIDERED TO BE (MESH SIZE)**2) )
!
      CALL VECTOR(TRAV1,'=','GRADF          X',IELM3,
     &            1.D0,U,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV2,'=','GRADF          Y',IELM3,
     &            1.D0,U,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV3,'=','GRADF          Z',IELM3,
     &            1.D0,U,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
!
      CALL VECTOR(TRAV2,'+','GRADF          X',IELM3,
     &            1.D0,V,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV5,'=','GRADF          Y',IELM3,
     &            1.D0,V,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV6,'=','GRADF          Z',IELM3,
     &            1.D0,V,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
!
      CALL VECTOR(TRAV3,'+','GRADF          X',IELM3,
     &            1.D0,W,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV6,'+','GRADF          Y',IELM3,
     &            1.D0,W,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV4,'=','GRADF          Z',IELM3,
     &            1.D0,W,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRAV1,2,MESH3)
        CALL PARCOM(TRAV2,2,MESH3)
        CALL PARCOM(TRAV3,2,MESH3)
        CALL PARCOM(TRAV4,2,MESH3)
        CALL PARCOM(TRAV5,2,MESH3)
        CALL PARCOM(TRAV6,2,MESH3)
      ENDIF
!
      DO I=1,U%DIM1
!
      TRAV5%R(I) = CS2 * SQRT(  2.D0*(TRAV1%R(I)**2
     &                               +TRAV5%R(I)**2
     &                               +TRAV4%R(I)**2)
     &                               +TRAV2%R(I)**2
     &                               +TRAV3%R(I)**2
     &                               +TRAV6%R(I)**2  )
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SMAGO3D
C
C#######################################################################
C