C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VISCOSITIES USING THE SMAGORINSKY MODEL:
!>  @code
!>                                           (1/2)
!>    NUSMAG    =   CS2 * ( 2.0 * SIJ * SIJ )      * (MESH SIZE)**2<br>
!>                         2        2            2
!>                     (DU)     (DV)   (DU   DV)
!>     2*SIJ*SIJ = ( 2*(--) + 2*(--) + (-- + --)
!>                     (DX)     (DY)   (DY   DX)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM3, MASKEL, MESH3, MSK, NUSMAG, T1, T2, T3, T4, U, V
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
!>    </th><td> VECTOR()
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 03/08/00
!> </td><td> C. GUILBAUD SOGREAH  0476334319
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
!>          <tr><td>NUSMAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
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
                        SUBROUTINE SMAGO
     &(U,V,T1,T2,T3,T4,NUSMAG,MESH3,IELM3,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM3          |---| 
C| MASKEL         |---| 
C| MESH3          |---| 
C| MSK            |---| 
C| NUSMAG         |---| 
C| T1             |---| 
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| U             |---| 
C| V             |---| 
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
      INTEGER, INTENT(IN)             :: IELM3
      LOGICAL, INTENT(IN)             :: MSK
      TYPE (BIEF_OBJ), INTENT(IN)     :: U,V
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: T1,T2,T3,T4
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: NUSMAG
      TYPE (BIEF_OBJ), INTENT(IN)     :: MASKEL
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH3
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION CS,CS2
      INTEGER I
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
      CS = 0.1D0
      CS2 = CS**2
C
C-----------------------------------------------------------------------
C
C     COMPUTES GRADIENTS (IN FACT AVERAGED GRADIENT MULTIPLIED BY
C     A SURFACE WHICH IS THE INTEGRAL OF TEST FUNCTIONS ON THE DOMAIN,
C     THIS SURFACE IS CONSIDERED TO BE (MESH SIZE)**2 )
C
      CALL VECTOR(T1,'=','GRADF          X',IELM3,
     &            1.D0,U,U,U,U,U,U,MESH3,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELM3,
     &            1.D0,U,U,U,U,U,U,MESH3,MSK,MASKEL)
      CALL VECTOR(T3,'=','GRADF          X',IELM3,
     &            1.D0,V,V,V,V,V,V,MESH3,MSK,MASKEL)
      CALL VECTOR(T4,'=','GRADF          Y',IELM3,
     &            1.D0,V,V,V,V,V,V,MESH3,MSK,MASKEL)
!
      DO I=1,NUSMAG%DIM1
        NUSMAG%R(I)=CS2*
     &  SQRT(2.D0*(T1%R(I)**2+T4%R(I)**2)+(T2%R(I)+T3%R(I))**2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SMAGO
C
C#######################################################################
C