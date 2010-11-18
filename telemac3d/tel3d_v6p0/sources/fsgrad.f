C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FREE SURFACE GRADIENT, TAKING INTO
!>                ACCOUNT THE TREATMENT OF TIDAL FLATS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GRADZS, IELM2H, MASKEL, MESH2D, MSK, NPOIN2, OPTBAN, S, T2_01, UNSV2D, Z, ZF, ZFLATS
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
!>    </th><td> SAVE_T2_01, ZZF
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> SAVE_T2_01
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), CRSL11(), OS(), PARCOM(), VECTOR()
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
!> </td><td> 28/07/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GRADZS
!></td><td><-></td><td>FREE SURFACE GRADIENT (BLOCK OF 2 COMPONENTS)
!>    </td></tr>
!>          <tr><td>IELM2H
!></td><td>--></td><td>TYPE OF ELEMENT
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>ARRAY OF MASKS, PER ELEMENT
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td><-></td><td>MESH
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>IF YES, THERE IS MASKING, MASKEL IS TO BE USED
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>--></td><td>OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!>                  MODIFIED AND PIECE-WISE LINEAR
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>EMPTY BIEF_OBJ STRUCTURE
!>    </td></tr>
!>          <tr><td>T2_01
!></td><td><-></td><td>BIEF_OBJ STRUCTURE FOR LOCAL WORK
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>--></td><td>INVERSE OF INTEGRAL OF BASES
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>Z COORDINATES OF THE 3D MESH
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFLATS
!></td><td><-></td><td>PIECE-WISE LINEAR FREE SURFACE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FSGRAD
     &(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,UNSV2D,T2_01,
     & NPOIN2,OPTBAN,S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GRADZS         |<->| FREE SURFACE GRADIENT (BLOCK OF 2 COMPONENTS)
C| IELM2H         |-->| TYPE OF ELEMENT
C| MASKEL         |-->| ARRAY OF MASKS, PER ELEMENT
C| MESH2D         |<->| MESH
C| MSK            |-->| IF YES, THERE IS MASKING, MASKEL IS TO BE USED
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
C|                |   | MODIFIED AND PIECE-WISE LINEAR
C| S             |-->| EMPTY BIEF_OBJ STRUCTURE
C| T2_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
C| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASES
C| Z             |-->| Z COORDINATES OF THE 3D MESH
C| ZF             |---| 
C| ZFLATS         |<->| PIECE-WISE LINEAR FREE SURFACE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)                  :: IELM2H,NPOIN2,OPTBAN
      DOUBLE PRECISION, TARGET, INTENT(IN) :: Z(NPOIN2)
      LOGICAL, INTENT(IN)                  :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)        :: GRADZS,ZFLATS,T2_01
      TYPE(BIEF_OBJ), INTENT(IN)           :: ZF,UNSV2D,S,MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT)       :: MESH2D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_T2_01,ZZF
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1) THEN
!
C       COMPUTES THE FREE SURFACE GRADIENT AS IN TELEMAC-2D
!
        CALL CRSL11(ZFLATS%R,Z,
     &              ZF%R,MESH2D%IKLE%I,MESH2D%NELEM,MESH2D%NELMAX)
        CALL VECTOR(GRADZS%ADR(1)%P,'=','GRADF          X',IELM2H,
     &              1.D0,ZFLATS,S,S,S,S,S,MESH2D,MSK,MASKEL)
        CALL VECTOR(GRADZS%ADR(2)%P,'=','GRADF          Y',IELM2H,
     &              1.D0,ZFLATS,S,S,S,S,S,MESH2D,MSK,MASKEL)
!
      ELSE
!
        SAVE_T2_01=>T2_01%R
        T2_01%R   =>Z
!
        CALL CPSTVC(ZF,T2_01)
C       THIS COPY IS REPLACED WITH T2_01%R POINTING TO Z
C       CALL OV('X=Y     ',T2_01%R,Z,T2_01%R,0.D0,NPOIN2)
        CALL VECTOR(GRADZS%ADR(1)%P,'=','GRADF          X',IELM2H,
     &              1.D0,T2_01,S,S,S,S,S,MESH2D,MSK,MASKEL)
        CALL VECTOR(GRADZS%ADR(2)%P,'=','GRADF          Y',IELM2H,
     &              1.D0,T2_01,S,S,S,S,S,MESH2D,MSK,MASKEL)
!
        T2_01%R=>SAVE_T2_01
!
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(GRADZS%ADR(1)%P,2,MESH2D)
        CALL PARCOM(GRADZS%ADR(2)%P,2,MESH2D)
      ENDIF
!
C     DIVISION BY INTEGRAL OF 2D BASES
!
      CALL OS('X=XY    ',X=GRADZS%ADR(1)%P,Y=UNSV2D)
      CALL OS('X=XY    ',X=GRADZS%ADR(2)%P,Y=UNSV2D)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C