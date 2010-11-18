C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALLED AFTER MASKEL HAS BEEN FILLED IN.
!><br>          - FILLS IN MASKPT (A POINT IS MASKED IF SURROUNDED BY
!>                MASKED ELEMENTS).
!><br>          - FILLS IN IFAMAS (SIMILAR ARRAY TO IFABOR, BUT
!>                CONSIDERS ANY FACE SEPARATING A MASKED FROM A
!>                NON-MASKED ELEMENT SUCH AS A SOLID BOUNDARY).
!><br>          - COMPUTES THE NEW COMPATIBLE NORMALS XNEBOR AND
!>                YNEBOR FOR PRISMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, IELM, IFABOR, IFAMAS, IKLE, MASKEL, MASKPT, MESH, NELEM, NPOIN, NSEG
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
!>    </th><td> I1, I2, I3, IELEM, N, WSEG
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MASKTO, WSEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), PARCOM2_SEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MASK3D(), SISYPHE(), TELEMAC2D()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 21/10/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMERO DES ELEMENTS VOISINS SANS MASQUE.
!>    </td></tr>
!>          <tr><td>IFAMAS
!></td><td><--</td><td>NUMERO DES ELEMENTS VOISINS AVEC MASQUE.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE.
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td><--</td><td>TABLEAU DE MASQUAGE DES POINTS
!>                  =1. : NORMAL   =0. : POINT MASQUE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MASKTO
     &(MASKEL,MASKPT,IFAMAS,IKLE,IFABOR,ELTSEG,NSEG,
     & NELEM,NPOIN,IELM,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |---| 
C| IELM           |-->| TYPE D'ELEMENTS.
C| IFABOR         |-->| NUMERO DES ELEMENTS VOISINS SANS MASQUE.
C| IFAMAS         |<--| NUMERO DES ELEMENTS VOISINS AVEC MASQUE.
C| IKLE           |-->| TABLE DE CONNECTIVITE.
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| MASKPT         |<--| TABLEAU DE MASQUAGE DES POINTS
C|                |   | =1. : NORMAL   =0. : POINT MASQUE.
C| MESH           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NPOIN          |-->| NOMBRE DE POINTS.
C| NSEG           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MASKTO => MASKTO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER, INTENT(IN)    :: NELEM,NPOIN,IELM,NSEG
      INTEGER, INTENT(IN)    :: IKLE(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(IN)    :: ELTSEG(NELEM,3)
      INTEGER, INTENT(INOUT) :: IFAMAS(NELEM,3)
C
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MASKPT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER IELEM,N,I1,I2,I3
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: WSEG
C
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
      WSEG => MESH%MSEG%X%R
C
      CALL OS('X=0     ',X=MASKPT)
C
      IF(IELM.EQ.11.OR.IELM.EQ.41) THEN
C
C 1) MASKS THE POINTS WHICH DO NOT BELONG TO NON-FROZEN ELEMENTS
C    (THOSE WHICH BELONG TO A 'NORMAL' ELEMENT ARE SET BACK TO 1)
C
        DO IELEM = 1,NELEM
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          MASKPT%R(I1) = MAX(MASKPT%R(I1),MASKEL(IELEM))
          MASKPT%R(I2) = MAX(MASKPT%R(I2),MASKEL(IELEM))
          MASKPT%R(I3) = MAX(MASKPT%R(I3),MASKEL(IELEM))
        ENDDO
C
C       IN PARALLEL MODE FOR INTERFACE POINTS, MAXIMUM RETAINED
C
        IF(NCSIZE.GT.1) CALL PARCOM(MASKPT,3,MESH)
C
C 2) COPIES IFABOR IN IFAMAS
C
        DO IELEM = 1,NELEM
          IFAMAS(IELEM,1) = IFABOR(IELEM,1)
          IFAMAS(IELEM,2) = IFABOR(IELEM,2)
          IFAMAS(IELEM,3) = IFABOR(IELEM,3)
        ENDDO
C
C 3) IDENTIFIES THE EDGES OF FROZEN ELEMENTS WITH 0 (LIQUID BOUNDARY)
C    TO STOP THE CHARACTERISTIC CURVES
C
C    USES AN ARRAY DEFINED BY SEGMENT TO COMMUNICATE IN PARALLEL MODE
C
C       WSEG SET TO 1
C
        DO N=1,NSEG
          WSEG(N)=1.D0
        ENDDO
C
C       THEN WSEG PUT TO 0 FOR DRY ELEMENTS
C
        DO IELEM=1,NELEM
          IF(MASKEL(IELEM).LT.0.5D0) THEN
            WSEG(ELTSEG(IELEM,1))=0.D0
            WSEG(ELTSEG(IELEM,2))=0.D0
            WSEG(ELTSEG(IELEM,3))=0.D0
          ENDIF
        ENDDO
C
C       IN PARALLEL MODE FOR INTERFACE EDGES, MINIMUM RETAINED
C
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM2_SEG(WSEG,WSEG,WSEG,NSEG,1,4,1,MESH,1)
        ENDIF
C
C       WSEG = 0.D0 TRANSLATED INTO IFAMAS = 0
C
        DO IELEM=1,NELEM
          IF(WSEG(ELTSEG(IELEM,1)).LT.0.5D0) IFAMAS(IELEM,1)=0
          IF(WSEG(ELTSEG(IELEM,2)).LT.0.5D0) IFAMAS(IELEM,2)=0
          IF(WSEG(ELTSEG(IELEM,3)).LT.0.5D0) IFAMAS(IELEM,3)=0
        ENDDO
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,1000) IELM
        IF(LNG.EQ.2) WRITE(LU,1100) IELM
1000    FORMAT(1X,'MASKTO: TYPE D''ELEMENT INCONNU :',1I6)
1100    FORMAT(1X,'MASKTO: UNKNOWN TYPE OF ELEMENT :',1I6)
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