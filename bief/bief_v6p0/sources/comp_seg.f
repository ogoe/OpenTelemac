C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLETES THE DATA STRUCTURE FOR EDGE-BASED STORAGE
!>                FOR HIGHER ORDER ELEMENTS.
!>  @code
!>    NUMBERING OF QUADRATI!> ELEMENTS SEGMENTS:
!>
!>    01 --> 1 - 2
!>    02 --> 2 - 3
!>    03 --> 3 - 1
!>    04 --> 1 - 4
!>    05 --> 2 - 5
!>    06 --> 3 - 6
!>    07 --> 2 - 4
!>    08 --> 3 - 5
!>    09 --> 1 - 6
!>    10 --> 1 - 5
!>    11 --> 2 - 6
!>    12 --> 3 - 4
!>    13 --> 4 - 5
!>    14 --> 5 - 6
!>    15 --> 6 - 4
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, GLOSEG, IELM, IKLE, MAXSEG, NELEM, NELMAX, NSEG, ORISEG
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IAD, IELEM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_COMP_SEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 05/02/2010
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
!></td><td><--</td><td>SEGMENTS OF EVERY TRIANGLE.
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td><--</td><td>GLOBAL NUMBERS OF POINTS OF SEGMENTS.
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>11: TRIANGLES.
!>                  21: QUADRILATERES.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>MAXSEG
!></td><td><--</td><td>1st DIMENSION OF MAXSEG.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!>                  (CAS DES MAILLAGES ADAPTATIFS)
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td><--</td><td>NUMBER OF SEGMENTS OF THE MESH.
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COMP_SEG
     &(NELEM,NELMAX,IELM,IKLE,GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
C| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
C| IELM           |-->| 11: TRIANGLES.
C|                |   | 21: QUADRILATERES.
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| MAXSEG         |<--| 1st DIMENSION OF MAXSEG.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C|                |   | (CAS DES MAILLAGES ADAPTATIFS)
C| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
C| ORISEG         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_COMP_SEG => COMP_SEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELMAX,NSEG,MAXSEG,IELM,NELEM
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,*)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2),ELTSEG(NELMAX,*)
      INTEGER, INTENT(INOUT) :: ORISEG(NELMAX,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IAD
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.12) THEN
C
C       3 ADDITIONAL SEGMENTS WITHIN QUASI-BUBBLE ELEMENTS
C       FOR THEM ORISEG IS IMPLICITLY 1 AND IS NEVER USED
C
        DO IELEM = 1 , NELEM
          ELTSEG(IELEM,4) = NSEG + 3*(IELEM-1) + 1
          ELTSEG(IELEM,5) = NSEG + 3*(IELEM-1) + 2
          ELTSEG(IELEM,6) = NSEG + 3*(IELEM-1) + 3
C         PRINCIPLE: FROM LINEAR POINT TO QUASI-BUBBLE POINT
          GLOSEG(ELTSEG(IELEM,4),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,4),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,5),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,5),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,6),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,6),2) = IKLE(IELEM,4)
        ENDDO
C
      ELSEIF(IELM.EQ.13) THEN
C
C       12 ADDITIONAL SEGMENTS IN QUADRATIC ELEMENTS
C       SEE GLOSEG BELOW
C
        DO IELEM = 1 , NELEM
C         6 SMALL LATERAL SEGMENTS (NUMBERED LIKE THEIR LARGER
C         SEGMENT WITH A SHIFT, AND SO THAT NUMBERS CORRESPOND
C         ON EITHER SIDE)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            ELTSEG(IELEM,04)=NSEG+ELTSEG(IELEM,01)
            ELTSEG(IELEM,07)=NSEG+ELTSEG(IELEM,01)+NSEG
          ELSE
            ELTSEG(IELEM,04)=NSEG+ELTSEG(IELEM,01)+NSEG
            ELTSEG(IELEM,07)=NSEG+ELTSEG(IELEM,01)
          ENDIF
          IF(ORISEG(IELEM,2).EQ.1) THEN
            ELTSEG(IELEM,05)=NSEG+ELTSEG(IELEM,02)
            ELTSEG(IELEM,08)=NSEG+ELTSEG(IELEM,02)+NSEG
          ELSE
            ELTSEG(IELEM,05)=NSEG+ELTSEG(IELEM,02)+NSEG
            ELTSEG(IELEM,08)=NSEG+ELTSEG(IELEM,02)
          ENDIF
          IF(ORISEG(IELEM,3).EQ.1) THEN
            ELTSEG(IELEM,06)=NSEG+ELTSEG(IELEM,03)
            ELTSEG(IELEM,09)=NSEG+ELTSEG(IELEM,03)+NSEG
          ELSE
            ELTSEG(IELEM,06)=NSEG+ELTSEG(IELEM,03)+NSEG
            ELTSEG(IELEM,09)=NSEG+ELTSEG(IELEM,03)
          ENDIF
        ENDDO
        IAD=3*NSEG
        DO IELEM = 1 , NELEM
C         THE 3 LARGE SEGMENTS INSIDE THE ELEMENT
          ELTSEG(IELEM,10) = IAD + 3*(IELEM-1) + 1
          ELTSEG(IELEM,11) = IAD + 3*(IELEM-1) + 2
          ELTSEG(IELEM,12) = IAD + 3*(IELEM-1) + 3
        ENDDO
        IAD=IAD+3*NELEM
        DO IELEM = 1 , NELEM
C         THE 3 SMALL SEGMENTS INSIDE THE ELEMENT
          ELTSEG(IELEM,13) = IAD + 3*(IELEM-1) + 1
          ELTSEG(IELEM,14) = IAD + 3*(IELEM-1) + 2
          ELTSEG(IELEM,15) = IAD + 3*(IELEM-1) + 3
        ENDDO
        IAD=IAD+3*NELEM
C
        IF(IAD.NE.MAXSEG) THEN
          WRITE(LU,*) 'COMP_SEG: ERROR ON MAXIMUM NUMBER OF SEGMENTS'
          CALL PLANTE(1)
          STOP
        ENDIF
C
        DO IELEM = 1 , NELEM
C         FOR SEGMENTS 4 TO 12: FROM LINEAR POINT TO QUADRATIC POINT
C         THIS IS IMPORTANT FOR RECTANGULAR MATRICES AND MVSEG
          GLOSEG(ELTSEG(IELEM,04),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,04),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,05),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,05),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,06),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,06),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,07),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,07),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,08),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,08),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,09),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,09),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,10),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,10),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,11),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,11),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,12),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,12),2) = IKLE(IELEM,4)
C         FOR SEGMENTS 13 TO 15: NO SPECIFIC PRINCIPLE
          GLOSEG(ELTSEG(IELEM,13),1) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,13),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,14),1) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,14),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,15),1) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,15),2) = IKLE(IELEM,4)
C         SHOULD NOT BE USEFUL (MEMORY TO BE REMOVED ?)
C         ORIENTATION FROM LINEAR TO QUADRATIC NEEDS NO
C         EXTRA INFORMATION
          ORISEG(IELEM,04) = 1
          ORISEG(IELEM,05) = 1
          ORISEG(IELEM,06) = 1
          ORISEG(IELEM,07) = 1
          ORISEG(IELEM,08) = 1
          ORISEG(IELEM,09) = 1
          ORISEG(IELEM,10) = 1
          ORISEG(IELEM,11) = 1
          ORISEG(IELEM,12) = 1
          ORISEG(IELEM,13) = 1
          ORISEG(IELEM,14) = 1
          ORISEG(IELEM,15) = 1
        ENDDO
      ELSE
        IF (LNG.EQ.1) WRITE(LU,500) IELM
        IF (LNG.EQ.2) WRITE(LU,501) IELM
500     FORMAT(1X,'COMP_SEG (BIEF) : ELEMENT NON PREVU : ',1I6)
501     FORMAT(1X,'COMP_SEG (BIEF): UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C