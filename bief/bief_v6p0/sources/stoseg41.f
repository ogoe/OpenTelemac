C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE
!>                IN PRISMS.
!>  @code
!>       LOCAL NUMBERING OF SEGMENTS CHOSEN HERE IN A PRISM
!>
!>       HORIZONTAL
!>
!>       01 : POINT 1 TO 2 (OR THE OPPOSITE DEPENDING OF ORISEG)
!>       02 : POINT 2 TO 3 (OR THE OPPOSITE DEPENDING OF ORISEG)
!>       03 : POINT 3 TO 1 (OR THE OPPOSITE DEPENDING OF ORISEG)
!>       04 : POINT 4 TO 5 (OR THE OPPOSITE DEPENDING OF ORISEG)
!>       05 : POINT 5 TO 6 (OR THE OPPOSITE DEPENDING OF ORISEG)
!>       06 : POINT 6 TO 4 (OR THE OPPOSITE DEPENDING OF ORISEG)
!>
!>       VERTICAL
!>
!>       07 : POINT 1 TO 4
!>       08 : POINT 2 TO 5
!>       09 : POINT 3 TO 6
!>
!>       CROSSED
!>
!>       10 : POINT 1 TO 5
!>       11 : POINT 2 TO 4
!>       12 : POINT 2 TO 6
!>       13 : POINT 3 TO 5
!>       14 : POINT 3 TO 4
!>       15 : POINT 1 TO 6
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, GLOSEG, IELM, IFABOR, IKLE, KNOLG, KP1BOR, MAXSEG, NBOR, NELBOR, NELEM, NELEM2, NELMAX, NELMAX2, NPLAN, NPOIN2, NPTFR, NPTFR2, NSEG, NULONE, ORISEG
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, I1, I2, I3, IELEM, IELEM3D, IPLAN, ISEG2D, ISEG3D, NSEG2D, NSEGH, NSEGV, ONE, TWO
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_STOSEG41
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBSEG(), PLANTE(), STOSEG()
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
!> </td><td> 19/10/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/10/09
!> </td><td> JMH
!> </td><td> NUMBERING OF CROSSED SEGMENTS CHANGED, TO FACILITATE
!>           PARALLELISM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/08/09
!> </td><td> JMH
!> </td><td> CROSSED AND VERTICAL SEGMENTS INVERTED IN THE NUMBERING.
!>           THIS IS FOR FINITE VOLUMES WHICH DO NOT USE CROSSED
!>           SEGMENTS. SUBROUTINES ACCORDINGLY CHANGED:
!>           AS3_4141_Q, AS3_4141_S, PREVERSEG, GETTRISEG
!>           PARCOM2_SEG AND
!>           UPWINDSEG, MURD3D, MURD3D_POS (TELEMAC-3D)
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
!>          <tr><td>IFABOR
!></td><td><--</td><td>TABLEAU DES VOISINS DES FACES.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMBER OF POINT FOLLOWING BOUNDARY POINT K
!>                  (I.E. K+1 MOST OF THE TIME BUT NOT ALWAYS).
!>    </td></tr>
!>          <tr><td>MAXSEG
!></td><td><--</td><td>1st DIMENSION OF MAXSEG.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>GLOBAL NUMBERS OF BOUNDARY POINTS.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMBER OF ELEMENT CONTAINING SEGMENT K OF
!>                  THE BOUNDARY.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!>                  (CAS DES MAILLAGES ADAPTATIFS)
!>    </td></tr>
!>          <tr><td>NELMAX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS.
!>    </td></tr>
!>          <tr><td>NPTFR2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td><--</td><td>NUMBER OF SEGMENTS OF THE MESH.
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
!>                  ELEMENT.
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td><--</td><td>ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE STOSEG41
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,NBOR,NPTFR,
     & GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG,KP1BOR,NELBOR,NULONE,NELMAX2,
     & NELEM2,NPTFR2,NPOIN2,NPLAN,KNOLG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
C| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
C| IELM           |-->| 11: TRIANGLES.
C|                |   | 21: QUADRILATERES.
C| IFABOR         |<--| TABLEAU DES VOISINS DES FACES.
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| KNOLG          |---| 
C| KP1BOR         |-->| NUMBER OF POINT FOLLOWING BOUNDARY POINT K
C|                |   | (I.E. K+1 MOST OF THE TIME BUT NOT ALWAYS).
C| MAXSEG         |<--| 1st DIMENSION OF MAXSEG.
C| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS.
C| NELBOR         |-->| NUMBER OF ELEMENT CONTAINING SEGMENT K OF
C|                |   | THE BOUNDARY.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELEM2         |---| 
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C|                |   | (CAS DES MAILLAGES ADAPTATIFS)
C| NELMAX2        |---| 
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS.
C| NPTFR2         |---| 
C| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
C| NULONE         |-->| LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
C|                |   | ELEMENT.
C| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_STOSEG41 => STOSEG41
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELMAX,NELMAX2,NPTFR,NSEG,MAXSEG,IELM
      INTEGER, INTENT(IN)    :: NELEM,NELEM2,NPTFR2,NPOIN2,NPLAN
      INTEGER, INTENT(IN)    :: NBOR(NPTFR2),KP1BOR(NPTFR2)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX2,*),IKLE(NELMAX,6)
      INTEGER, INTENT(IN)    :: NELBOR(NPTFR2),NULONE(NPTFR2)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(INOUT) :: ELTSEG(NELMAX,15),ORISEG(NELMAX,15)
      INTEGER, INTENT(IN)    :: KNOLG(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I1,I2,I3,IELEM,I,ONE,TWO
      INTEGER NSEG2D,IPLAN,ISEG2D,ISEG3D,IELEM3D,NSEGH,NSEGV
C
C-----------------------------------------------------------------------
C
      IF(IELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,500) IELM
        IF (LNG.EQ.2) WRITE(LU,501) IELM
500     FORMAT(1X,'STOSEG41 (BIEF) : ELEMENT NON PREVU : ',1I6)
501     FORMAT(1X,'STOSEG41 (BIEF) : UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     BUILDS 2D SEGMENTS (THE FIRST IN THE NUMBERING)
C
C     IN PARALLEL MODE, THE NUMBER OF SEGMENTS IN THE CONTOUR LINE IS NOT NPTFR2
C     NSEG2D=(3*NELEM2+NPTFR2)/2
      NSEG2D=NBSEG(11)
      NSEGH=NSEG2D*NPLAN
      NSEGV=(NPLAN-1)*NPOIN2
C
      CALL STOSEG(IFABOR,NELEM2,NELMAX,NELMAX2,11,IKLE,NBOR,NPTFR2,
     &            GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG2D,
     &            KP1BOR,NELBOR,NULONE,KNOLG)
C
C-----------------------------------------------------------------------
C
C     COMPLETES HORIZONTAL SEGMENTS (1,2,3,4,5,6)
C
      DO IPLAN=2,NPLAN
      DO ISEG2D=1,NSEG2D
        ISEG3D=ISEG2D+(IPLAN-1)*NSEG2D
        GLOSEG(ISEG3D,1)=GLOSEG(ISEG2D,1)+NPOIN2*(IPLAN-1)
        GLOSEG(ISEG3D,2)=GLOSEG(ISEG2D,2)+NPOIN2*(IPLAN-1)
      ENDDO
      ENDDO
C
C     VERTICAL SEGMENTS (7,8,9)
C
      DO IPLAN=1,NPLAN-1
      DO I=1,NPOIN2
        ISEG3D=NSEGH+NPOIN2*(IPLAN-1)+I
        GLOSEG(ISEG3D,1)=NPOIN2*(IPLAN-1)+I
        GLOSEG(ISEG3D,2)=NPOIN2*(IPLAN  )+I
      ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
C     ARRAY ELTSEG GIVES GLOBAL NUMBERS OF SEGMENTS IN A PRISM
C     ARRAY ORISEG GIVES ORIENTATION OF SEGMENT
C
C     COMPLETES SEGMENTS 1,2,3
C
      IF(NPLAN.GT.2) THEN
      DO IPLAN=2,NPLAN-1
        DO IELEM=1,NELEM2
          IELEM3D=IELEM+(IPLAN-1)*NELEM2
          ELTSEG(IELEM3D,1)=ELTSEG(IELEM,1)+NSEG2D*(IPLAN-1)
          ELTSEG(IELEM3D,2)=ELTSEG(IELEM,2)+NSEG2D*(IPLAN-1)
          ELTSEG(IELEM3D,3)=ELTSEG(IELEM,3)+NSEG2D*(IPLAN-1)
          ORISEG(IELEM3D,1)=ORISEG(IELEM,1)
          ORISEG(IELEM3D,2)=ORISEG(IELEM,2)
          ORISEG(IELEM3D,3)=ORISEG(IELEM,3)
        ENDDO
      ENDDO
      ENDIF
C
C     SEGMENTS 4,5,6 (=SEGMENTS 1,2,3 + NSEG2D)
C
      DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM2
          IELEM3D=IELEM+(IPLAN-1)*NELEM2
          ELTSEG(IELEM3D,4)=ELTSEG(IELEM3D,1)+NSEG2D
          ELTSEG(IELEM3D,5)=ELTSEG(IELEM3D,2)+NSEG2D
          ELTSEG(IELEM3D,6)=ELTSEG(IELEM3D,3)+NSEG2D
          ORISEG(IELEM3D,4)=ORISEG(IELEM,1)
          ORISEG(IELEM3D,5)=ORISEG(IELEM,2)
          ORISEG(IELEM3D,6)=ORISEG(IELEM,3)
        ENDDO
      ENDDO
C
C     SEGMENTS 7,8,9 (VERTICAL SEGMENTS)  SEE SECTION 3)
C
      DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM2
          IELEM3D=IELEM+(IPLAN-1)*NELEM2
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          ELTSEG(IELEM3D,7)=NSEGH+NPOIN2*(IPLAN-1)+I1
          ELTSEG(IELEM3D,8)=NSEGH+NPOIN2*(IPLAN-1)+I2
          ELTSEG(IELEM3D,9)=NSEGH+NPOIN2*(IPLAN-1)+I3
          ORISEG(IELEM3D,7)=1
          ORISEG(IELEM3D,8)=1
          ORISEG(IELEM3D,9)=1
        ENDDO
      ENDDO
C
C     SEGMENTS 10 TO 15 (CROSSED SEGMENTS)
C
C     THE PROBLEM IS TO FIND A GLOBAL NUMBERING OF CROSSED SEGMENTS
C     WHICH IS INDEPENDENT OF THE PRISM CONSIDERED, SO THAT WHEN
C     ASSEMBLING THE ISEG3D IS THE CORRECT ONE
C     HERE WE TAKE AS CROSSED SEGMENT 1 THE ONE THAT STARTS
C     FROM THE ORIGIN OF THE ORIENTED HORIZONTAL SEGMENT
C
C     FOR EVERY VERTICAL RECTANGLE IN A PRISM
C     WHEN THE HORIZONTAL SEGMENT HAS ORISEG = 1, THE CROSSED
C     SEGMENT NUMBER 1 IS THUS THE 10,12 OR 14
C     WHEN THE HORIZONTAL SEGMENT HAS ORISEG = 2, THE CROSSED
C     SEGMENT NUMBER 1 IS THUS THE 11,13 OR 15
C
C     IN THE GLOBAL NUMBERING OF SEGMENTS
C     THE ORISEG=1 SEGMENTS ARE PUT FIRST (NSEG2D PER LAYER)
C     THE ORISEG=2 SEGMENTS ARE PUT AFTER (NSEG2D PER LAYER)
C
      DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM2
          IELEM3D=IELEM+(IPLAN-1)*NELEM2
          DO I=1,3
            ISEG2D=ELTSEG(IELEM,I)
            ISEG3D=NSEGH+NSEGV+NSEG2D*2*(IPLAN-1)+ISEG2D
            ONE=ISEG3D
            TWO=ISEG3D+NSEG2D
            IF(ORISEG(IELEM,I).EQ.1) THEN
              ELTSEG(IELEM3D,8+2*I)  =ONE
              ELTSEG(IELEM3D,8+2*I+1)=TWO
            ELSE
              ELTSEG(IELEM3D,8+2*I+1)=ONE
              ELTSEG(IELEM3D,8+2*I)  =TWO
            ENDIF
C           ONE: FOLLOWS ISEG2D
            GLOSEG(ONE,1)=GLOSEG(ISEG2D,1)+(IPLAN-1)*NPOIN2
            GLOSEG(ONE,2)=GLOSEG(ISEG2D,2)+(IPLAN  )*NPOIN2
C           BACKWARDS WITH RESPECT TO ISEG2D
            GLOSEG(TWO,1)=GLOSEG(ISEG2D,2)+(IPLAN-1)*NPOIN2
            GLOSEG(TWO,2)=GLOSEG(ISEG2D,1)+(IPLAN  )*NPOIN2
          ENDDO
          ORISEG(IELEM3D,10)=1
          ORISEG(IELEM3D,11)=1
          ORISEG(IELEM3D,12)=1
          ORISEG(IELEM3D,13)=1
          ORISEG(IELEM3D,14)=1
          ORISEG(IELEM3D,15)=1
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C