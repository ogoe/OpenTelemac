C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, GLOSEG, IELM, IFABOR, IKLE, KNOLG, KP1BOR, MAXSEG, NBOR, NELBOR, NELEM, NELMAX, NELMAX2, NPTFR, NSEG, NULONE, ORISEG
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
!>    </th><td> I1, I2, IELEM, IELEM1, IELEM2, IFA, IFACE, IG1, IG2, IPTFR, J1, J2, JFACE, NEL, NEXT, NSE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_STOSEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELMET(), GREDELSEG(), INBIEF(), STOSEG41()

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
!> </td><td> 02/10/08
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
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
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!>                  (CAS DES MAILLAGES ADAPTATIFS)
!>    </td></tr>
!>          <tr><td>NELMAX2
!></td><td>--></td><td>PREMIERE DIMENSION DE IFABOR
!>                  (EN 3D LE NOMBRE D'ELEMENTS 2D)
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS.
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
                        SUBROUTINE STOSEG
     &(IFABOR,NELEM,NELMAX,NELMAX2,IELM,IKLE,NBOR,NPTFR,
     & GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG,KP1BOR,NELBOR,NULONE,KNOLG)
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
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C|                |   | (CAS DES MAILLAGES ADAPTATIFS)
C| NELMAX2        |-->| PREMIERE DIMENSION DE IFABOR
C|                |   | (EN 3D LE NOMBRE D'ELEMENTS 2D)
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS.
C| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
C| NULONE         |-->| LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
C|                |   | ELEMENT.
C| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_STOSEG => STOSEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELMAX,NELMAX2,NPTFR,NSEG,MAXSEG,IELM
      INTEGER, INTENT(IN)    :: NELEM
      INTEGER, INTENT(IN)    :: NBOR(NPTFR),KP1BOR(NPTFR)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX2,*),IKLE(NELMAX,*)
      INTEGER, INTENT(IN)    :: NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(INOUT) :: ELTSEG(NELMAX,*),ORISEG(NELMAX,3)
      INTEGER, INTENT(IN)    :: KNOLG(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPTFR,NSE
C
      INTEGER NEL,IFA,I1,I2,J1,J2,IFACE,JFACE,IG1,IG2
      INTEGER IELEM,IELEM1,IELEM2
C
      INTEGER NEXT(3)
      DATA NEXT / 2,3,1 /
C
C-----------------------------------------------------------------------
C
      IF(IELM.NE.11.AND.IELM.NE.12.AND.IELM.NE.13.AND.IELM.NE.14) THEN
        IF (LNG.EQ.1) WRITE(LU,500) IELM
        IF (LNG.EQ.2) WRITE(LU,501) IELM
500     FORMAT(1X,'STOSEG (BIEF) : ELEMENT NON PREVU : ',1I6)
501     FORMAT(1X,'STOSEG (BIEF) : UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     INITIALISES ELTSEG
C
      DO IELEM = 1 , NELEM
        ELTSEG(IELEM,1) = 0
        ELTSEG(IELEM,2) = 0
        ELTSEG(IELEM,3) = 0
      ENDDO
C
C-----------------------------------------------------------------------
C
C     LOOP ON BOUNDARY POINTS :
C
      NSE = 0
      DO IPTFR = 1 , NPTFR
C
C       IN PARALLEL MODE, IF THE BOUNDARY POINT FOLLOWING IPTFR IS IN
C       ANOTHER SUB-DOMAIN, KP1BOR(IPTFR)=IPTFR.
C       IN THIS CASE THE SEGMENT
C       BASED ON IPTFR AND THIS POINT IS NOT IN THE LOCAL DOMAIN.
C       A CONSEQUENCE IS THAT NSE IS NOT EQUAL TO IPTFR.
C
        IF(KP1BOR(IPTFR).NE.IPTFR) THEN
C
          NSE = NSE + 1
C         NOTE: ON BOUNDARIES, SEGMENTS ARE NOT ORIENTED LOWER RANK
C               TO HIGHER RANK, AS IS DONE FOR INTERNAL SEGMENTS
          GLOSEG(NSE,1) = NBOR(IPTFR)
          GLOSEG(NSE,2) = NBOR(KP1BOR(IPTFR))
          NEL = NELBOR(IPTFR)
          IFA = NULONE(IPTFR)
          ELTSEG(NEL,IFA) = NSE
          ORISEG(NEL,IFA) = 1
C
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     LOOP ON ELEMENTS FOR NUMBERING INTERNAL SEGMENTS AND FILLING:
C     GLOSEG, ELTSEG, ORISEG
C
      DO IELEM1 = 1 , NELEM
        DO IFACE = 1 , 3
          IF(ELTSEG(IELEM1,IFACE).EQ.0) THEN
C           NEW SEGMENT (HENCE INTERNAL SO IFABOR<>0)
            NSE = NSE + 1
C           BOTH NEIGHBOURING ELEMENTS ARE TREATED FOR THIS SEGMENT
            I1 = IKLE(IELEM1,     IFACE)
            I2 = IKLE(IELEM1,NEXT(IFACE))
            IF(I1.EQ.I2) THEN
              IF(LNG.EQ.1) THEN
               WRITE(LU,*) 'STOSEG : SEGMENT AVEC UN SEUL POINT'
               WRITE(LU,*) '         ELEMENT ',IELEM1,' FACE ',IFACE
              ENDIF
              IF(LNG.EQ.2) THEN
               WRITE(LU,*) 'STOSEG: EDGE MADE OF ONLY ONE POINT'
               WRITE(LU,*) '        ELEMENT ',IELEM1,' FACE ',IFACE
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
            ELTSEG(IELEM1,IFACE) = NSE
            IF(NCSIZE.GT.1) THEN
              IG1=KNOLG(I1)
              IG2=KNOLG(I2)
            ELSE
              IG1=I1
              IG2=I2
            ENDIF
C           SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
            IF(IG1.LT.IG2) THEN
              GLOSEG(NSE,1) = I1
              GLOSEG(NSE,2) = I2
              ORISEG(IELEM1,IFACE) = 1
            ELSE
              GLOSEG(NSE,1) = I2
              GLOSEG(NSE,2) = I1
              ORISEG(IELEM1,IFACE) = 2
            ENDIF
C           OTHER ELEMENT NEIGHBOURING THIS SEGMENT
            IELEM2 = IFABOR(IELEM1,IFACE)
C           IELEM2 = 0 OR -1 MAY OCCUR IN PARALLEL MODE
            IF(IELEM2.GT.0) THEN
C             LOOKS FOR THE RIGHT SIDE OF ELEMENT IELEM2
              DO JFACE = 1,3
                J1 = IKLE(IELEM2,     JFACE)
                J2 = IKLE(IELEM2,NEXT(JFACE))
C               ALL ELEMENTS HAVE A COUNTER-CLOCKWISE NUMBERING
                IF(I1.EQ.J2.AND.I2.EQ.J1) THEN
                  ELTSEG(IELEM2,JFACE) = NSE
                  ORISEG(IELEM2,JFACE) = 3-ORISEG(IELEM1,IFACE)
C                 SIDE FOUND, NO NEED TO GO ON
                  GO TO 1000
                ELSEIF(I1.EQ.J1.AND.I2.EQ.J2) THEN
C                 SIDE BADLY ORIENTED
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'STOSEG : MAILLAGE DEFECTUEUX'
                    WRITE(LU,*) '         LA FACE ',JFACE
                    WRITE(LU,*) '         DE L''ELEMENT ',IELEM2
                    WRITE(LU,*) '         EST MAL ORIENTEE'
                    WRITE(LU,*) '         (POINTS ',I1,' ET ',I2,')'
                  ENDIF
                  IF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'STOSEG: WRONG MESH'
                    WRITE(LU,*) '        FACE ',JFACE
                    WRITE(LU,*) '        OF ELEMENT ',IELEM2
                    WRITE(LU,*) '        IS NOT WELL ORIENTED'
                    WRITE(LU,*) '         (POINTS ',I1,' AND ',I2,')'
                  ENDIF
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ENDDO
C             SIDE NOT FOUND, THIS IS AN ERROR
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'STOSEG : MAILLAGE DEFECTUEUX'
                WRITE(LU,*) '         ELEMENTS ',IELEM1,' ET ',IELEM2
                WRITE(LU,*) '         LIES PAR LES POINTS ',I1,' ET ',I2
                WRITE(LU,*) '         MAIS CES POINTS NE FONT PAS UNE'
                WRITE(LU,*) '         FACE DE L''ELEMENT ',IELEM2
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'STOSEG: WRONG MESH'
                WRITE(LU,*) '        ELEMENTS ',IELEM1,' AND ',IELEM2
                WRITE(LU,*) '        LINKED BY POINTS ',I1,' AND ',I2
                WRITE(LU,*) '        BUT THESE POINTS ARE NOT AN EDGE'
                WRITE(LU,*) '        OF ELEMENT ',IELEM2
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
1000        CONTINUE
          ENDIF
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
C     CHECKS
C
      IF(NSEG.NE.NSE) THEN
        IF (LNG.EQ.1) WRITE(LU,502) NSE,NSEG
        IF (LNG.EQ.2) WRITE(LU,503) NSE,NSEG
502     FORMAT(1X,'STOSEG (BIEF) : MAUVAIS NOMBRE DE SEGMENTS : ',1I6,
     &            '                AU LIEU DE ',1I6,' ATTENDUS')
503     FORMAT(1X,'STOSEG (BIEF): WRONG NUMBER OF SEGMENTS : ',1I6,
     &            '               INSTEAD OF ',1I6,' EXPECTED')
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