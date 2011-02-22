C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS ARRAY KNOGL.
!>                MODIFIES NBOR,NACHB TO GO FROM GLOBAL NODE NUMBERS
!>               (WHOLE MESH) TO GLOBAL NODE NUMBERS (SUB-DOMAIN).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM1_KNOGL, KNOGL, KNOLG, NACHB, NBOR, NPOIN, NPTFR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NPTIR NPTIR@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::NNAMECODE NNAMECODE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARAGL
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
!>      <td><center> 5.6                                       </center>
!> </td><td> 19/12/05
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIM1_KNOGL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNOGL
!></td><td><--</td><td>PASSAGE DES NUMEROS GLOBAUX AUX LOCAUX.
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td><--</td><td>PASSAGE DES NUMEROS LOCAUX  AUX GLOBAUX.
!>    </td></tr>
!>          <tr><td>NACHB
!></td><td><-></td><td>NACHB(1,I) : GLOBAL (INPUT) OR LOCAL (OUTPUT)
!>                  NUMBER OF INTERFACE POINT.
!>                  NACHB(2 TO 5,I) : NUMBER OF OTHER SUB-DOMAINS
!>                  CONTAINING THE POINT I.
!>                  I IS A NUMBERING OF INTERFACE POINTS.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td><-></td><td>NUMEROS GLOBAUX DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE DOMAIN.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARAGL
     &(KNOGL,DIM1_KNOGL,KNOLG,NBOR,NACHB,NPTFR,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM1_KNOGL     |---| 
C| KNOGL          |<--| PASSAGE DES NUMEROS GLOBAUX AUX LOCAUX.
C| KNOLG          |<--| PASSAGE DES NUMEROS LOCAUX  AUX GLOBAUX.
C| NACHB          |<->| NACHB(1,I) : GLOBAL (INPUT) OR LOCAL (OUTPUT)
C|                |   | NUMBER OF INTERFACE POINT.
C|                |   | NACHB(2 TO 5,I) : NUMBER OF OTHER SUB-DOMAINS
C|                |   | CONTAINING THE POINT I.
C|                |   | I IS A NUMBERING OF INTERFACE POINTS.
C| NBOR           |<->| NUMEROS GLOBAUX DES POINTS DE BORD.
C| NPOIN          |-->| NUMBER OF POINTS IN THE DOMAIN.
C| NPTFR          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARAGL => PARAGL
      USE DECLARATIONS_TELEMAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: DIM1_KNOGL,NPTFR,NPOIN
      INTEGER, INTENT(INOUT) :: KNOGL(DIM1_KNOGL),KNOLG(NPOIN)
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR),NACHB(NBMAXNSHARE,NPTIR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
C  BUILDS THE REVERSE OF ARRAY KNOLG:
C
C     POINTS OUTSIDE THE DOMAIN : 0
C     SEE IN ALMESH THE SIZE OF KNOGL: SUM OF NPOIN OF ALL SUB-DOMAINS
      DO I=1,DIM1_KNOGL
        KNOGL(I) = 0
      ENDDO
C
      DO I=1,NPOIN
        KNOGL(KNOLG(I))=I
      ENDDO
C
C  FROM IKLE TO SUB-DOMAIN NUMBERING
C  ALREADY DONE BY THE DOMAIN PARTITIONER
C      DO 18 J=1,3
C      DO 17 I=1,NELEM
C        IKLE(I,J)=KNOGL(IKLE(I,J))
C17    CONTINUE
C18    CONTINUE
C
C  FROM NBOR TO SUB-DOMAIN NUMBERING
C  EXCEPT FOR ESTEL3D WHERE NBOR ALREADY HAS LOCAL NUMBERS
C  SEE M_UNV2MESH.F90 /ESTEL3D  AND PARTEL.F /PARALLEL
C
      IF(NNAMECODE(1).NE.'ESTEL3D                 ') THEN
        DO 19 I=1,NPTFR
          NBOR(I)=KNOGL(NBOR(I))
19      CONTINUE
      ENDIF
C
C  FROM NACHB TO SUB-DOMAIN NUMBERING
C
      DO 21 I=1,NPTIR
        NACHB(1,I)=KNOGL(NACHB(1,I))
21    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C