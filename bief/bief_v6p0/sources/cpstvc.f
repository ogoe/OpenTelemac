C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COPIES A VECTOR STRUCTURE ONTO ANOTHER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> SIZEX, SIZEY
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CPSTVC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH(), BEDLOAD_BAILARD(), BEDLOAD_MEYER(), BEDLOAD_NERBED_VF(), BEDLOAD_SOLVS_FE(), BILANT1(), CFLPSI(), CHARAC(), CPSTMT(), CVDFTR(), CVTRVF(), CVTRVF_POS(), DIFF3D(), DIRI01(), DIRI04(), DIRI09(), DRAGFO(), DWNUP1(), FRICTI(), FRICTION_UNIF(), FRICTION_ZONES(), FSGRAD(), GODWN1(), GOUP1(), GRACJG(), INBIEF(), KEPSIL(), LUMP(), MATVEC(), MAXSLOPE(), MURD3D(), MURD3D_POS(), OS(), POSITIVE_DEPTHS(), PREDIV(), PRERES_TELEMAC2D(), PRERES_TELEMAC3D(), PROPAG(), PROPAG_ADJ(), PROSOU(), PUOG1(), RESCJG(), SOURCES_SINKS(), STREAMLINE(), SUSPENSION_COMPUTATION(), SUSPENSION_DISPERSION(), TELEMAC2D(), TELEMAC3D(), VGFPSI(), VISCLM(), WAVE_EQUATION(), WSTARW()

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
!> </td><td> 01/03/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>X
!></td><td>--></td><td>STRUCTURE COPIEE.
!>    </td></tr>
!>          <tr><td>Y
!></td><td><--</td><td>STRUCTURE SUR LAQUELLE ON COPIE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CPSTVC
     &( X , Y )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| X             |-->| STRUCTURE COPIEE.
C| Y             |<--| STRUCTURE SUR LAQUELLE ON COPIE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CPSTVC => CPSTVC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: Y
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER SIZEX,SIZEY
C
C-----------------------------------------------------------------------
C  TREATS ONLY VECTORS HERE :
C-----------------------------------------------------------------------
C
      IF(Y%TYPE.NE.2.OR.X%TYPE.NE.2) THEN
        IF(LNG.EQ.1) WRITE(LU,200) X%NAME,X%TYPE,Y%NAME,Y%TYPE
        IF(LNG.EQ.2) WRITE(LU,201) X%NAME,X%TYPE,Y%NAME,Y%TYPE
 200    FORMAT(1X,'CPSTVC : CAS NON PREVU POUR X ET Y :',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
 201    FORMAT(1X,'CPSTVC : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
      SIZEX = X%DIM1*X%DIM2
      SIZEY = Y%MAXDIM1*Y%MAXDIM2
C
      IF(SIZEX.GT.SIZEY) THEN
        IF(LNG.EQ.1) WRITE(LU,300) X%NAME,SIZEX,Y%NAME,SIZEY
        IF(LNG.EQ.2) WRITE(LU,301) X%NAME,SIZEX,Y%NAME,SIZEY
 300    FORMAT(1X,'CPSTVC : CAS NON PREVU POUR X ET Y:',/,1X,
     &            'X=',A6,' TAILLE         :',1I6,/,1X,
     &            'Y=',A6,' TAILLE MAXIMUM :',1I6)
 301    FORMAT(1X,'CPSTVC : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' SIZE        :',1I6,/,1X,
     &            'Y=',A6,' MAXIMUM SIZE:',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     DISCRETISATION
      IF(Y%ELM.NE.X%ELM.AND.Y%STATUS.EQ.1) THEN
        IF(LNG.EQ.1) WRITE(LU,400) X%NAME,Y%NAME
        IF(LNG.EQ.2) WRITE(LU,401) X%NAME,Y%NAME
400     FORMAT(1X,'CPSTVC : COPIE DE ',A6,' INTERDITE SUR ',A6)
401     FORMAT(1X,'CPSTVC : COPY OF ',A6,' FORBIDDEN ON ',A6)
        CALL PLANTE(1)
        STOP
      ELSE
        Y%ELM = X%ELM
      ENDIF
C     FIRST VECTOR DIMENSION
      Y%DIM1 = X%DIM1
C     SECOND VECTOR DIMENSION
      Y%DIM2 = X%DIM2
C     CASE OF DISCONTINUOUS VECTORS
      Y%DIMDISC = X%DIMDISC
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C