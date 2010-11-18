C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COPIES A MATRIX STRUCTURE ONTO ANOTHER.
!>                X COPIED ONTO Y.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> TRANS, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM1, IELM2, IELN1, IELN2, TR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CPSTMT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>OM()

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
!> </td><td> 03/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>TRANS
!></td><td>---</td><td>
!>    </td></tr>
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
                        SUBROUTINE CPSTMT
     &( X , Y , TRANS )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| TRANS          |---| 
C| X             |-->| STRUCTURE COPIEE.
C| Y             |<--| STRUCTURE SUR LAQUELLE ON COPIE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CPSTMT => CPSTMT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: Y
      LOGICAL, INTENT(IN), OPTIONAL :: TRANS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELM1,IELM2,IELN1,IELN2
      LOGICAL TR
C
C-----------------------------------------------------------------------
C  TREATS ONLY MATRICES HERE :
C-----------------------------------------------------------------------
C
      IF(X%TYPE.NE.3.OR.Y%TYPE.NE.3) THEN
        IF(LNG.EQ.1) WRITE(LU,200) X%NAME,X%TYPE,Y%NAME,Y%TYPE
        IF(LNG.EQ.2) WRITE(LU,201) X%NAME,X%TYPE,Y%NAME,Y%TYPE
200     FORMAT(1X,'CPSTMT : CAS NON PREVU POUR X ET Y :',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
201     FORMAT(1X,'CPSTMT : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(PRESENT(TRANS)) THEN
        TR = TRANS
      ELSE
        TR = .FALSE.
      ENDIF
C
      IF(.NOT.TR) THEN
        IELM1 = X%ELMLIN
        IELM2 = X%ELMCOL
      ELSE
        IELM1 = X%ELMCOL
        IELM2 = X%ELMLIN
      ENDIF
C
C CONTROLS MEMORY SIZE FOR DIAGONAL AND EXTRA-DIAGONAL TERMS :
C
      IF(X%D%DIM1.GT.Y%D%MAXDIM1.OR.
     &   X%X%DIM2*X%X%DIM1.GT.Y%X%MAXDIM1*Y%X%MAXDIM2) THEN
        IELN1 = Y%ELMLIN
        IELN2 = Y%ELMCOL
        IF(LNG.EQ.1) WRITE(LU,400) X%NAME,IELM1,IELM2,Y%NAME,IELN1,IELN2
        IF(LNG.EQ.2) WRITE(LU,401) X%NAME,IELM1,IELM2,Y%NAME,IELN1,IELN2
        IF(LNG.EQ.1) WRITE(LU,402) X%TYPDIA,X%D%DIM1,X%TYPEXT,
     &                 X%X%DIM2*X%X%DIM1,
     &                 Y%TYPDIA,Y%D%MAXDIM1,
     &                 Y%TYPEXT,Y%X%MAXDIM1*Y%X%MAXDIM2
        IF(LNG.EQ.2) WRITE(LU,403) X%TYPDIA,X%D%DIM1,X%TYPEXT,
     &                 X%X%DIM2*X%X%DIM1,
     &                 Y%TYPDIA,Y%D%MAXDIM1,
     &                 Y%TYPEXT,Y%X%MAXDIM1*Y%X%MAXDIM2
 400    FORMAT(1X,'CPSTMT : CAS IMPOSSIBLE POUR X ET Y :',/,1X,
     &            'X=',A6,/,1X,'ELEMENTS ',1I6,' ET ',1I6,/,1X,
     &            'Y=',A6,/,1X,'ELEMENTS ',1I6,' ET ',1I6,/,1X,
     &            'Y EST PLUS PETITE QUE X')
 402    FORMAT(1X,'X A UNE DIAGONALE DE TYPE ',A1,/,1X,
     &            'AVEC UNE TAILLE DE ',1I6,/,1X,
     &            'DES TERMES EXTRADIAGONAUX DE TYPE ',A1,/,1X,
     &            'AVEC UNE TAILLE DE ',1I6,/,1X,
     &            'Y A UNE DIAGONALE DE TYPE ',A1,/,1X,
     &            'ET DE TAILLE MAXIMUM ',1I6,/,1X,
     &            'DES TERMES EXTRADIAGONAUX DE TYPE ',A1,/,1X,
     &            'ET UNE TAILLE MAXIMUM DE ',1I6)
 401    FORMAT(1X,'CPSTMT : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,/,1X,'ELEMENTS ',1I6,' AND ',1I6,/,1X,
     &            'Y=',A6,/,1X,'ELEMENTS ',1I6,' AND ',1I6,/,1X,
     &            'Y IS SMALLER THAN X')
 403    FORMAT(1X,'X HAS A DIAGONAL OF TYPE ',A1,/,1X,
     &            'WITH A SIZE OF ',1I6,/,1X,
     &            'AND OFF-DIAGONAL TERMS OF TYPE ',A1,/,1X,
     &            'WITH A SIZE OF ',1I6,/,1X,
     &            'Y HAS A DIAGONAL OF TYPE ',A1,/,1X,
     &            'AND A MAXIMUM SIZE OF ',1I6,/,1X,
     &            'AND OFF-DIAGONAL TERMS OF TYPE ',A1,/,1X,
     &            'AND A MAXIMUM SIZE OF ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C COPIES TYPES OF ELEMENTS
C
      Y%ELMLIN = IELM1
      Y%ELMCOL = IELM2
C
C 4) COPIES TYPES OF DIAGONAL AND EXTRADIAGONAL TERMS
C
      CALL CPSTVC(X%D,Y%D)
      CALL CPSTVC(X%X,Y%X)
C
C 5) COPIES THE MATRIX CHARACTERISTICS
C
      Y%TYPDIA = X%TYPDIA
      Y%TYPEXT = X%TYPEXT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C