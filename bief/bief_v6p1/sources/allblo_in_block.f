C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES MEMORY FOR N BLOCKS, WHICH WILL BE PART
!>                OF A GIVEN BLOCK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BLO, N, NOMGEN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CHIFFRE, I, IDEB, II, NOM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ALLBLO_IN_BLOCK
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLBLO()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>POINT_TELEMAC3D(), SOLVE()

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
!> </td><td> 11/07/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BLO
!></td><td><-></td><td>BLOC OU ON VA ALLOUER LES VECTEURS
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NOMBRE DE VECTEURS A ALLOUER
!>    </td></tr>
!>          <tr><td>NOMGEN
!></td><td>--></td><td>NOM GENERIQUE FORTRAN DES VECTEURS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ALLBLO_IN_BLOCK
     &( BLO , N , NOMGEN )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BLO            |<->| BLOC OU ON VA ALLOUER LES VECTEURS
C| N             |-->| NOMBRE DE VECTEURS A ALLOUER
C| NOMGEN         |-->| NOM GENERIQUE FORTRAN DES VECTEURS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ALLBLO_IN_BLOCK => ALLBLO_IN_BLOCK
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      INTEGER         , INTENT(IN)    :: N
      CHARACTER(LEN=6), INTENT(IN)    :: NOMGEN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IDEB,I,II
C
      CHARACTER(LEN=6) :: NOM
      CHARACTER(LEN=1) :: CHIFFRE(0:9)
      DATA CHIFFRE/'0','1','2','3','4','5','6','7','8','9'/
      SAVE CHIFFRE
C
C-----------------------------------------------------------------------
C
      IDEB = 6
      DO 5 I=5,2,-1
        IF(NOMGEN(I:I).EQ.' ') IDEB = I
5     CONTINUE
C
C-----------------------------------------------------------------------
C
      IF(N.LE.BLO%MAXBLOCK) THEN
C
      DO I = 1 , N
C
C  NAME OF THE BLOCK
C
        NOM=NOMGEN
        IF(I.LT.10) THEN
          IDEB = MIN(6,IDEB)
          NOM(IDEB:IDEB) = CHIFFRE(I)
        ELSEIF(I.LT.100) THEN
          IDEB = MIN(5,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/10)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(I-10*(I/10))
        ELSEIF(I.LT.1000) THEN
          IDEB = MIN(4,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/100)
          II=I-100*(I/100)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(II/10)
          NOM(IDEB+2:IDEB+2) = CHIFFRE(II-10*(II/10))
        ELSE
          STOP 'TOO MANY BLOCKS IN ALLBLO_IN_BLOCK'
        ENDIF
C
C  ALLOCATES THE BLOCK
C
        ALLOCATE(BLO%ADR(I)%P)
        CALL ALLBLO(BLO%ADR(I)%P,NOM)
        BLO%N=BLO%N+1
C
      ENDDO
C
      ELSE
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'ALLBLO_IN_BLOCK : PLUS DE ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) '                  BLOCS DEMANDES'
        WRITE(LU,*) '                  CHANGER MAXBLOCK DANS ALLBLO'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'ALLBLO_IN_BLOCK : MORE THAN '
        WRITE(LU,*) '                 ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) '                  BLOCKS TO BE ALLOCATED'
        WRITE(LU,*) '                  CHANGE MAXBLOCK IN ALLBLO'
      ENDIF
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