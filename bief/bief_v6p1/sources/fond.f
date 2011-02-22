C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE BOTTOM ELEVATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KP1BOR, NBOR, NFON, NPOIN, NPTFR, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, C, COTE, ERR, NP, XRELV, YRELV
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FOND
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FASP()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FONSTR()

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
!> </td><td> 20/03/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COTE
!></td><td><--</td><td>TABLEAU DE TRAVAIL DE DIMENSION NPMAX.
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES NOEUDS DE BORD.
!>    </td></tr>
!>          <tr><td>NFON
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER DES FONDS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU PRIVE POUR L'UTILISATEUR.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><--</td><td>COTE DU FOND AUX NOEUDS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FOND
     &(ZF  ,X,Y,NPOIN,NFON,NBOR,KP1BOR,NPTFR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COTE           |<--| TABLEAU DE TRAVAIL DE DIMENSION NPMAX.
C| KP1BOR         |---| 
C| NBOR           |-->| NUMEROS GLOBAUX DES NOEUDS DE BORD.
C| NFON           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES FONDS.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| PRIVE          |-->| TABLEAU PRIVE POUR L'UTILISATEUR.
C| X,Y            |-->| COORDONNEES DU MAILLAGE.
C| ZF             |<--| COTE DU FOND AUX NOEUDS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FOND => FOND
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NFON,NPOIN,NPTFR
      DOUBLE PRECISION, INTENT(OUT) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NP,ERR
C
      DOUBLE PRECISION BID
C
      CHARACTER*1 C
C
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XRELV,YRELV,COTE
C
C-----------------------------------------------------------------------
C                    READS THE DIGITISED POINTS
C                      FROM LOGICAL UNIT NFON
C-----------------------------------------------------------------------
C
C  ASSESSES THE EXTENT OF DATA
C
      NP = 0
20    READ(NFON,120,END=24,ERR=124) C
120   FORMAT(A1)
      IF(C(1:1).NE.'C'.AND.C(1:1).NE.'B') THEN
        BACKSPACE ( UNIT = NFON )
        NP = NP + 1
        READ(NFON,*) BID,BID,BID
      ENDIF
      GO TO 20
124   CONTINUE
      IF(LNG.EQ.1) WRITE(LU,18) NP
      IF(LNG.EQ.2) WRITE(LU,19) NP
18    FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERREUR DANS LE FICHIER DES FONDS'
     &      ,/,1X,'A LA LIGNE ',I7)
19    FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERROR IN THE BOTTOM FILE'
     &      ,/,1X,'AT LINE ',I7)
      STOP
24    CONTINUE
C
C  DYNAMICALLY ALLOCATES THE ARRAYS
C
      ALLOCATE(XRELV(NP),STAT=ERR)
      ALLOCATE(YRELV(NP),STAT=ERR)
      ALLOCATE(COTE(NP) ,STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,10) NP
        IF(LNG.EQ.2) WRITE(LU,11) NP
10      FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERREUR A L''ALLOCATION DE 3 TABLEAUX'
     &      ,/,1X,'DE TAILLE ',I7)
11      FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERROR DURING ALLOCATION OF 3 ARRAYS'
     &      ,/,1X,'OF SIZE ',I7)
        STOP
      ENDIF
C
C  READS THE DATA
C
      REWIND(NFON)
      NP = 0
23    READ(NFON,120,END=22,ERR=122) C
      IF(C(1:1).NE.'C'.AND.C(1:1).NE.'B') THEN
        BACKSPACE ( UNIT = NFON )
        NP = NP + 1
        READ(NFON,*) XRELV(NP) , YRELV(NP) , COTE(NP)
      ENDIF
      GO TO 23
C
122   CONTINUE
      IF(LNG.EQ.1) WRITE(LU,12) NP
      IF(LNG.EQ.2) WRITE(LU,13) NP
12    FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERREUR DANS LE FICHIER DES FONDS'
     &      ,/,1X,'A LA LIGNE ',I7)
13    FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERROR IN THE BOTTOM FILE'
     &      ,/,1X,'AT LINE ',I7)
      STOP
C
22    CONTINUE
C
      IF(LNG.EQ.1) WRITE(LU,112) NP
      IF(LNG.EQ.2) WRITE(LU,113) NP
112   FORMAT(1X,'FOND (BIEF) :'
     &      ,/,1X,'NOMBRE DE POINTS DANS LE FICHIER DES FONDS : ',I7)
113   FORMAT(1X,'FOND (BIEF):'
     &      ,/,1X,'NUMBER OF POINTS IN THE BOTTOM FILE: ',I7)
C
C-----------------------------------------------------------------------
C   THE BOTTOM ELEVATION IS COMPUTED BY INTERPOLATION ONTO THE
C                      DOMAIN INTERIOR POINTS
C-----------------------------------------------------------------------
C
      CALL FASP(X,Y,ZF,NPOIN,XRELV,YRELV,COTE,NP,NBOR,KP1BOR,NPTFR,0.D0)
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(XRELV)
      DEALLOCATE(YRELV)
      DEALLOCATE(COTE)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C