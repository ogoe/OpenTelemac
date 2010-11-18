C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES DATA NECESSARY TO RESUME COMPUTATION
!>                AT A LATER DATE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINR3D, COURAN, DEPTH, F, FREQ, MAREE, NELEM2, NF, NPLAN, NPOIN2, NR3D, TETA, TITRE, U, UV, V, VENT, VV
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ATT, CAR, IB, ISTAT, NTOT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ECRI2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>BINR3D
!></td><td>--></td><td>BINAIRE DU FICHIER DES RESULTATS GLOBAUX
!>    </td></tr>
!>          <tr><td>COURAN
!></td><td>--></td><td>LOGIQUE INDIQUANT SI IL YA UN COURANT
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>DENSITE SPECTRALE D'ENERGIE
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>DISTRIBUTION DES FREQUENCES
!>    </td></tr>
!>          <tr><td>MAREE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS OU DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NR3D
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER DES
!>                  RESULTATS GLOBAUX
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>DISTRIBUTION DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>TITRE
!></td><td>--></td><td>TITRE DU CAS
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DU COURANT
!>    </td></tr>
!>          <tr><td>UV,VV
!></td><td>--></td><td>COMPOSANTES DU VENT
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>LOGIQUE INDIQUANT SI IL YA UN VENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SOR3D
     &(F,NPLAN,NF,TETA,FREQ,NELEM2,NPOIN2,AT,U,V,UV,VV,DEPTH,VENT,
     & COURAN,MAREE,TITRE,NR3D,BINR3D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINR3D         |-->| BINAIRE DU FICHIER DES RESULTATS GLOBAUX
C| COURAN         |-->| LOGIQUE INDIQUANT SI IL YA UN COURANT
C| DEPTH          |---| 
C| F             |<--| DENSITE SPECTRALE D'ENERGIE
C| FREQ           |-->| DISTRIBUTION DES FREQUENCES
C| MAREE          |---| 
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NR3D           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES
C|                |   | RESULTATS GLOBAUX
C| TETA           |-->| DISTRIBUTION DES DIRECTIONS
C| TITRE          |-->| TITRE DU CAS
C| U,V            |-->| COMPOSANTES DU COURANT
C| UV,VV          |-->| COMPOSANTES DU VENT
C| VENT           |-->| LOGIQUE INDIQUANT SI IL YA UN VENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER NR3D,NF,NPLAN,NELEM2,NPOIN2
C
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),AT, ATT(1)
      DOUBLE PRECISION FREQ(NF),TETA(NPLAN)
      DOUBLE PRECISION U(NPOIN2),V(NPOIN2),UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2)
C
      INTEGER ISTAT,IB(2),NTOT
C
      LOGICAL COURAN,VENT,MAREE
C
      CHARACTER*3 BINR3D,CAR
      CHARACTER*72 TITRE
C
C***********************************************************************
C
C
C WRITES TITLE
C
      CALL ECRI2(F,IB,TITRE,80,'CH',NR3D,BINR3D,ISTAT)
C
C WRITES NPLAN, NF
C
      IB(1)=NPLAN
      IB(2)=NF
      CALL ECRI2(F,IB,CAR,2,'I ',NR3D,BINR3D,ISTAT)
C
C WRITES NELEM2, NPOIN2
C
      IB(1)=NELEM2
      IB(2)=NPOIN2
      CALL ECRI2(F,IB,CAR,2,'I ',NR3D,BINR3D,ISTAT)
C
C WRITES TIME
C
      ATT(1)=AT
      CALL ECRI2(ATT,IB,CAR,1,'R4',NR3D,BINR3D,ISTAT)
C
C WRITES TETA
C
      CALL ECRI2(TETA,IB,CAR,NPLAN,'R4',NR3D,BINR3D,ISTAT)
C
C WRITES FREQ
C
      CALL ECRI2(FREQ,IB,CAR,NF,'R4',NR3D,BINR3D,ISTAT)
C
C WRITES F
C
      NTOT=NPOIN2*NPLAN*NF
      CALL ECRI2(F,IB,CAR,NTOT,'R4',NR3D,BINR3D,ISTAT)
C
C WRITES U,V,UV,VV (IF HAS TO)
C
      IF (COURAN) THEN
      CALL ECRI2(U ,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      CALL ECRI2(V ,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      ENDIF
      IF (VENT) THEN
      CALL ECRI2(UV,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      CALL ECRI2(VV,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      ENDIF
      IF (MAREE) THEN
      CALL ECRI2(DEPTH,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C