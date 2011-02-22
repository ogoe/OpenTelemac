C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE DATA FOR A CONTINUATION OF COMPUTATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINPRE, COURAN, DEPTH, DZHDT, F, FREQ, MAREE, NELEM2, NF, NPLAN, NPOIN2, NPRE, TC1, TC2, TETA, TM1, TM2, TV1, TV2, UC, UC1, UC2, UV, UV1, UV2, VC, VC1, VC2, VENT, VV, VV1, VV2, ZM1, ZM2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ATT, CAR, I, IB, ISTAT, NTOT, PI, W, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LIT(), OV(), PLANTE()
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

C
C#######################################################################
C
                        SUBROUTINE LECSUI
     &(F,NPLAN,NF,TETA,FREQ,NELEM2,NPOIN2,AT,UC,VC,UC1,VC1,UC2,VC2,
     & UV,VV,UV1,VV1,UV2,VV2,VENT,TV1,TV2,
     & COURAN,NPRE,BINPRE,DEPTH,TC1,TC2,ZM1,ZM2,DZHDT,TM1,TM2,MAREE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINPRE         |-->| BINAIRE DU FICHIER DU CALCUL PRECEDENT
C| COURAN         |-->| LOGIQUE INDIQUANT SI IL YA UN COURANT
C| DEPTH          |---| 
C| DZHDT          |---| 
C| F             |<--| DENSITE SPECTRALE D'ENERGIE
C| FREQ           |<--| DISTRIBUTION DES FREQUENCES
C| MAREE          |---| 
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPRE           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DU CALCUL
C|                |   | PRECEDENT
C| TC1            |---| 
C| TC2            |---| 
C| TETA           |<--| DISTRIBUTION DES DIRECTIONS
C| TM1            |---| 
C| TM2            |---| 
C| TV1,2          |-->| TEMPS DES ENREGISTREMENTS DE VENT 1 ET 2
C| TV2            |---| 
C| U,V            |<--| COMPOSANTES DU COURANT
C| U1,V1          |<--| COMPOSANTES DU VENT
C| U2,V2          |<--| COMPOSANTES DU VENT
C| UC             |---| 
C| UC1            |---| 
C| UC2            |---| 
C| UV,VV          |<--| COMPOSANTES DU VENT
C| UV1            |---| 
C| UV2            |---| 
C| VC             |---| 
C| VC1            |---| 
C| VC2            |---| 
C| VENT           |-->| LOGIQUE INDIQUANT SI IL YA UN VENT
C| VV1            |---| 
C| VV2            |---| 
C| ZM1            |---| 
C| ZM2            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPRE,NF,NPLAN,NELEM2,NPOIN2
      INTEGER I,ISTAT,IB(2),NTOT
C
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),AT,ATT(1)
      DOUBLE PRECISION TETA(NPLAN+1),FREQ(NF),PI,TV1,TV2,Z(1)
      DOUBLE PRECISION UC(NPOIN2),VC(NPOIN2),UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION UV1(NPOIN2),VV1(NPOIN2),UV2(NPOIN2),VV2(NPOIN2)
      DOUBLE PRECISION UC1(NPOIN2),VC1(NPOIN2),UC2(NPOIN2),VC2(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2),ZM1(NPOIN2),ZM2(NPOIN2)
      DOUBLE PRECISION DZHDT(NPOIN2)
      DOUBLE PRECISION TC1,TC2,TM1,TM2
C
      LOGICAL COURAN,VENT,MAREE
C
      CHARACTER*3 BINPRE
      CHARACTER*72 CAR
C
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPOIN2*NPLAN*NF))
C
C***********************************************************************
C
      PI=3.141592D0
      REWIND NPRE
C
C     READS TITLE
C
      CALL LIT(F,W,IB,CAR,72,'CH',NPRE,BINPRE,ISTAT)
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) '**** SUITE DE CALCUL ****'
         WRITE(LU,*) ' '
         WRITE(LU,*) 'TITRE DU CALCUL PRECEDENT :'
         WRITE(LU,*) '     ',CAR
      ELSE
         WRITE(LU,*) '**** FOLLOWING COMPUTATION ****'
         WRITE(LU,*) ' '
         WRITE(LU,*) 'TITLE OF THE PREVIOUS COMPUTATION :'
      ENDIF
      WRITE(LU,*) '     ',CAR
C
C     READS NPLAN, NF AND PERFORMS CHECK
C
      CALL LIT(F,W,IB,CAR,2,'I ',NPRE,BINPRE,ISTAT)
      IF ((IB(1).NE.NPLAN).OR.(IB(2).NE.NF)) THEN
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) '**** ERREUR DANS LECSUI : ****'
         WRITE(LU,*) 'LE NOMBRE DE DIRECTIONS ET/OU CELUI DE FREQUENCES'
         WRITE(LU,*) ' NE CORRESPOND PAS'
         WRITE(LU,*) 'VALEURS LUES : NDIR=',IB(1),' NF=',IB(2)
         WRITE(LU,*) 'VALEURS ATTENDUES : NDIR=',NPLAN,' NF=',NF
       ELSE
         WRITE(LU,*) '**** ERROR IN LECSUI : ****'
         WRITE(LU,*) 'THE NUMBER OF DIRECTIONS AND/OR FREQUENCIES'
         WRITE(LU,*) '   IS NOT CORRESPONDING '
         WRITE(LU,*) 'READ VALUES : NDIR=',IB(1),' NF=',IB(2)
         WRITE(LU,*) 'EXPECTED VALUES : NDIR=',NPLAN,' NF=',NF
       ENDIF
       CALL PLANTE(0)
      ENDIF
C
C     READS NELEM2, NPOIN2 AND PERFORMS CHECK
C
      CALL LIT(F,W,IB,CAR,2,'I ',NPRE,BINPRE,ISTAT)
      IF ((IB(1).NE.NELEM2).OR.(IB(2).NE.NPOIN2)) THEN
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) '**** ERREUR DANS LECSUI ****'
         WRITE(LU,*) 'LE NOMBRE DE POINTS ET/OU CELUI D''ELEMENTS 2D NE'
         WRITE(LU,*) 'CORRESPOND PAS'
         WRITE(LU,*) 'VALEURS LUES : NELEM2=',IB(1),' NPOIN2=',IB(2)
         WRITE(LU,*) 'VALEURS ATTENDUES : NELEM2=',NELEM2,
     &               ' NPOIN2=',NPOIN2
       ELSE
         WRITE(LU,*) '**** ERROR IN LECSUI : ****'
         WRITE(LU,*) 'THE NUMBER OF POINTS AND/OR 2D ELEMENTS '
         WRITE(LU,*) '   IS NOT CORRESPONDING '
         WRITE(LU,*) 'READ VALUES     : NELEM2=',IB(1),' NPOIN2=',IB(2)
         WRITE(LU,*) 'EXPECTED VALUES : NELEM2=',NELEM2,
     &               ' NPOIN2=',NPOIN2
       ENDIF
       CALL PLANTE(0)
      ENDIF
C
C     READS TIME STAMP
C
      CALL LIT(ATT,W,IB,CAR,1,'R4',NPRE,BINPRE,ISTAT)
      AT = ATT(1)
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) '- REPRISE DE CALCUL AU TEMPS  ',AT
      ELSE
         WRITE(LU,*) '- COMPUTATIONAL RESUMPTION AT TIME ',AT
      ENDIF
C
C     READS TETA
C
      CALL LIT(TETA,W,IB,CAR,NPLAN,'R4',NPRE,BINPRE,ISTAT)
      TETA(NPLAN+1)=2.D0*PI+TETA(1)
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'DISTRIBUTION DES DIRECTIONS :'
      ELSE
         WRITE(LU,*) 'DISTRIBUTION OF THE DIRECTIONS:'
      ENDIF
      DO I=1,NPLAN
        WRITE(LU,*) '       ',TETA(I)*180/PI,' DEGRES'
      ENDDO
C
C     READS FREQ
C
      CALL LIT(FREQ,W,IB,CAR,NF,'R4',NPRE,BINPRE,ISTAT)
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DISTRIBUTION DES FREQUENCES :'
      ELSE
        WRITE(LU,*) 'DISTRIBUTION OF THE FREQUENCIES:'
      ENDIF
      DO I=1,NF
        WRITE(LU,*) '       ',FREQ(I),' HERTZ'
      ENDDO
C
C     READS F
C
      NTOT=NPOIN2*NPLAN*NF
      CALL LIT(F,W,IB,CAR,NTOT,'R4',NPRE,BINPRE,ISTAT)
C
C     READS U,V,UV,VV IF HAS TO
C
      IF (COURAN) THEN
      CALL LIT(UC ,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
      CALL LIT(VC ,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
C
C     SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
C
      TC1=AT
      TC2=AT
        CALL OV( 'X=Y     ' , UC1 , UC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , UC2 , UC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VC1 , VC , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VC2 , VC , Z , 0.D0 , NPOIN2)
      ENDIF
C
      IF (VENT) THEN
      CALL LIT(UV,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
      CALL LIT(VV,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
C
C     SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
C
      TV1=AT
      TV2=AT
        CALL OV( 'X=Y     ' , UV1 , UV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , UV2 , UV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VV1 , VV , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , VV2 , VV , Z , 0.D0 , NPOIN2)
      ENDIF
C
      IF (MAREE) THEN
      CALL LIT(DEPTH ,W,IB,CAR,NPOIN2,'R4',NPRE,BINPRE,ISTAT)
C
C     SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
C
      TM1=AT
      TM2=AT
        CALL OV( 'X=Y     ' , ZM1 , DEPTH , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=Y     ' , ZM2 , DEPTH , Z , 0.D0 , NPOIN2)
        CALL OV( 'X=C     ' , DZHDT , DEPTH , Z , 0.D0 , NPOIN2)
      ENDIF
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(W)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
