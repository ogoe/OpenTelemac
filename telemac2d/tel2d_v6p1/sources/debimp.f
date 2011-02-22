C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IMPOSES FLUX BOUNDARY CONDITIONS,
!>                WITH AN ASSUMPTION OF AFFINITY WITH THE
!>                VELOCITY PROFILES AT THE ENTRANCE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EQUA, H, IFRLIQ, KP1BOR, MASK, MESH, NPTFR, NUMLIQ, Q, U, UBOR, V, VBOR, WORK1, WORK2
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
!>    </th><td> IELM, K, Q1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_SUM(), IELBOR(), PLANTE(), P_DSUM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 24/04/1997
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>EQUA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDEB,NFIN
!></td><td>--></td><td>NUMEROS DES POINTS DE BORD ENTRE LESQUELS ON
!>                  VEUT IMPOSER LE DEBIT
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE.
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>VALEUR DU DEBIT IMPOSE
!>    </td></tr>
!>          <tr><td>U , V , H
!></td><td>--></td><td>VALEURS DE U,V,H AU TEMPS N
!>    </td></tr>
!>          <tr><td>UBOR,VBOR
!></td><td><--</td><td>VALEURS DE U ET V DIRICHLET A L'ENTREE
!>    </td></tr>
!>          <tr><td>WORK1,WORK2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DEBIMP
     &(Q,UBOR,VBOR,U,V,H,NUMLIQ,IFRLIQ,WORK1,WORK2,NPTFR,MASK,MESH,
     & KP1BOR,EQUA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| EQUA           |---| 
C| IFRLIQ         |---| 
C| KP1BOR         |---| 
C| MASK           |---| 
C| MESH           |---| 
C| NDEB,NFIN      |-->| NUMEROS DES POINTS DE BORD ENTRE LESQUELS ON
C|                |   | VEUT IMPOSER LE DEBIT
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
C| NUMLIQ         |---| 
C| Q             |-->| VALEUR DU DEBIT IMPOSE
C| U , V , H      |-->| VALEURS DE U,V,H AU TEMPS N
C| UBOR,VBOR      |<--| VALEURS DE U ET V DIRICHLET A L'ENTREE
C| WORK1,WORK2    |-->| TABLEAUX DE TRAVAIL.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPTFR,IFRLIQ
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),KP1BOR(NPTFR,2)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: MASK(*),Q
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)      :: H,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WORK1,WORK2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,IELM
C
      DOUBLE PRECISION Q1
C
      INTRINSIC ABS
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
C=======================================================================
C     COMPUTES FLUX
C=======================================================================
C
C  IN THE FOLLOWING LOOP ONE RESTRICTS THE MASK OF DIRICHLETS SEGMENTS
C  TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ. AS NUMLIQ IS
C  DEFINED AT NODES, ONE RISKS AN ERROR FOR THE SEGMENT FOLLOWING
C  THE LAST NODE ON THE BOUNDARY. IN FACT THIS SEGMENT WILL BE SOLID
C  AND WILL HAVE A MASK ALREADY SET TO ZERO.
C
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
C
      DO K=1,NPTFR
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          WORK1%R(K)=MASK(K)
        ELSE
          WORK1%R(K)=0.D0
        ENDIF
      ENDDO
C
      ELSE
C
C     FINITE VOLUMES COUNT THE SOLID SEGMENTS CLOSE TO
C     THE LIQUID BOUNDARIES
C
      DO K=1,NPTFR
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          WORK1%R(K)          =MASK(K)
C         RA ON 07/12/2010
C         WORK1%R(KP1BOR(K,1))=MASK(K)
C         WORK1%R(KP1BOR(K,2))=MASK(K)
        ELSE
          WORK1%R(K)=0.D0
        ENDIF
      ENDDO
C
      ENDIF
C
      IELM=11
      CALL VECTOR(WORK2,'=','FLUBDF          ',IELBOR(IELM,1),
     &            1.D0,H,H,H,U,V,V,MESH,.TRUE.,WORK1)
C     SIGN CONVENTION REVERSED BETWEEN USER AND CODE
C     FOR THE USER: POSITIVE DISCHARGE = ENTERING
C     FOR THE CODE: U.N < 0 = ENTERING
      Q1 = - BIEF_SUM(WORK2)
      IF(NCSIZE.GT.1) Q1 = P_DSUM(Q1)
C
      IF(ABS(Q1).LT.1.D-10) THEN
C
C ZERO FLUX: WARNING MESSAGE
C
        IF(ABS(Q).GT.1.D-10) THEN
          IF(LNG.EQ.1) WRITE(LU,30) IFRLIQ
          IF(LNG.EQ.2) WRITE(LU,31) IFRLIQ
30        FORMAT(1X,'DEBIMP : PROBLEME SUR LA FRONTIERE ',1I6,/,1X,
     &     '         DONNER UN PROFIL DE VITESSES        ',/,1X,
     &     '         DANS LE FICHIER DES CONDITIONS AUX LIMITES',/,1X,
     &     '         OU VERIFIER LES HAUTEURS D''EAU')
31        FORMAT(1X,'DEBIMP : PROBLEM ON BOUNDARY NUMBER ',1I6,/,1X,
     &     '         GIVE A VELOCITY PROFILE  ',/,1X,
     &     '         IN THE BOUNDARY CONDITIONS FILE',/,1X,
     &     '         OR CHECK THE WATER DEPTHS')
          CALL PLANTE(1)
          STOP
        ELSE
          Q1 = 1.D0
        ENDIF
C
      ENDIF
C
C=======================================================================
C   COMPUTES UBOR AND VBOR
C=======================================================================
C
      DO 40 K=1,NPTFR
C
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          UBOR(K) = UBOR(K) * Q / Q1
          VBOR(K) = VBOR(K) * Q / Q1
        ENDIF
C
40    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
