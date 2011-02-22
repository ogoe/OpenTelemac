C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES A LINEAR SYSTEM OF THE FORM A X = B
!><br>            USING ITERATIVE METHODS
!>                WITH POSSIBLE PRECONDITIONING.
!>  @code
!>-----------------------------------------------------------------------
!>                        CHOICE OF THE METHOD
!>-----------------------------------------------------------------------
!>      VALUE          I       MEANING         I
!>-----------------------------------------------------------------------
!>                     I                       I
!>        1            I   CONJUGATE GRADIENT  I
!>                     I                       I
!>        2            I   CONJUGATE RESIDUAL  I
!>                     I                       I
!>        3            I   CONJUGATE GRADIENT  I
!>                     I  ON A NORMAL EQUATION I
!>                     I                       I
!>        4            I     MINIMUM ERROR     I
!>                     I                       I
!>        5            I        SQUARED        I
!>                     I   CONJUGATE GRADIENT  I
!>                     I                       I
!>        6            I        SQUARED        I
!>                     I   CONJUGATE GRADIENT  I
!>                     I        (CGSTAB)       I
!>                     I                       I
!>        7            I       GMRES           I
!>                     I                       I
!>        8            I     DIRECT : YSMP     I
!>                     I                       I
!>        9            I     DIRECT : MUMPS    I
!>                     I                       I
!>-----------------------------------------------------------------------
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  FOR SOME PRECONDITIONING ALGORITHMS, MATRIX A
!>            CAN BE MODIFIED

!>  @code
!>                        PRECONDITIONING
!>
!>     NOTES     : SOME PRECONDITIONING ALGORITHMS CAN BE ADDED
!>                 (DIAGONAL 2, 3 OU 5 WITH THE OTHERS).
!>                 THAT'S WHY PRIME NUMBERS WERE USED TO CHARACTERISE
!>                 THE PRECONDITIONING ALGORITHMS.
!>                 SHOULD THE USER WANT TO CUMULATE THE EFFECTS, HE/SHE
!>                 SHOULD GIVE THE PRODUCT OF THE CORRESPONDING PRIMES
!>
!>-----------------------------------------------------------------------
!>        VALUE        I                  MEANING
!>-----------------------------------------------------------------------
!>        0 OR 1       I  NO PRECONDITIONING
!>                     I
!>        2            I  DIAGONAL PRECONDITIONING WITH THE MATRIX
!>                     I  DIAGONAL
!>                     I
!>        3            I  DIAGONAL PRECONDITIONING WITH THE CONDENSED MATRIX
!>                     I
!>        5            I  DIAGONAL PRECONDITIONING WITH THE ABSOLUTE
!>                     I  VALUE OF THE MATRIX DIAGONAL
!>                     I
!>        7            I  CROUT'S PRECONDITIONING BY ELEMENT
!>                     I
!>       11            I  GAUSS-SEIDEL'S PRECONDITIONING BY ELEMENT
!>                     I
!>       13            I  PRECONDITIONING MATRIX SUPPLIED BY THE
!>                     I  SUBROUTINE CALLING
!>                     I
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, AUX, B, CFG, INFOGR, MESH, TB, X
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BB, BX, C, DIADON, DIFF1, DIFF2, FIRST, I, IAD, IBL1, IBL2, IG, IT1, IT2, IT3, IT4, IT5, IT6, IT7, ITB, ITBB, K, LV, NBL, NPLAN, NPOIN2, NPOIN_TEMP, NPOIN_TOT, PB, PRESTO, PREXSM, PX, S, TBB, TDEB1, TFIN1
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SOLVE, PB, PX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ADDBLO(), ALLBLO(), ALLBLO_IN_BLOCK(), CGSQUA(), CGSTAB(), DCPLDU(), EQUNOR(), ERRMIN(), GMRES(), GRACJG(), GSEBE(), OS(), PARCOM(), PLANTE(), PRE4_MUMPS(), PREBDT(), PRECDT(), PREVEREBE(), PREVERSEG(), RESCJG(), SD_SOLVE_1(), SD_SOLVE_4(), SOLAUX(), UM1X()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO(), CVDFTR(), DIFF3D(), KEPSIL(), PREDIV(), PROPAG(), PROPAG_ADJ(), WAVE_EQUATION()

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
!> </td><td> 19/03/10
!> </td><td> C. DENIS (SINETICS)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td> 18/02/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>MATRICE DU SYSTEME
!>    </td></tr>
!>          <tr><td>AUX
!></td><td>--></td><td>MATRICE DE TRAVAIL DE MEME STRUCTURE QUE A
!>                  (UTILISEE SEULEMENT POUR CERTAINS PRECONDI-
!>                  TIONNEMENTS : 7 , 11 , 13)
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>SECOND MEMBRE DU SYSTEME.
!>    </td></tr>
!>          <tr><td>CFG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>SI OUI, ON IMPRIME UN COMPTE-RENDU
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>TB
!></td><td>--></td><td>BLOC DE VECTEURS DE TRAVAIL AVEC AU MOINS
!>                  MAX(7,2+2*CFG%KRYLOV)*S VECTEURS, S VALANT 1
!>                  SI A EST UNE MATRICE, 2 SI C'EST UN BLOC DE 4
!>                  ET 3 SI C'EST UN BLOC DE 9.
!>                  CFG%KRYLOV N'EST PRIS EN COMPTE QUE SI
!>                  CFG%SLV = 7 (GMRES)
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VALEUR INITIALE, PUIS SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE SOLVE
     &(X, A,B,TB,CFG,INFOGR,MESH,AUX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE DU SYSTEME
C| AUX            |-->| MATRICE DE TRAVAIL DE MEME STRUCTURE QUE A
C|                |   | (UTILISEE SEULEMENT POUR CERTAINS PRECONDI-
C|                |   | TIONNEMENTS : 7 , 11 , 13)
C| B             |-->| SECOND MEMBRE DU SYSTEME.
C| CFG            |---| 
C| INFOGR         |-->| SI OUI, ON IMPRIME UN COMPTE-RENDU
C| MESH           |-->| MAILLAGE.
C| TB             |-->| BLOC DE VECTEURS DE TRAVAIL AVEC AU MOINS
C|                |   | MAX(7,2+2*CFG%KRYLOV)*S VECTEURS, S VALANT 1
C|                |   | SI A EST UNE MATRICE, 2 SI C'EST UN BLOC DE 4
C|                |   | ET 3 SI C'EST UN BLOC DE 9.
C|                |   | CFG%KRYLOV N'EST PRIS EN COMPTE QUE SI
C|                |   | CFG%SLV = 7 (GMRES)
C| X             |<--| VALEUR INITIALE, PUIS SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SOLVE => SOLVE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(SLVCFG), INTENT(INOUT) :: CFG
C
C     STRUCTURES OF VECTORS OR BLOCKS OF VECTORS
C
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT) :: X,B
      TYPE(BIEF_OBJ), INTENT(INOUT)         :: TB
C
C     STRUCTURES OF MATRIX OR BLOCK OF MATRICES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A,AUX
C
C      INTEGER, INTENT(IN)            :: LT
      LOGICAL, INTENT(IN) :: INFOGR
C
C     MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER PRESTO,IG,LV,S,NBL,I
      INTEGER IT1,IT2,IT3,IT4,IT5,IT6,IT7,IBL1,IBL2,K,IAD,ITB,ITBB
C
      DOUBLE PRECISION C
      DOUBLE PRECISION TDEB1, TFIN1
C
      LOGICAL DIADON,PREXSM
      INTEGER NPOIN_TOT,DIFF1,DIFF2,NPOIN_TEMP,NPLAN
      EXTERNAL P_ISUM
      INTEGER P_ISUM
      INTEGER NPOIN2
C
C-----------------------------------------------------------------------
C
C     STRUCTURES OF BLOCKS OF WORKING ARRAYS
C
      TYPE(BIEF_OBJ)          :: TBB
      TYPE(BIEF_OBJ), TARGET  :: BB,BX
      TYPE(BIEF_OBJ), POINTER :: PB,PX
C
C-----------------------------------------------------------------------
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE TBB,BB,BX
C-----------------------------------------------------------------------
C
C  ALLOCATES THE BLOCK OF BLOCKS TBB AND THE BLOCKS IN TBB
C
      IF(FIRST) THEN
        CALL ALLBLO(TBB,'TBB   ')
        CALL ALLBLO(BB ,'BB    ')
        CALL ALLBLO(BX ,'BX    ')
        FIRST=.FALSE.
      ENDIF
      NBL = 7
      IF(CFG%SLV.EQ.7) NBL = MAX(NBL,4+2*CFG%KRYLOV)
      IF(NBL.GT.TBB%N) THEN
        TBB%N=0
        CALL ALLBLO_IN_BLOCK(TBB,NBL,'BL    ')
      ENDIF
C
C-----------------------------------------------------------------------
C
      LV = MESH%LV
C
C-----------------------------------------------------------------------
C
C  VARIOUS TYPES OF SOLVED SYSTEMS S = 0 : NORMAL MATRIX
C                                  S = 1 : 1 MATRICE IN A BLOCK
C                                  S = 2 : 4 MATRICES IN A BLOCK
C                                  S = 3 : 9 MATRICES IN A BLOCK
C
      IF(A%TYPE.EQ.3) THEN
        S = 0
        BB%N = 0
        BX%N = 0
        CALL ADDBLO(BB,B)
        CALL ADDBLO(BX,X)
        PX => BX
        PB => BB
      ELSEIF(A%TYPE.EQ.4) THEN
        IF(A%N.EQ.1) THEN
          S = 1
        ELSEIF(A%N.EQ.4) THEN
          S = 2
        ELSEIF(A%N.EQ.9) THEN
          S = 3
        ENDIF
        PX => X
        PB => B
      ENDIF
C
C----------------
C  DIRECT SOLVERS
C  ---> YSMP
C----------------
C
      IF(CFG%SLV.EQ.8) THEN
C
        IF(NCSIZE.GT.1) THEN
          IF(LNG.EQ.1) WRITE(LU,2018)
          IF(LNG.EQ.2) WRITE(LU,2019)
2018      FORMAT(1X,'UTILISER LE SOLVEUR DIRECT PARALLEL MUMPS,',/,1X,
     &              'SOLVEUR = 9',///)
2019      FORMAT(1X,'USE THE PARALLEL DIRECT SOLVER MUMPS,',/,1X,
     &              'SOLVER = 9',///)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(S.EQ.0) THEN
          IF(A%TYPEXT.NE.'S'.AND.A%TYPEXT.NE.'Q') THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'SOLVE (BIEF) : TERMES EXTRA-DIAGONAUX'
              WRITE(LU,*) '               DE TYPE ',A%TYPEXT
              WRITE(LU,*) '               NON TRAITE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'SOLVE (BIEF): OFF-DIAGONAL TERMS'
              WRITE(LU,*) '              OF TYPE ',A%TYPEXT
              WRITE(LU,*) '              NOT IMPLEMENTED'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL SD_SOLVE_1(A%D%DIM1,MESH%NSEG,MESH%GLOSEG%I,
     &                    MESH%GLOSEG%DIM1,
     &                    A%D%R,A%X%R,X%R,B%R,INFOGR,A%TYPEXT)
        ELSEIF(S.EQ.1) THEN
          IF(A%ADR(1)%P%TYPEXT.NE.'S'.AND.A%ADR(1)%P%TYPEXT.NE.'Q') THEN
            IF(LNG.EQ.1) THEN
             WRITE(LU,*) 'SOLVE (BIEF) : SOLVEUR DIRECT POUR LES'
             WRITE(LU,*) '               SYSTEMES SYMETRIQUES SEULEMENT'
            ENDIF
            IF(LNG.EQ.2) THEN
             WRITE(LU,*) 'SOLVE (BIEF): DIRECT SOLVER FOR SYMMETRIC'
             WRITE(LU,*) '              SYSTEMS ONLY'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL SD_SOLVE_1(A%ADR(1)%P%D%DIM1,MESH%NSEG,MESH%GLOSEG%I,
     &                    MESH%GLOSEG%DIM1,
     &                    A%ADR(1)%P%D%R,A%ADR(1)%P%X%R,X%ADR(1)%P%R,
     &                    B%ADR(1)%P%R,INFOGR,A%ADR(1)%P%TYPEXT)
        ELSEIF(S.EQ.2) THEN
          CALL SD_SOLVE_4(MESH%NPOIN,MESH%NSEG,MESH%GLOSEG%I,
     &                    A%ADR(1)%P%D%R,A%ADR(2)%P%D%R,
     &                    A%ADR(3)%P%D%R,A%ADR(4)%P%D%R,
     &                    A%ADR(1)%P%X%R,A%ADR(2)%P%X%R,
     &                    A%ADR(3)%P%X%R,A%ADR(4)%P%X%R,
     &                    X%ADR(1)%P%R,X%ADR(2)%P%R,
     &                    B%ADR(1)%P%R,B%ADR(2)%P%R,INFOGR,
     &                    A%ADR(1)%P%TYPEXT)
C       ELSEIF(S.EQ.3) THEN
        ELSE
          IF(LNG.EQ.1) WRITE(LU,301) S
          IF(LNG.EQ.2) WRITE(LU,401) S
301       FORMAT(1X,'SOLVE (BIEF) : S=',1I6,' CAS NON PREVU')
401       FORMAT(1X,'SOLVE (BIEF): S=',1I6,' CASE NOT IMPLEMENTED')
          CALL PLANTE(1)
          STOP
        ENDIF
        RETURN
C
      ELSEIF(CFG%SLV.EQ.9) THEN
C
C----------------
C  DIRECT SOLVERS
C  ---> MUMPS
C----------------
C
        IF(NCSIZE.LT.1) THEN
          IF(LNG.EQ.1) WRITE(LU,3018)
          IF(LNG.EQ.2) WRITE(LU,3019)
3018      FORMAT(1X,'MUMPS NON DISPONIBLE POUR DES TESTS SEQUENTIELS',
     &         /,1X,
     &              'UTILISER LE SOLVEUR SEQUENTIEL (SOLVEUR =8)',///)
3019      FORMAT(1X,'MUMPS ARE NOT AVAILABLE FOR SEQUENTIAL RUNS,',/,1X,
     &         'USE SEQUENITAL DIRECT SOLVER (SOLVER = 8) ',///)
          CALL PLANTE(1)
          STOP
        ENDIF
C NOTE JMH: VERY DANGEROUS !!!!!!!!!!!
        OPEN(UNIT=25,FILE='FRONT_GLOB.DAT')
        READ(25,*) NPOIN_TOT
        CLOSE(25)
        IF(S.EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,30110) S
          IF(LNG.EQ.2) WRITE(LU,40110) S
30110     FORMAT(1X,'SOLVE (BIEF) : S=',1I6,' CAS NON ENCORE PREVU
     &           POUR MUMPS')
40110     FORMAT(1X,'SOLVE (BIEF): S=',1I6,' CASE NOT YET
     &           IMPLEMENTED FOR MUMPS')
          CALL PLANTE(1)
          STOP
        ELSEIF(S.EQ.1) THEN
          IF(LNG.EQ.1) WRITE(LU,3011) S
          IF(LNG.EQ.2) WRITE(LU,4011) S
 3011     FORMAT(1X,'SOLVE (BIEF) : S=',1I6,' CAS NON ENCORE PREVU
     &           POUR MUMPS')
 4011     FORMAT(1X,'SOLVE (BIEF): S=',1I6,' CASE NOT YET
     &           IMPLEMENTED FOR MUMPS')
          CALL PLANTE(1)
          STOP
        ELSEIF(S.EQ.2) THEN
          CALL PRE4_MUMPS(MESH%NPOIN,MESH%NSEG,MESH%GLOSEG%I,
     &                    A%ADR(1)%P%D%R,A%ADR(2)%P%D%R,
     &                    A%ADR(3)%P%D%R,A%ADR(4)%P%D%R,
     &                    A%ADR(1)%P%X%R,A%ADR(2)%P%X%R,
     &                    A%ADR(3)%P%X%R,A%ADR(4)%P%X%R,
     &                    X%ADR(1)%P%R,X%ADR(2)%P%R,
     &                    B%ADR(1)%P%R,B%ADR(2)%P%R,INFOGR,
     &                    A%ADR(1)%P%TYPEXT,MESH%KNOLG%I,
     &                    NPOIN_TOT,IPID)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,301) S
          IF(LNG.EQ.2) WRITE(LU,401) S
          CALL PLANTE(1)
          STOP
        ENDIF
        RETURN
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      PRESTO = CFG%PRECON
      IF(CFG%PRECON.EQ.0) CFG%PRECON = 1
C
C-----------------------------------------------------------------------
C
C  MANAGES WORKING ARRAYS : ITB --> NEXT AVAILABLE VECTOR
C
C  ITB  --> NEXT AVAILABLE VECTOR IN TB
C  ITBB --> NEXT AVAIALBLE BLOCK  IN TBB
C
      ITB  = 1
      ITBB = 1
C
C  ALLOCATES TWO WORKING BLOCKS WITH, EITHER A VECTOR,
C  OR A BLOCK OF VECTORS (CASE WHERE S IS 0).
C  THESE TWO BLOCKS ARE COMMON TO ALL THE METHODS.
C
C     FOR THE PRECONDITIONING MATRICES
      IF(3*(CFG%PRECON/3).EQ.CFG%PRECON) THEN
C       DIAGONAL PRECONDITIONING WITH THE CONDENSED MATRIX : S**2 DIAGONALS
        CALL SOLAUX(IT1, TB,TBB,ITB,ITBB,S**2)
      ELSE
C       OTHER : S DIAGONALS
        CALL SOLAUX(IT1, TB,TBB,ITB,ITBB,S)
      ENDIF
C
      CALL SOLAUX(IT2, TB,TBB,ITB,ITBB,S)
C
      IF(CFG%SLV.EQ.7) THEN
C       SPECIAL GMRES : ARRAYS DEPENDING ON THE SIZE OF KRYLOV
C                       TBB(IBL1) : BLOCK OF CFG%KRYLOV VECTORS
C                       OR BLOCK OF CFG%KRYLOV BLOCKS OF S VECTORS
C                       TBB(IBL2) : IDEM
C
        IBL1=ITBB
        ITBB = ITBB + 1
        IBL2=ITBB
        ITBB = ITBB + 1
        TBB%ADR(IBL1)%P%N=0
        TBB%ADR(IBL2)%P%N=0
        DO 10 K=1,CFG%KRYLOV
          CALL SOLAUX(IAD, TB,TBB,ITB,ITBB,S)
          CALL ADDBLO(TBB%ADR(IBL1)%P,TBB%ADR(IAD)%P)
          CALL SOLAUX(IAD, TB,TBB,ITB,ITBB,S)
          CALL ADDBLO(TBB%ADR(IBL2)%P,TBB%ADR(IAD)%P)
10      CONTINUE
C       AVOIDS A WARNING FROM THE INTEL COMPILER
        IT3=-1
        IT4=-1
        IT5=-1
        IT6=-1
        IT7=-1
      ELSE
C       OTHER METHODS (COULD SOMETIMES NOT ALLOCATE IT6 OR IT7)
        CALL SOLAUX(IT3, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT4, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT5, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT6, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT7, TB,TBB,ITB,ITBB,S)
C       AVOIDS A WARNING FROM THE CRAY COMPILER
        IBL1=1
        IBL2=1
C
      ENDIF
C
C     CROUT'S PRECONDITIONING : REQUIRES A PRELIMINARY PRECONDITIONING
C     THAT SETS DIAGONALS TO 1.
C     THE GRADIENT WILL BE DISTINGUISHED FROM THE RESIDUE (POINTER IG)
C
      IF(  7*(CFG%PRECON/ 7).EQ.CFG%PRECON.OR.
     &    11*(CFG%PRECON/11).EQ.CFG%PRECON.OR.
     &    13*(CFG%PRECON/13).EQ.CFG%PRECON.OR.
     &    17*(CFG%PRECON/17).EQ.CFG%PRECON     ) THEN
        IG=IT6
        IF(2*(CFG%PRECON/2).NE.CFG%PRECON.AND.
     &     3*(CFG%PRECON/3).NE.CFG%PRECON) THEN
C         SELECTS DIAGONAL
          CFG%PRECON=2*CFG%PRECON
        ENDIF
      ELSE
C       NOTE IT5 =-1 IF CFG%SLV.EQ.7 BUT IN THIS CASE IG IS USELESS
        IG=IT5
      ENDIF
C
C  END OF: MANAGES THE WORKING ARRAYS
C
C-----------------------------------------------------------------------
C                               -1/2      -1/2  1/2        -1/2
C  DIAGONAL PRECONDITIONINGS : D     A  D      D     X  = D      B
C
      DIADON = .FALSE.
      PREXSM = .TRUE.
C
      IF(3*(CFG%PRECON/3).EQ.CFG%PRECON.AND.(S.EQ.2.OR.S.EQ.3)) THEN
C       DIAGONAL PRECONDITIONING WITH THE CONDENSED MATRIX (4 OR 9 MATRICES)
        CALL PREBDT(X,A,B,TBB%ADR(IT1)%P,MESH,PREXSM,DIADON,S)
C       DOES NOT MOFIFY D11, D22, D33 WHEN PRECDT IS CALLED
        DIADON = .TRUE.
      ENDIF
C
      IF(2*(CFG%PRECON/2).EQ.CFG%PRECON.OR.
     &   3*(CFG%PRECON/3).EQ.CFG%PRECON.OR.
     &   5*(CFG%PRECON/5).EQ.CFG%PRECON) THEN
        CALL PRECDT(X,A,B,TBB%ADR(IT1)%P,MESH,
     &              CFG%PRECON,PREXSM,DIADON,S)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  BUILDS THE PRECONDITIONING MATRICES:
C
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) THEN
        CALL DCPLDU(AUX,A,MESH,.TRUE.,LV)
      ELSEIF(11*(CFG%PRECON/11).EQ.CFG%PRECON) THEN
        CALL GSEBE(AUX,A,MESH)
      ELSEIF(13*(CFG%PRECON/13).EQ.CFG%PRECON) THEN
C       DOES NOTHING, AUX IS SUPPLIED BY THE SUBROUTINE CALLING
      ELSEIF(17*(CFG%PRECON/17).EQ.CFG%PRECON) THEN
        IF(CFG%SLV.NE.1.AND.CFG%SLV.NE.2) THEN
          WRITE(LU,*) 'PRECONDITIONING 17'
          WRITE(LU,*) 'NOT IMPLEMENTED FOR SOLVER ',CFG%SLV
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(MESH%TYPELM.NE.40) THEN
          WRITE(LU,*) 'PRECONDITIONING 17'
          WRITE(LU,*) 'IMPLEMENTED ONLY FOR PRISMS'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(AUX%TYPE.NE.3) THEN
          WRITE(LU,*) 'PRECONDITIONING 17'
          WRITE(LU,*) 'NOT IMPLEMENTED FOR BLOCKS OF MATRICES'
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(AUX%STO.EQ.1) THEN
        CALL PREVEREBE(AUX%X%R,A%D%R,A%X%R,A%TYPDIA,A%TYPEXT,
     &              MESH%IKLE%I,MESH%NPOIN,MESH%NELEM,MESH%NELMAX,MESH)
        ELSE
        CALL PREVERSEG(AUX%X%R,A%D%R,A%X%R,A%TYPDIA,A%TYPEXT,
     &                 MESH%NPOIN,MESH,MESH%NSEG)
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  PARALLEL MODE: SECOND MEMBER
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(B,2,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  SOLVES THE LINEAR SYSTEM:
C
      IF(CFG%SLV.EQ.1) THEN
C
C       CONJUGATE GRADIENT
C
        CALL GRACJG(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,
     &              TBB%ADR(IT5)%P,TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
C
      ELSEIF(CFG%SLV.EQ.2) THEN
C
C       CONJUGATE RESIDUAL
C
        CALL RESCJG(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,
     &              TBB%ADR(IT4)%P,TBB%ADR(IT5)%P,
     &              TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
C
      ELSEIF(CFG%SLV.EQ.3) THEN
C
C       NORMAL EQUATION
C
        CALL EQUNOR(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,
     &              TBB%ADR(IT4)%P,TBB%ADR(IT5)%P,
     &              TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
C
      ELSEIF(CFG%SLV.EQ.4) THEN
C
C       MINIMUM ERROR
C
        CALL ERRMIN(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,TBB%ADR(IT5)%P,
     &              TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
C
      ELSEIF(CFG%SLV.EQ.5) THEN
C
C       SQUARED CONJUGATE GRADIENT
C
        CALL CGSQUA(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,TBB%ADR(IT4)%P,
     &              TBB%ADR(IT5)%P,TBB%ADR(IT6)%P,TBB%ADR(IT7)%P,
     &              CFG,INFOGR)
C
      ELSEIF(CFG%SLV.EQ.6) THEN
C
C       CGSTAB
C
        CALL CGSTAB(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,TBB%ADR(IT4)%P,
     &              TBB%ADR(IT5)%P,TBB%ADR(IT6)%P,TBB%ADR(IT7)%P,
     &              CFG,INFOGR,AUX)
C
      ELSEIF(CFG%SLV.EQ.7) THEN
C
C       GENERALISED MINIMUM RESIDUAL
C
        CALL GMRES(PX, A,PB,MESH,
     &             TBB%ADR(IT2)%P,TBB%ADR(IBL1)%P,TBB%ADR(IBL2)%P,
     &             CFG,INFOGR,AUX)
C
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,300) CFG%SLV
        IF (LNG.EQ.2) WRITE(LU,400) CFG%SLV
300     FORMAT(1X,'SOLVE (BIEF) :',1I6,' METHODE NON PREVUE :')
400     FORMAT(1X,'SOLVE (BIEF) :',1I6,' METHOD NOT AVAILABLE :')
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INVERSES THE CHANGE IN VARIABLE IF PRECONDITIONING
C                                                DIAGONAL
C                                                DIAGONAL-BLOCK
C
      IF(2*(CFG%PRECON/2).EQ.CFG%PRECON.OR.
     &   3*(CFG%PRECON/3).EQ.CFG%PRECON.OR.
     &   5*(CFG%PRECON/5).EQ.CFG%PRECON    ) THEN
        CALL OS( 'X=XY    ' , PX , TBB%ADR(IT1)%P , PX , C )
      ENDIF
C
      IF(3*(CFG%PRECON/3).EQ.CFG%PRECON.AND.(S.EQ.2.OR.S.EQ.3)) THEN
        CALL UM1X(X,TBB%ADR(IT1)%P,S)
      ENDIF
C
C-----------------------------------------------------------------------
C
      CFG%PRECON = PRESTO
C
C-----------------------------------------------------------------------
C


      RETURN
      END

C
C#######################################################################
C
