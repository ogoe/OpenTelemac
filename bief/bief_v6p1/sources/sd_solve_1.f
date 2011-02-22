C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIRECT RESOLUTION OF A SYMMETRICAL LINEAR SYSTEM WITH
!>                MINIMUM DEGREE PERMUTATION AND LDLT DECOMPOSITION.
!><br>            FROM SEGMENT STORAGE TO COMPACT STORAGE (MORSE).
!>  @code
!> IMPORTANT NOTE: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!>
!>
!>               YALE SPARSE MATRIX PACKAGE - NONSYMMETRIC CODES
!>                    SOLVING THE SYSTEM OF EQUATIONS MX = B
!>                        (UNCOMPRESSED POINTER STORAGE)
!>
!>    I.   CALLING SEQUENCES
!>         THE COEFFICIENT MATRIX CAN BE PROCESSED BY AN ORDERING ROUTINE
!>    (E.G., TO REDUCE FILLIN OR ENSURE NUMERICAL STABILITY) BEFORE USING
!>    THE REMAINING SUBROUTINES.  IF NO REORDERING IS DONE, THEN SET
!>    R(I) = C(I) = IC(I) = I  FOR I=1,...,N.  THE CALLING SEQUENCE IS --
!>        (      (MATRIX ORDERING))
!>         NSF   (SYMBOLIC FACTORIZATION TO DETERMINE WHERE FILLIN WILL
!>                 OCCUR DURING NUMERIC FACTORIZATION)
!>         NNF   (NUMERIC FACTORIZATION INTO PRODUCT LDU OF UNIT LOWER
!>                 TRIANGULAR MATRIX L, DIAGONAL MATRIX D, AND UNIT UPPER
!>                 TRIANGULAR MATRIX U, AND SOLUTION OF LINEAR SYSTEM)
!>         NNS   (SOLUTION OF LINEAR SYSTEM FOR ADDITIONAL RIGHT-HAND
!>     OR          SIDE USING LDU FACTORIZATION FROM NNF)
!>         NNT   (SOLUTION OF TRANSPOSED LINEAR SYSTEM FOR ADDITIONAL
!>                 RIGHT-HAND SIDE USING LDU FACTORIZATION FROM NNF)
!>
!>    II.  STORAGE OF SPARSE MATRICES
!>         THE NONZERO ENTRIES OF THE COEFFICIENT MATRIX M ARE STORED
!>    ROW-BY-ROW IN THE ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO
!>    ENTRIES IN EACH ROW, WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY
!>    LIES.  THE COLUMN INDICES WHICH CORRESPOND TO THE NONZERO ENTRIES
!>    OF M ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN
!>    JA(K) = J.  IN ADDITION, WE NEED TO KNOW WHERE EACH ROW STARTS AND
!>    HOW LONG IT IS.  THE INDEX POSITIONS IN JA AND A WHERE THE ROWS OF
!>    M BEGIN ARE STORED IN THE ARRAY IA;  I.E., IF M(I,J) IS THE FIRST
!>    NONZERO ENTRY (STORED) IN THE I-TH ROW AND A(K) = M(I,J),  THEN
!>    IA(I) = K.  MOREOVER, THE INDEX IN JA AND A OF THE FIRST LOCATION
!>    FOLLOWING THE LAST ELEMENT IN THE LAST ROW IS STORED IN IA(N+1).
!>    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS GIVEN BY
!>    IA(I+1) - IA(I),  THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED
!>    CONSECUTIVELY IN
!>            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),
!>    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN
!>            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).
!>    FOR EXAMPLE, THE 5 BY 5 MATRIX
!>                ( 1. 0. 2. 0. 0.)
!>                ( 0. 3. 0. 0. 0.)
!>            M = ( 0. 4. 5. 6. 0.)
!>                ( 0. 0. 0. 7. 0.)
!>                ( 0. 0. 0. 8. 9.)
!>    WOULD BE STORED AS
!>               \ 1  2  3  4  5  6  7  8  9
!>            ---+--------------------------
!>            IA \ 1  3  4  7  8 10
!>            JA \ 1  3  2  2  3  4  4  4  5
!>             A \ 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
!>
!>         THE STRICT TRIANGULAR PORTIONS OF THE MATRICES L AND U ARE
!>    STORED IN THE SAME FASHION USING THE ARRAYS  IL, JL, L  AND
!>    IU, JU, U  RESPECTIVELY.  THE DIAGONAL ENTRIES OF L AND U ARE
!>    ASSUMED TO BE EQUAL TO ONE AND ARE NOT STORED.  THE ARRAY D
!>    CONTAINS THE RECIPROCALS OF THE DIAGONAL ENTRIES OF THE MATRIX D.
!>
!>    III. ADDITIONAL STORAGE SAVINGS
!>         IN NSF, R AND IC CAN BE THE SAME ARRAY IN THE CALLING
!>    SEQUENCE IF NO REORDERING OF THE COEFFICIENT MATRIX HAS BEEN DONE.
!>         IN NNF, R, C AND IC CAN ALL BE THE SAME ARRAY IF NO REORDERING
!>    HAS BEEN DONE.  IF ONLY THE ROWS HAVE BEEN REORDERED, THEN C AND IC
!>    CAN BE THE SAME ARRAY.  IF THE ROW AND COLUMN ORDERINGS ARE THE
!>    SAME, THEN R AND C CAN BE THE SAME ARRAY.  Z AND ROW CAN BE THE
!>    SAME ARRAY.
!>         IN NNS OR NNT, R AND C CAN BE THE SAME ARRAY IF NO REORDERING
!>    HAS BEEN DONE OR IF THE ROW AND COLUMN ORDERINGS ARE THE SAME.  Z
!>    AND B CAN BE THE SAME ARRAY;  HOWEVER, THEN B WILL BE DESTROYED.
!>
!>    IV.  PARAMETERS
!>         FOLLOWING IS A LIST OF PARAMETERS TO THE PROGRAMS.  NAMES ARE
!>    UNIFORM AMONG THE VARIOUS SUBROUTINES.  CLASS ABBREVIATIONS ARE --
!>       N - INTEGER VARIABLE
!>       F - REAL VARIABLE
!>       V - SUPPLIES A VALUE TO A SUBROUTINE
!>       R - RETURNS A RESULT FROM A SUBROUTINE
!>       I - USED INTERNALLY BY A SUBROUTINE
!>       A - ARRAY
!>
!> CLASS \ PARAMETER
!> ------+----------
!> FVA   \ A     - NONZERO ENTRIES OF THE COEFFICIENT MATRIX M, STORED
!>       \           BY ROWS.
!>       \           SIZE = NUMBER OF NONZERO ENTRIES IN M.
!> FVA   \ B     - RIGHT-HAND SIDE B.
!>       \           SIZE = N.
!> NVA   \ C     - ORDERING OF THE COLUMNS OF M.
!>       \           SIZE = N.
!> FVRA  \ D     - RECIPROCALS OF THE DIAGONAL ENTRIES OF THE MATRIX D.
!>       \           SIZE = N.
!> NR    \ FLAG  - ERROR FLAG;  VALUES AND THEIR MEANINGS ARE --
!>       \            0     NO ERRORS DETECTED
!>       \            N+K   NULL ROW IN A  --  ROW = K
!>       \           2N+K   DUPLICATE ENTRY IN A  --  ROW = K
!>       \           3N+K   INSUFFICIENT STORAGE FOR JL  --  ROW = K
!>       \           4N+1   INSUFFICIENT STORAGE FOR L
!>       \           5N+K   NULL PIVOT  --  ROW = K
!>       \           6N+K   INSUFFICIENT STORAGE FOR JU  --  ROW = K
!>       \           7N+1   INSUFFICIENT STORAGE FOR U
!>       \           8N+K   ZERO PIVOT  --  ROW = K
!> NVA   \ IA    - POINTERS TO DELIMIT THE ROWS IN A.
!>       \           SIZE = N+1.
!> NVA   \ IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF M;  I.E.,
!>       \           IC(C(I) = I  FOR I=1,...N.
!>       \           SIZE = N.
!> NVRA  \ IL    - POINTERS TO DELIMIT THE ROWS IN L.
!>       \           SIZE = N+1.
!> NVRA  \ IU    - POINTERS TO DELIMIT THE ROWS IN U.
!>       \           SIZE = N+1.
!> NVA   \ JA    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF A.
!>       \           SIZE = SIZE OF A.
!> NVRA  \ JL    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF L.
!>       \           SIZE = JLMAX.
!> NV    \ JLMAX - DECLARED DIMENSION OF JL;  JLMAX MUST BE LARGER THAN
!>       \           THE NUMBER OF NONZERO ENTRIES IN THE STRICT LOWER
!>       \           TRIANGLE OF M PLUS FILLIN  (IL(N+1)-1 AFTER NSF).
!> NVRA  \ JU    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF U.
!>       \           SIZE = JUMAX.
!> NV    \ JUMAX - DECLARED DIMENSION OF JU;  JUMAX MUST BE LARGER THAN
!>       \           THE NUMBER OF NONZERO ENTRIES IN THE STRICT UPPER
!>       \           TRIANGLE OF M PLUS FILLIN  (IU(N+1)-1 AFTER NSF).
!> FVRA  \ L     - NONZERO ENTRIES IN THE STRICT LOWER TRIANGULAR PORTION
!>       \           OF THE MATRIX L, STORED BY ROWS.
!>       \           SIZE = LMAX
!> NV    \ LMAX  - DECLARED DIMENSION OF L;  LMAX MUST BE LARGER THAN
!>       \           THE NUMBER OF NONZERO ENTRIES IN THE STRICT LOWER
!>       \           TRIANGLE OF M PLUS FILLIN  (IL(N+1)-1 AFTER NSF).
!> NV    \ N     - NUMBER OF VARIABLES/EQUATIONS.
!> NVA   \ R     - ORDERING OF THE ROWS OF M.
!>       \           SIZE = N.
!> FVRA  \ U     - NONZERO ENTRIES IN THE STRICT UPPER TRIANGULAR PORTION
!>       \           OF THE MATRIX U, STORED BY ROWS.
!>       \           SIZE = UMAX.
!> NV    \ UMAX  - DECLARED DIMENSION OF U;  UMAX MUST BE LARGER THAN
!>       \           THE NUMBER OF NONZERO ENTRIES IN THE STRICT UPPER
!>       \           TRIANGLE OF M PLUS FILLIN  (IU(N+1)-1 AFTER NSF).
!> FRA   \ Z     - SOLUTION X.
!>       \           SIZE = N.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DA, GLOSEG, INFOGR, MAXSEG, NPOIN, NSEGB, RHS, TYPEXT, XA, XINC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AC, ACTRI, ESP, FLAG, I, IERR, IERRK, IN, INDIC, INDTRI, INX, IP, IPX, ISEGIP, ISP, IW1, MEMFACTOR, NPBLK, NSEGBLK, NSP, RSP, SIZE_AC, SIZE_ACTRI, SIZE_IN, SIZE_INDTRI, SIZE_INX, SIZE_IP, SIZE_IPX, SIZE_ISEGIP, SIZE_ISP, SIZE_IW1, SIZE_RSP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_SOLVE_1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), SD_FABCAD(), SD_NDRV(), SD_ODRV(), SD_SDRV(), SD_STRSSD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_SOLVE_4(), SOLVE()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 20/11/06
!> </td><td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DA,XA
!></td><td>--></td><td>DIAGONALE ET TERMES EXTRA-DIAGONAUX DE LA MATRICE
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF, YES INFORMATIONS ON LISTING
!>    </td></tr>
!>          <tr><td>MAXSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE D'INCONNUES
!>    </td></tr>
!>          <tr><td>NSEGB
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>RHS
!></td><td>--></td><td>SECOND MEMBRE
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XINC
!></td><td><--</td><td>SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE SD_SOLVE_1
     &(NPOIN,NSEGB,GLOSEG,MAXSEG,DA,XA,XINC,RHS,INFOGR,TYPEXT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DA,XA          |-->| DIAGONALE ET TERMES EXTRA-DIAGONAUX DE LA MATRICE
C| GLOSEG         |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
C| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
C| MAXSEG         |---| 
C| NPOIN          |-->| NOMBRE D'INCONNUES
C| NSEGB          |-->| NOMBRE DE SEGMENTS
C| RHS            |-->| SECOND MEMBRE
C| TYPEXT         |---| 
C| XINC           |<--| SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_SOLVE_1 => SD_SOLVE_1
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NSEGB,MAXSEG
      INTEGER, INTENT(IN)             :: GLOSEG(MAXSEG,2)
      LOGICAL, INTENT(IN)             :: INFOGR
      DOUBLE PRECISION, INTENT(IN)    :: XA(*),RHS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XINC(NPOIN),DA(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER MEMFACTOR,IERR,NPBLK,NSEGBLK,NSP,ESP,INDIC,FLAG,I,IERRK
C
      INTEGER, ALLOCATABLE :: INDTRI(:),INX(:),IPX(:),IW1(:)
      INTEGER, ALLOCATABLE :: IN(:),IP(:),ISP(:),ISEGIP(:)
C
      DOUBLE PRECISION, ALLOCATABLE :: AC(:),ACTRI(:),RSP(:)
C
C     MANAGES THE SIZE OF ALLOCATABLE ARRAYS
C
      INTEGER SIZE_IN,SIZE_IP,SIZE_ISEGIP,SIZE_IW1,SIZE_INDTRI
      INTEGER SIZE_INX,SIZE_IPX,SIZE_AC,SIZE_ACTRI,SIZE_ISP,SIZE_RSP
C
      DATA SIZE_IN    /0/
      DATA SIZE_IP    /0/
      DATA SIZE_ISEGIP/0/
      DATA SIZE_IW1   /0/
      DATA SIZE_INDTRI/0/
      DATA SIZE_INX   /0/
      DATA SIZE_IPX   /0/
      DATA SIZE_AC    /0/
      DATA SIZE_ACTRI /0/
      DATA SIZE_ISP   /0/
      DATA SIZE_RSP   /0/
C
      SAVE
C
C-----------------------------------------------------------------------
C
C     CORRECTS DIAGONALS (TIDAL FLATS WITH MASKING)
C
      DO I=1,NPOIN
        IF(ABS(DA(I)).LT.1.D-15) DA(I)=1.D0
      ENDDO
C
C-----------------------------------------------------------------------
C
      IF(INFOGR) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '                       RESOLUTION DIRECTE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) '                      DIRECT SYSTEM SOLVER'
        ENDIF
      ENDIF
C
      NPBLK=NPOIN
      NSEGBLK=NSEGB
C
C     1. MEMFACTOR: MEMORY FACTOR FOR SIZE ISP AND RSP IN ODRV AND SDRV
C     =======================================================================
C
      IF(TYPEXT.EQ.'S') THEN
        MEMFACTOR = 5
      ELSE
        MEMFACTOR = 15
      ENDIF
      NSP=MEMFACTOR*(NPBLK+4*NSEGBLK)
      ESP=MEMFACTOR*(NPBLK+4*NSEGBLK)
C
C     2. ALLOCATES ARRAYS (OR REALLOCATES IF TOO SMALL)
C     =======================================================================
C
      IF(SIZE_IN.EQ.0) THEN
        ALLOCATE(IN(NPBLK+1))
        SIZE_IN=    NPBLK+1
      ELSEIF(       NPBLK+1.GT.SIZE_IN) THEN
        DEALLOCATE(IN)
        ALLOCATE(IN(NPBLK+1))
        SIZE_IN=    NPBLK+1
      ENDIF
C
      IF(SIZE_IP.EQ.0) THEN
        ALLOCATE(IP(NSEGBLK*2))
        SIZE_IP=    NSEGBLK*2
      ELSEIF(       NSEGBLK*2.GT.SIZE_IP) THEN
        DEALLOCATE(IP)
        ALLOCATE(IP(NSEGBLK*2))
        SIZE_IP=    NSEGBLK*2
      ENDIF
C
      IF(SIZE_ISEGIP.EQ.0) THEN
        ALLOCATE(ISEGIP(NSEGBLK*2+1))
        SIZE_ISEGIP=    NSEGBLK*2+1
      ELSEIF(           NSEGBLK*2+1.GT.SIZE_ISEGIP) THEN
        DEALLOCATE(ISEGIP)
        ALLOCATE(ISEGIP(NSEGBLK*2+1))
        SIZE_ISEGIP=    NSEGBLK*2+1
      ENDIF
C
      IF(SIZE_IW1.EQ.0) THEN
        ALLOCATE(IW1(NPBLK))
        SIZE_IW1=    NPBLK
      ELSEIF(        NPBLK.GT.SIZE_IW1) THEN
        DEALLOCATE(IW1)
        ALLOCATE(IW1(NPBLK))
        SIZE_IW1=    NPBLK
      ENDIF
C
      IF(SIZE_INDTRI.EQ.0) THEN
        ALLOCATE(INDTRI(NPBLK))
        SIZE_INDTRI=    NPBLK
      ELSEIF(           NPBLK.GT.SIZE_INDTRI) THEN
        DEALLOCATE(INDTRI)
        ALLOCATE(INDTRI(NPBLK))
        SIZE_INDTRI=    NPBLK
      ENDIF
C
      IF(SIZE_INX.EQ.0) THEN
        ALLOCATE(INX(NPBLK+1))
        SIZE_INX=    NPBLK+1
      ELSEIF(        NPBLK+1.GT.SIZE_INX) THEN
        DEALLOCATE(INX)
        ALLOCATE(INX(NPBLK+1))
        SIZE_INX=    NPBLK+1
      ENDIF
C
      IF(SIZE_IPX.EQ.0) THEN
        ALLOCATE(IPX(NSEGBLK*2+NPBLK+1))
        SIZE_IPX=    NSEGBLK*2+NPBLK+1
      ELSEIF(        NSEGBLK*2+NPBLK+1.GT.SIZE_IPX) THEN
        DEALLOCATE(IPX)
        ALLOCATE(IPX(NSEGBLK*2+NPBLK+1))
        SIZE_IPX=    NSEGBLK*2+NPBLK+1
      ENDIF
C
      IF(SIZE_AC.EQ.0) THEN
        ALLOCATE(AC(NSEGBLK*2+NPBLK+1))
        SIZE_AC=    NSEGBLK*2+NPBLK+1
      ELSEIF(       NSEGBLK*2+NPBLK+1.GT.SIZE_AC) THEN
        DEALLOCATE(AC)
        ALLOCATE(AC(NSEGBLK*2+NPBLK+1))
        SIZE_AC=    NSEGBLK*2+NPBLK+1
      ENDIF
C
      IF(SIZE_ACTRI.EQ.0) THEN
        ALLOCATE(ACTRI(NPBLK))
        SIZE_ACTRI=    NPBLK
      ELSEIF(          NPBLK.GT.SIZE_ACTRI) THEN
        DEALLOCATE(ACTRI)
        ALLOCATE(ACTRI(NPBLK))
        SIZE_ACTRI=    NPBLK
      ENDIF
C
      IF(SIZE_ISP.EQ.0) THEN
        ALLOCATE(ISP(NSP))
        SIZE_ISP=    NSP
      ELSEIF(        NSP.GT.SIZE_ISP) THEN
        DEALLOCATE(ISP)
        ALLOCATE(ISP(NSP))
        SIZE_ISP=    NSP
      ENDIF
C
      IF(SIZE_RSP.EQ.0) THEN
        ALLOCATE(RSP(ESP))
        SIZE_RSP=    ESP
      ELSEIF(        ESP.GT.SIZE_RSP) THEN
        DEALLOCATE(RSP)
        ALLOCATE(RSP(ESP))
        SIZE_RSP=    ESP
      ENDIF
C
C     3. BUILDS NONSYMMETRICAL COMPACT STORAGE (IN,IP)
C     WITHOUT THE DIAGONAL AND (INX,IPX) WITH THE DIAGONAL
C     =======================================================================
C
      CALL SD_STRSSD(NPBLK,NSEGBLK,GLOSEG(1,1),GLOSEG(1,2),
     &               IN,IP,ISEGIP,IW1)
C
      IF(TYPEXT.EQ.'S') THEN
        CALL SD_FABCAD(NPBLK,NSEGBLK,IN,IP,ISEGIP,
     &                 INDTRI,IW1,INX,IPX,ACTRI,XA,XA,DA,AC)
C                             ISTRI
      ELSE
        CALL SD_FABCAD(NPBLK,NSEGBLK,IN,IP,ISEGIP,
     &                 INDTRI,IW1,INX,IPX,ACTRI,XA,XA(NSEGBLK+1),DA,AC)
      ENDIF
C
C     4. MINIMUM DEGREE PERMUTATION (YSMP PACKAGE)
C     =======================================================================
C
      INDIC=1
C
      CALL SD_ODRV(NPBLK,INX,IPX,AC,IN  ,IW1 ,NSP,ISP,INDIC,FLAG)
C                                   PERM,INVP
C
      IF(FLAG.NE.0) THEN
        IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'AUGMENTER LE FACTEUR MEMOIRE (MEMFACTOR)',
     &                 ' DANS LA ROUTINE SD_SOLVE_1'
        ELSEIF(LNG.EQ.2)THEN
           WRITE(LU,*) 'INCREASE THE MEMORY FACTOR (MEMFACTOR)',
     &                 ' IN THE ROUTINE SD_SOLVE_1'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C---> SECOND MEMBER OF THE SYSTEM
C
C     5. LDLT DECOMPOSITION AND RESOLUTION (YSMP PACKAGE)
C
      IF(TYPEXT.EQ.'S') THEN
C                          PERM,INVP
        CALL SD_SDRV(NPBLK,IN  ,IW1 ,INX,IPX,AC,RHS,XINC,
     &               NSP,ISP,RSP,ESP,INDIC,FLAG)
      ELSE
        CALL SD_NDRV(NPBLK,IN  ,IN  ,IW1,INX,IPX,AC,RHS,XINC,
     &               NSP,ISP,RSP,ESP,INDIC,FLAG)
      ENDIF
C
      IF(TYPEXT.EQ.'S') THEN
      IF(FLAG.NE.0) THEN
        IERR=FLAG-8*NPBLK
        IF(IERR.GT.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MATRICE AVEC PIVOT NUL A LA LIGNE'
            WRITE(LU,*) IERR
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'MATRIX WITH ZERO PIVOT AT ROW'
            WRITE(LU,*) IERR
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'AUGMENTER LE FACTEUR MEMOIRE (MEMFACTOR)',
     &                  ' DE 1 DANS LA ROUTINE SD_SOLVE_1'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'ADD 1 TO THE MEMORY FACTOR (MEMFACTOR)',
     &                  ' IN SUBROUTINE SD_SOLVE_1'
          ENDIF
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ELSE
C
C---> COMMENTS THE ERROR: FLAG_SD_NDRV:
C
C FLAG  - ERROR FLAG;  VALUES AND THEIR MEANINGS ARE --
C             0     NO ERRORS DETECTED
C             N+K   NULL ROW IN A  --  ROW = K
C            2N+K   DUPLICATE ENTRY IN A  --  ROW = K
C            3N+K   INSUFFICIENT STORAGE IN NSF  --  ROW = K
C            4N+1   INSUFFICIENT STORAGE IN NNF
C            5N+K   NULL PIVOT  --  ROW = K
C            6N+K   INSUFFICIENT STORAGE IN NSF  --  ROW = K
C            7N+1   INSUFFICIENT STORAGE IN NNF
C            8N+K   ZERO PIVOT  --  ROW = K
C           10N+1   INSUFFICIENT STORAGE IN NDRV
C           11N+1   ILLEGAL PATH SPECIFICATION (INDIC)
      IF(FLAG.NE.0) THEN
        IERR=INT(FLAG/NPBLK)
        IF(IERR.EQ.3.OR.IERR.EQ.5.OR.IERR.EQ.8) THEN
          IERRK=FLAG-IERR*NPBLK
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MATRICE AVEC PIVOT NUL A LA LIGNE'
            WRITE(LU,*) IERRK
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'MATRIX WITH ZERO PIVOT AT ROW'
            WRITE(LU,*) IERRK
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'AUGMENTER LE FACTEUR MEMOIRE (MEMFACTOR)',
     &                  ' DANS LE SOUS-PROGRAMME SD_SOLVE_1'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'INCREASE THE MEMORY FACTOR (MEMFACTOR)',
     &                  ' IN SUBROUTINE SD_SOLVE_1'
          ENDIF
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C