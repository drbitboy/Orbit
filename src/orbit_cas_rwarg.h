/**********************************************************************/
/* orbit_cas_rwarg.h
 *
 * the code in this include file either 
 *   reads from array of pointers to arguments to a CAS 
 * *OR*
 *    writes from a CAS to a string of comma-separated arguments
 *
 * ***N.B. this is not reversible
 *
 * this code was a massive macro and was a bear to debug, had to ensure 
 * EVERY line but the last ended with a backslash
 * 
 * "INPUTS/OUTPUTS" - i.e. variables/MACROs required by this include file
 *
 *
 * MACROS:
 *
 *  ***N.B. ISREAD is required; PRTWARN & RTNERR are not
 *  ***N.B. ISREAD, PRTWARN & RTNERR are #undef'ed at the end of this file
 *
 * #define ISREAD 1    read into (CAS *)lclcas from args (tokPtr)
 *
 * #define ISREAD 0    write from (CAS *)lclcas CAS to string (workingstring)
 *
 * #define PRTWARN(A) ...    how to output error message
 *                           e.g.
 *
 *                             fprintf(stderr, "%s at token %d ...", A, i...)
 *
 *                           - if ISREAD==1, defaults to
 *
 *                             fprintf( stderr, "%s at token %d (%s)\n"
 *                                    , A, i-1, tokPtr[i-1])
 *
 *                           - not used if !ISREAD
 * 
 * #define RTNERR ...        optional, how to exit routine on error if ISREAD==1
 *                           ***N.B. RTNERR must stand alone (end with ; or })
 *                           ***N.B. rwargFailed will be set to !0 before 
 *                                   RTNERR and there will be a break;
 *                                   after RTNERR
 *                           ***N.B. RTNERR may be null
 *
 *                           e.g.
 *                             #define RTNERR \
 *                             if ( dupStr) free( dupStr); \
 *                             if ( lclcasWasAllocedHere ) { \
 *                               orbit_CAS_freeCAS(lclcas); \
 *                             } \
 *                             return (CAS *) NULL;
 *
 *                           - if ISREAD==1, RTNERR defaults to noop i.e.
 *                             on error the following statements will be
 *                             executed:
 * 
 *                             rwargFailed = !0; break;
 *
 *                           - if !ISREAD, RTNERR is ignored & code will exit 
 *                             switch with a break
 *
 *
 * VARIABLES:
 *
 * ********* Reading or writing:
 *
 * CAS *lclcas;      - local pointer to CAS into/from which to read/write
 *                     - for reading, CAS must already exist & be consistent 
 *                       with tokPtr[0] (see below)
 * long ilong;       - misc var used by this file
 * int j;            - misc var used by this file
 * int rwargFailed   - status variable, 0 for success, non-0 for failure
 *                     ***N.B. see N.B for RTNERR above
 *
 *
 * ********* Reading only:
 *
 * char *tokPtr[N];  - pointer to input arguments (argument strings)
 *                     - tokPtr[0] points to fragment type (e.g. "CAS_OPNAV_A")
 *                     - tokPtr[1] points to _delayStart unless lclcas is
 *                       SHOOT, REQ, CAS, CASDEF or CASOPNAV
 *                     - N > 30 should be ok
 * int ntok;         - number of args in tokPtr[]
 *
 * int i;            - misc var used by this file - current tokPtr index + 1
 * int cou;          - misc var used by this file
 * char *tPtr;       - misc var used by this file
 * char *nlPtr;      - misc var used by this file
 * int tlen;         - misc var used by this file
 *
 *
 * ********* Writing only:
 *
 * char workingString[N];  - working string used by this file to hold output 
 *                           argument list string
 *                           - first argument will be _delayStart unless 
 *                             lclcas is SHOOT, REQ, CAS, CASDEF of CASOPNAV
 *                             ***N.B. first arg is NOT fragment type (e.g. REQ)
 *                           - N >= 1024 should be ok
 *                           - On successful return, the routine that uses 
 *                             this file probably does this:
 *
 *                               return strdup(workingString);
 *
 * char *workptr;    - misc var used by this file
 * long i2;          - misc var used by this file
 */

/**********************************************************************/
/* these next two macros will be the same for reading and for writing */
/**********************************************************************/

# define RWARGNAME( VAR, ARRAY, NAMELIST) \
  RWARGNAMEQUOTESOPTIONAL( VAR, ARRAY, NAMELIST, 1)

# define RWARGNAMENOQUOTES( VAR, ARRAY, NAMELIST) \
  RWARGNAMEQUOTESOPTIONAL( VAR, ARRAY, NAMELIST, 0)

/********************************************************************/
/* macros to read a value from a string to a member of a CAS struct */
/********************************************************************/

#if ISREAD == 1

/* - error handling */

# define PRTERR PRTWARN( "Error")

#ifndef RTNERR
# define RWARGRTNERR { rwargFailed=!0; break; }
#else
# define RWARGRTNERR { rwargFailed=!0; RTNERR break; }
#endif

#ifndef PRTWARN
# define PRTWARN(WARNING) fprintf( stderr, "%s at token %d (%s)\n" \
                                         , WARNING, i-1, tokPtr[i-1])
#endif

# define RWARGTEST if ( i >= ntok) { PRTWARN( "Out of arguments"); RWARGRTNERR }

# define RWARG2PTR( FMT, PTR) \
  RWARGTEST \
  if ( (cou = sscanf( tokPtr[i++], FMT, PTR)) != 1) { PRTERR; RWARGRTNERR }

# define RWARGSTR( MBR, RWMAXLEN) RWARGRAWSTR( lclcas->MBR, RWMAXLEN)

# define RWARGRAWSTR( STR, RWMAXLEN) \
  RWARGTEST \
  strncpy( STR, tokPtr[i++], ((int)(RWMAXLEN))); \
  STR[((int)(RWMAXLEN))-1] = '\0'

# define RWDBLFMT "%lf"
# define RWARG( FMT, MBR) RWARG2PTR( FMT, &lclcas->MBR)

# define RWARGLNGNOCHANGE( MBR) \
  RWARGTEST \
  if ( !strcmp( "NOCHANGE", tokPtr[i]) || !strcmp( "\"NOCHANGE\"", tokPtr[i])){\
    lclcas->MBR = ORBIT_NOCHANGEVAL; \
    i++; \
  } else { \
    RWARG2FMT( "%ld", "\"%ld\"", MBR); \
  } \

/* read with one format, write with another, strip leading & trailing quotes */
# define RWARG2FMT( READFMT, WRITEFMT, MBR) \
  tPtr = tokPtr[i]; \
  tlen = strlen( tPtr); \
  if ( *tPtr == '"' && tPtr[tlen-1] == '"') { \
    tPtr[tlen-1] = '\0'; \
    tokPtr[i]++; \
  } \
  RWARG2PTR( READFMT, &lclcas->MBR)

# define RWARGP( FMT, MBR) RWARG2PTR( FMT, lclcas->MBR)
# define RWARGPVEC( F, M) RWARGP( F, M); RWARGP( F, M+1); RWARGP( F, M+2)

/* RWARGDTIME - [+/-]\SS\ or \HH:MM:SS\ text, long storage */

# define RWARGDTIME( MBR) \
  RWARGTEST \
  if ( ORBIT_BAD_DTIME != (lclcas->MBR = orbit_readDtime( tokPtr[i]))) { \
    i++; \
  } else { \
    RWARG( "%ld", MBR); \
  }

/* RWARGID long text, double storage */

# define RWARGID( MBR) RWARG2PTR( "%ld", &ilong); lclcas->MBR = ilong

/* BIT argument - if read a 0 then clear bit, else set bit */
# define RWARGBIT( MBR, BIT) \
  RWARG2PTR( "%ld", &ilong) \
  if ( ilong) lclcas->MBR |= (BIT); else lclcas->MBR &= (~(BIT))
# define RWARGBIT3( MBR, BIT1, BIT2, BIT3) \
  RWARGBIT( MBR, BIT1); RWARGBIT( MBR, BIT2); RWARGBIT( MBR, BIT3)

/* integer argument that translates to something else in the CAS 
 * e.g. 0=>Iaci, 1=>Ieci, 2=>Isci for one ->ds40*FrmType, but 
 *      0=>Iabf, 1=>Iaci, ... for another
 */
# define RWARGFIX( MBR, ARRAY) \
  RWARG( "%ld", MBR); \
  if ( lclcas->MBR >= 0 && lclcas->MBR < (sizeof(ARRAY) / sizeof(long)) ) \
    lclcas->MBR = ARRAY[lclcas->MBR]; \
  else { \
    RWARGRTNERR \
  }

# define RWARGNAMEQUOTESOPTIONAL( VAR, ARRAY, NAMELIST, USEQUOTES) \
  RWARGTEST \
  /* USEQUOTES ignored */ \
  tPtr = tokPtr[i++]; \
  tlen = strlen(tPtr); \
  if ( *tPtr == '"' && tPtr[tlen-1] == '"') { /* strip encl quotes */ \
    if ( !(USEQUOTES)) { \
      PRTWARN( "WARNING:  Ignoring enclosing quotes while"); \
    } \
    tPtr++; tlen -= 2; tPtr[tlen] = '\0'; \
  } else if ( USEQUOTES) { \
    PRTWARN( "WARNING:  Enclosing quotes missing while"); \
  } \
  for ( j=0, nlPtr=NAMELIST[0]; nlPtr; nlPtr = NAMELIST[++j]) { \
    if ( !strcmp( nlPtr, tPtr)) break; \
  } \
  if ( !nlPtr) { RWARGRTNERR } \
  VAR = ARRAY[j]

# define RWARGBITSINIT(BITS) lclcas->BITS = 0

#endif /* ISREAD == 1 */

/*********************************************************************/
/* end read macros                                                   */
/*********************************************************************/
/* start macros to write CAS member's contents to a malloc'ed string */
/*********************************************************************/

#if !ISREAD

# define WR2ARG( FMT, ARG) \
  if ( workptr != workingString)                 /* (1) before args 2-n ... */ \
    *(workptr++) = ',';                                   /* append a comma */ \
  sprintf( workptr, FMT, ARG);                       /* (2) write to string */ \
  workptr += strlen(workptr)                            /* (3) move pointer */

# define RWARGSTR( MBR, RWMAXLEN) RWARGRAWSTR( lclcas->MBR, RWMAXLEN)

# define RWARGRAWSTR( STR, RWMAXLEN) \
  if ( workptr != workingString)                 /* (1) before args 2-n ... */ \
    *(workptr++) = ',';                                   /* append a comma */ \
  strcpy( workptr, STR);                               /* append the string */ \
  for (j=0; j<(RWMAXLEN-1) && *workptr; ++j, ++workptr) /* loop through str */ \
    if (*workptr == ',') *workptr = ';';    /* ... changing commas to semis */ \
  *workptr == '\0'    /* null terminator over existing one or at RWMAXLEN-1 */

# define RWDBLFMT "%.9lg"
# define RWARG( FMT, MBR) WR2ARG( FMT, lclcas->MBR)
# define RWARGLNGNOCHANGE( MBR) \
  RWARG( (lclcas->MBR==ORBIT_NOCHANGEVAL) ? "\"NOCHANGE\"" : "\"%ld\"", MBR)
# define RWARG2FMT( READFMT, WRITEFMT, MBR) RWARG( WRITEFMT, MBR)
# define RWARGP( FMT, MBR) WR2ARG( FMT, *(lclcas->MBR))/* non-string pointers */
# define RWARGPVEC( F, M) RWARGP( F, M); RWARGP( F, M+1); RWARGP( F, M+2)

/* RWARGDTIME - \HH:MM:SS\ text, long storage */

# define RWARGDTIME( MBR) RWARG( "%ld", MBR)

/* RWARGID long text, double storage */

# define RWARGID( MBR) ilong = lclcas->MBR; WR2ARG( "%ld", ilong)

/* write a 1 if bit is set, else write a 0 */
# define RWARGBIT( MBR, BIT) \
  if ( lclcas->MBR & (BIT)) i2=1; else i2 = 0; \
  WR2ARG( "%ld", i2)

# define RWARGBIT3( MBR, BIT1, BIT2, BIT3) \
  RWARGBIT( MBR, BIT1); RWARGBIT( MBR, BIT2); RWARGBIT( MBR, BIT3)

/* convert integer structure member value to external integer via array
 * - last member of array is -1
 */

# define RWARGFIX( MBR, ARRAY) \
  for ( i2 = 0; ARRAY[i2] != -1 && ARRAY[i2] != lclcas->MBR; ++i2);\
  if ( ARRAY[i2] == -1L) i2 = 0; \
  WR2ARG( "%ld", i2)

# define RWARGNAMEQUOTESOPTIONAL( VAR, ARRAY, NAMELIST, USEQUOTES) \
  for ( i2 = 0; ARRAY[i2] != -1 && ARRAY[i2] != VAR; ++i2);\
  if ( ARRAY[i2] == -1L) i2 = 0; \
  *(workptr++) = ',';                                     /* append a comma */ \
  if ( USEQUOTES) *(workptr++) = '"';                     /* append a quote */ \
  sprintf( workptr, NAMELIST[i2]);                            /* write name */ \
  workptr += strlen(workptr);                             /* adjust pointer */ \
  if ( USEQUOTES) *(workptr++) = '"';                     /* append a quote */ \
  *workptr = '\0'

# define RWARGBITSINIT( A)

#endif /* !ISREAD */

/*******************************************************************/
/* end of write macros                                             */
/*******************************************************************/
/* start of code that steps through structures and uses the macros */
/*******************************************************************/

# if !ISREAD
  workptr = workingString;
# endif

# if ISREAD==1
  i = 1; 
# endif

  rwargFailed = 0;

  while ( !isANYTOP(*lclcas) && !isSHOOT(*lclcas)) { /* delay goes after name */ 
    RWARG( RWDBLFMT, _delayStart);                  /* - no delay for SHOOT */ 

#   if ISREAD==1
    i = 2; 
#   endif

    break;
  } 

  if ( !rwargFailed) switch (lclcas->_type) {
  case CASTYPE_DS40XGRS:
  case CASTYPE_DS40:
  case CASTYPE_DS40FULL:
    RWARGDTIME( ds40SlewDuration);                          /* slew_duration */
    RWARGFIX( ds40AimptFrmType, ds40SysArray);             /* DS40_coord_sys */
    RWARGPVEC( RWDBLFMT, ds40AimptVec);                        /* _aimpt_xyz */
    if ( isDS40XGRS(*lclcas)) break;         /* skip out if xgrs point quick */
    if ( isDS40FULL(*lclcas)) { 
      RWARGPVEC( RWDBLFMT, ds40ScRollVec);                  /* _roll_vec_xyz */
    } 
    RWARGFIX( ds40AimptSelect, ds40SelArray);            /* _aimpt_short_cut */
    if ( isDS40FULL(*lclcas)) { 
      RWARGPVEC( RWDBLFMT, ds40RollRefVec);                 /* _roll_ref_xyz */
    } 
    RWARGPVEC( RWDBLFMT, ds40VbVec);                       /* _boresight_xyz */
    if ( isDS40FULL(*lclcas)) { 
      RWARGFIX( ds40RollFrmType, ds40SysRollArray);           /* _roll_algor */
    } 
    break;

  case CASTYPE_DS56:
    RWARGBITSINIT( ds56Bits);
    RWARGNAMENOQUOTES( lclcas->ds56ReUse, ds56ReUseArray 
                     , ds56ReUseNames);                  /* RE_USE_PREV_SCAN */
    RWARGDTIME( ds56ScanDur);                          /* DS56_scan_duration */
    RWARGFIX( ds56FrmType, ds56SysArray);                      /* _coord_sys */
    RWARGBIT3( ds56Bits, BITxCHG, BITyCHG, BITzCHG);             /* _rev_xyz */
    RWARGBIT3( ds56Bits, BITxPAUSE, BITyPAUSE, BITzPAUSE);/* _hold_place_xyz */
    RWARGPVEC( RWDBLFMT, ds56RatePauseDur[0]);              /* _rate_dur_xyz */
    RWARGPVEC( RWDBLFMT, ds56RateVec);                          /* _rate_xyz */
    RWARGPVEC( RWDBLFMT, ds56RatePauseDur[1]);             /* _pause_dur_xyz */

    break;

  case CASTYPE_SHOOT:
    { int counter;
    RWARG( "%ld", msiShootCount);                            /* num_executes */
    for ( counter=0; counter<12; ++counter) { 
      RWARGP( "%ld", msiShootSeq+counter);         /* seq_num_1 - seq_num_12 */
      RWARGP( "%ld", msiShootDel+counter);       /* im_delta_1 - im_delta_12 */
      RWARG2FMT( "%lf" 
               , (lclcas->msiShootImageTypeDbl[counter] < 10.0) 
                 ? "\"%.1lf\"" : "\"%.0lf\"" 
               , msiShootImageTypeDbl[counter]);   /* im_type_1 - im_type_12 */
    } 
    if ( rwargFailed) break;
    } 
    break;

  case CASTYPE_MSISEQDEF:
    RWARGBITSINIT( msiSeqDefBits); 
    RWARG( "%ld", msiSeqDefIdNum);                                /* seq_num */
    RWARG( "%ld", msiSeqDefNumImages);                         /* num_images */
    RWARG( "%ld", msiSeqDefInterval);                            /* interval */
    RWARGNAME(lclcas->msiSeqDefCmpTbl,msiSeqDefCmpTblArray
                                     ,msiSeqDefCmpTblNames);   /* comp_table */

    RWARGNAME(lclcas->msiSeqDefDpcm,msiSeqDefDpcmArray
                                     ,msiSeqDefDpcmNames);      /* flag_dpcm */
    RWARGNAME(lclcas->msiSeqDefCmpAlg,msiSeqDefCmpAlgArray
                                     ,msiSeqDefCmpAlgNames);     /* comp_alg */
    RWARG( "%ld", msiSeqDefPixels);                                /* pixels */

    RWARGNAME(lclcas->msiSeqDefMode,msiSeqDefModeArray
                                     ,msiSeqDefModeNames);           /* mode */

    /* im{1-8}_dur */ 
    for ( ilong=0; ilong<8; ++ilong) { RWARGP( "%ld", msiSeqDefMsecExp+ilong);}
    if ( rwargFailed) break;
    /* im{1-8}_filt */ 
    for ( ilong=0; ilong<8; ++ilong) { RWARGP( "%ld", msiSeqDefFilt+ilong); }
    if ( rwargFailed) break;

    break;

  case CASTYPE_NISSEQDEF:
    RWARG( "%ld", nisIdNum);                                      /* seq_num */
    RWARG( "%ld", nisNumScans);                                 /* num_scans */
    RWARG( "%ld", nisSecBtwScan);                   /* seconds_between_scans */
    RWARG( "%ld", nisNumObs);                                     /* num_obs */
    RWARG( "%ld", nisCalInterval);                           /* cal_interval */
    RWARG( "%ld", nisSecPerObs);                          /* spectra_per_obs */
    RWARG( "%ld", nisNumRests);                                 /* num_rests */
    RWARG( "%ld", nisNumDarks);                                 /* num_darks */
    RWARG( "%ld", nisStepMirror);                /* mirror_steps_between_obs */
    RWARG( "%ld", nisSecBtwObs);                      /* seconds_between_obs */

    break;

  case CASTYPE_MSIPARK:                       /* all of these have no params */
  case CASTYPE_NISPARK:
  case CASTYPE_XGRSPARK: /**/
  case CASTYPE_MSIRELATT:
  case CASTYPE_NISRELATT:
  case CASTYPE_XGRSRELATT:
  case CASTYPE_NAVRELATT:
  case CASTYPE_NISCAL:
  case CASTYPE_NISBUFFLUSH:
    break;

  case CASTYPE_AUTOEXPOSE:
    RWARG( "%ld", msiAutoTImDelta);
    RWARG( "%ld", msiAutoFile4TIm);
    RWARG( "%ld", msiAutoTImExpTime);
    RWARG( "%ld", msiAutoAllowSat);
    RWARG( "%ld", msiAutoTargSatDN);
    RWARG( "%ld", msiAutoOvrExpFlbkTim);
    RWARG( "%ld", msiAutoNoiseOffset);
    break;

  case CASTYPE_LOADFILT:
    { int counter;
    for ( counter=0; counter<8; ++counter) {
      RWARGP( "%ld", msiLoadFiltSensArray+counter);
    }
    if ( rwargFailed) break;
    }
    break;

  case CASTYPE_XGRSCONFIG:
    RWARGBITSINIT( xgrsConfBits);
    RWARGNAME( lclcas->xgrsConfPosn
             , xgrsConfPosnArray, xgrsConfPosnNames); 
    RWARGNAME( lclcas->xgrsConfDir
             , xgrsConfDirArray, xgrsConfDirNames); 
    break;

  case CASTYPE_MSICONFIG:
    RWARGBITSINIT( msiConfBits);
    RWARG( "%ld", msiConfFullIm);
    RWARG( "%ld", msiConfSumIm);
    break;

  case CASTYPE_NISCONFIG:
    RWARGBITSINIT( nisConfBits);
    RWARG( "%ld", nisConfSlitDriveSel);
    RWARG( "%ld", nisConfShutDriveSel);
    RWARG( "%ld", nisConfVoltSet);
    RWARG( "%ld", nisConfScanDriveSel);
    break;

  case CASTYPE_MSITR:
    { int counter;
    RWARGDTIME( msiRepDelay[0]);                          /* START_IMAGING */
    for ( counter=0; counter<3; ++counter) { 
      RWARGBITSINIT( msiRepBitsTR[counter]);
      RWARGP( "%ld", msiRepSeq+counter);                       /* SEQ_1/2/3 */
      RWARGP( "%ld", msiRepDel+counter);                     /* DELTA_1/2/3 */
      RWARG2FMT( "%lf"
               , (lclcas->msiRepImageTypeDblTR[counter] < 10.0) 
                 ? "\"%.1lf\"" : "\"%.0lf\"" 
               , msiRepImageTypeDblTR[counter]);         /* IMAGE_TYPE 1/2/3 */
    }
    if ( rwargFailed) break;
    RWARGP( "%ld", msiRepIter);                                /* ITERATIONS */
    }
    break;

  case CASTYPE_MSISR:
    RWARGBITSINIT( msiRepBits);
    RWARGDTIME( msiRepDelay[0]);                            /* START_IMAGING */
    RWARGP( "%ld", msiRepSeq);                                      /* SEQ_1 */
    RWARGP( "%ld", msiRepDel);                                    /* DELTA_1 */
    RWARGP( "%ld", msiRepIter);                                /* ITERATIONS */
    RWARG2FMT( "%lf" 
             , (lclcas->msiRepImageTypeDbl < 10.0) 
               ? "\"%.1lf\"" : "\"%.0lf\"" 
             , msiRepImageTypeDbl);                            /* IMAGE_TYPE */

    break;

  case CASTYPE_MSIDR:
    RWARGBITSINIT( msiRepBits);
    RWARGDTIME( msiRepDelay[0]);                            /* START_IMAGING */
    RWARGP( "%ld", msiRepSeq);                                      /* SEQ_1 */
    RWARGP( "%ld", msiRepDel);                                    /* DELTA_1 */
    RWARGP( "%ld", msiRepSeq+1);                                    /* SEQ_2 */
    RWARGP( "%ld", msiRepDel+1);                                  /* DELTA_2 */
    RWARGP( "%ld", msiRepIter);                                /* ITERATIONS */
    RWARG2FMT( "%lf" 
             , (lclcas->msiRepImageTypeDbl < 10.0) 
               ? "\"%.1lf\"" : "\"%.0lf\"" 
             , msiRepImageTypeDbl);                            /* IMAGE_TYPE */
    break;

  case CASTYPE_MSIDSR:
    RWARGBITSINIT( msiRepBits);
    RWARGDTIME( msiRepDelay[0]);                      /* START_SEQ_1_IMAGING */
    if ( !ISREAD) {
      lclcas->msiRepDelay[1]= lclcas->msiRepDelay[0] + lclcas->msiRepSeq2Start;
    }
    RWARGDTIME( msiRepDelay[1]);                      /* START_SEQ_2_IMAGING */
    if ( ISREAD) {
      lclcas->msiRepSeq2Start= lclcas->msiRepDelay[1] - lclcas->msiRepDelay[0];
    }
    RWARGP( "%ld", msiRepSeq);                                      /* seq_1 */
    RWARGP( "%ld", msiRepDel);                                    /* delta_1 */
    RWARGP( "%ld", msiRepIter);                          /* seq_1_iterations */
    RWARGP( "%ld", msiRepSeq+1);                                    /* seq_2 */
    RWARGP( "%ld", msiRepDel+1);                                  /* delta_2 */
    RWARGP( "%ld", msiRepIter+1);                        /* seq_1_iterations */
    RWARG2FMT( "%lf" 
             , (lclcas->msiRepImageTypeDbl < 10.0) 
               ? "\"%.1lf\"" : "\"%.0lf\"" 
             , msiRepImageTypeDbl);                            /* IMAGE_TYPE */

    break;


  case CASTYPE_NISSU1:
  case CASTYPE_NISSU2:
    RWARGBITSINIT( nisSuBits);
    if ( lclcas->_type == CASTYPE_NISSU1) {     /* START_SEQUENCING ONLY FOR */
      RWARGDTIME( nisSuRepDelay);       /* CAS_NIS_*_SUPERSCAN_1 BUT NOT FOR */
    }                                                /* CAS_NIS_SUPER_SCAN_2 */
    RWARG( "%ld", nisSuSeq);                                      /* seq_num */
    RWARG2FMT( "%ld",   "%ld"  , nisSuMirrorPosn);           /* init_mir_pos */
    RWARGNAME( lclcas->nisSuAperture
             , nisSuAperArray, nisSuAperNames);                  /* aperture */
    RWARGNAME( lclcas->nisSuGain
             , nisSuGainArray, nisSuGainNames);                      /* gain */
    RWARG( "%ld", nisSuPpss);                    /* positions_per_super_scan */
    if ( lclcas->_type == CASTYPE_NISSU2) {
      RWARGNAME( lclcas->nisSuStepDir
               , nisSuStepDirArray, nisSuStepDirNames);          /* step_dir */
    } else {
      RWARG( "%ld", nisSuNoss);                     /* number_of_super_scans */
    }

    break;

  case CASTYPE_NISEX: 
    RWARGBITSINIT( nisRepBits[0]);
    RWARGDTIME( nisRepDelay);                            /* START_SEQUENCING */
    RWARGP( "%ld", nisRepSeq);                                    /* seq_num */
    RWARG2FMT( "%ld", "\"%ld\"", nisRepMirrorPosn[0]);         /* mirror_pos */
    RWARGNAME( lclcas->nisRepAperture[0]
             , nisRepAperArray, nisRepAperNames);                /* aperture */
    RWARGNAME( lclcas->nisRepGain[0]
             , nisRepGainArray, nisRepGainNames);                    /* gain */

    break;

  case CASTYPE_NISSR: 
    RWARGBITSINIT( nisRepBits[0]);
    RWARGDTIME( nisRepDelay);                            /* START_SEQUENCING */
    RWARGDTIME( nisRepSetup);                                      /* SET_UP */
    RWARG2FMT( "%ld", "%ld", nisRepMirrorPosn[0]);            /* mirror_step */
    RWARGNAME( lclcas->nisRepAperture[0]
             , nisRepAperArray, nisRepAperNames);          /* aperture_state */
    RWARGNAME( lclcas->nisRepGain[0]
             , nisRepGainArray, nisRepGainNames);                    /* gain */
    RWARGP( "%ld", nisRepSeq);                                  /* seq_num_1 */
    RWARGP( "%ld", nisRepDel);                                 /* interval_1 */
    RWARG( "%ld", nisRepIter);                                 /* iterations */

    break;

  case CASTYPE_NISDR: 
    RWARGBITSINIT( nisRepBits[0]);
    RWARGBITSINIT( nisRepBits[1]);
    RWARGDTIME( nisRepDelay);                            /* START_SEQUENCING */
    RWARGDTIME( nisRepSetup);                                      /* SET_UP */
    RWARG2FMT( "%ld", "%ld", nisRepMirrorPosnSetup);          /* mirror_step */
    RWARGNAME( lclcas->nisRepApertureSetup
             , nisRepAperArray, nisRepAperNames);          /* aperture_state */
    RWARGNAME( lclcas->nisRepGainSetup
             , nisRepGainArray, nisRepGainNames);                    /* gain */
    RWARGP( "%ld", nisRepSeq);                                  /* seq_num_1 */
          /* NISDR:  allow for NOCHANGE for Mirror position, Aperture & Gain */
    RWARGLNGNOCHANGE( nisRepMirrorPosn[0]);                  /* mirror_pos_1 */
    RWARGNAME( lclcas->nisRepAperture[0]
             , nisRepAperNochArray, nisRepAperNochNames);      /* aperture_1 */
    RWARGNAME( lclcas->nisRepGain[0]
             , nisRepGainNochArray, nisRepGainNochNames);          /* gain_1 */
    RWARGP( "%ld", nisRepDel);                                 /* interval_1 */
    RWARGP( "%ld", nisRepSeq+1);                                /* seq_num_2 */
    RWARGLNGNOCHANGE( nisRepMirrorPosn[1]);                  /* mirror_pos_2 */
    RWARGNAME( lclcas->nisRepAperture[1]
             , nisRepAperNochArray, nisRepAperNochNames);      /* aperture_2 */
    RWARGNAME( lclcas->nisRepGain[1]
             , nisRepGainNochArray, nisRepGainNochNames);          /* gain_2 */
    RWARGP( "%ld", nisRepDel+1);                               /* interval_2 */
    RWARG( "%ld", nisRepIter);                                 /* iterations */

    break;

  case CASTYPE_CAS:  
  case CASTYPE_CASDEF:
  case CASTYPE_CASOPNAV:
  case CASTYPE_REQ:
    RWARGSTR( _name, IDLEN);

#   if !ISREAD                     /* write time token; read it if it's there */
      RWARG( RWDBLFMT, _delayStart); 
#   endif
#   if ISREAD==1
    if ( ntok > i) { RWARG( RWDBLFMT, _delayStart); } 
    else { PRTWARN("WARNING:  DELAY_START missing after separator (set to 0)");}
#   endif

    if ( !isCASOPNAV(*lclcas)) break;

    /************** OPNAV ARGUMENTS ***************/
    {
    OPNAVARGLIST *opnavArgListPtr;
      if ( !(opnavArgListPtr=getOpnavArgList(lclcas->opnavSubType))) {
#       if ISREAD==1
        PRTWARN("***orbit_cas_rwarg.h:  ISREAD:  Can't get OPNAV arg list");
        RWARGRTNERR
#       endif
#       if !ISREAD
        fprintf( stderr
               , "***orbit_cas_rwarg.h:  !ISREAD:  %s\n"
               , "Can't get OPNAV argument list");
        rwargFailed = !0;
#       endif
        break;
      }
      for ( ; opnavArgListPtr->_argId != -1; ++opnavArgListPtr) {
      long argId, argCount;
      long idx;
        argCount = opnavArgListPtr->_argCount;
        if ( argCount > 0) { idx = argCount-1; }      /* (N>0):  get item N-1 */
        else { idx = 0; argCount = -argCount; }/* (N<=0):  get items 0 to N-1 */

        for ( ; idx < argCount; ++idx) {

          switch ( opnavArgListPtr->_argId) {

          case OPNAVARG_PRIO:
            RWARGNAMENOQUOTES( lclcas->opnavPrio[idx]
                             , opnavPrioArray, opnavPrioNames);
            break;

          case OPNAVARG_SLEWDUR:
            RWARGDTIME( opnavSlewDur[idx]);
            break;

          case OPNAVARG_STARTNTHSLEW:
            RWARGP( "%ld", opnavStartNthSlew+idx);
            break;

          case OPNAVARG_DS40COORDSYS:
            RWARGFIX( opnavDs40[idx]->ds40AimptFrmType, ds40SysArray);
            break;

          case OPNAVARG_DS40AIMXYZ:
            RWARGPVEC( RWDBLFMT, opnavDs40[idx]->ds40AimptVec);
            break;

          case OPNAVARG_DS40AIMSHORT:
            RWARGFIX( opnavDs40[idx]->ds40AimptSelect, ds40SelArray);
            break;

          case OPNAVARG_DS40BOREXYZ:
            RWARGPVEC( RWDBLFMT, opnavDs40[idx]->ds40VbVec);
            break;

          case OPNAVARG_DS56:
            RWARGBITSINIT( opnavDs56[idx]->ds56Bits);
            RWARGDTIME( opnavDs56[idx]->ds56ScanDur);
            RWARGFIX( opnavDs56[idx]->ds56FrmType, ds56SysArray);
            RWARGBIT3( opnavDs56[idx]->ds56Bits, BITxCHG, BITyCHG, BITzCHG);
            RWARGBIT3(opnavDs56[idx]->ds56Bits,BITxPAUSE,BITyPAUSE,BITzPAUSE);
            RWARGPVEC( RWDBLFMT, opnavDs56[idx]->ds56RatePauseDur[0]);
            RWARGPVEC( RWDBLFMT, opnavDs56[idx]->ds56RateVec);
            RWARGPVEC( RWDBLFMT, opnavDs56[idx]->ds56RatePauseDur[1]);
            break;

          case OPNAVARG_FRAMES:
            RWARG( "%ld", opnavShoot[idx]->msiShootCount);
            break;

          case OPNAVARG_IM_TYPE:
            RWARG2FMT( "%lf" 
                     , (lclcas->opnavShoot[0]->msiShootImageTypeDbl[idx]<10.0)
                       ? "\"%.1lf\"" : "\"%.0lf\""
                     , opnavShoot[0]->msiShootImageTypeDbl[idx]);
            break;

          case OPNAVARG_IM_DELTA:
            RWARGP( "%ld", opnavImDeltas+idx);
            break;

          case OPNAVARG_MISCINT:
            RWARGP( "%ld", opnavMiscInt+idx);
            break;

          default:

#           if ISREAD==1
            PRTWARN( "***orbit_cas_rwarg.h:  ISREAD:  Unknown _argId");
            RWARGRTNERR
#           endif

#           if !ISREAD
            fprintf( stderr
                   , "***orbit_cas_rwarg.h:  !ISREAD:  Unknown _argId\n");
            rwargFailed = !0;
#           endif

            break;

          } /* switch opnavArgListPtr->_argId */
          if ( rwargFailed) break;
        } /* for idx < argCount */
        if ( rwargFailed) break;
      } /* for opnavArgListPtr->_argId != -1 */
      if ( rwargFailed) break;
    }
#   if ISREAD==1
      if ( isCASOPNAV(*lclcas)) orbit_CAS_opnavSetChildren( lclcas);
#   endif
    break; /* case CASTYPE_CAS* */

  default:
    break;
  } /* if (rwargFailed) switch lclcas->_type */

/*************************/
/* end of code           */
/*************************/
/* start macros' cleanup */
/*************************/

#undef PRTERR
#undef RWDBLFMT
#undef RWARG
#undef RWARGLNGNOCHANGE
#undef RWARG2FMT
#undef RWARGSTR
#undef RWARGRAWSTR
#undef RWARG2PTR
#undef RWARGP
#undef RWARGPVEC
#undef RWARGDTIME
#undef RWARGID
#undef RWARGBIT
#undef RWARGBIT3
#undef RWARGFIX
#undef RWARGNAME
#undef RWARGNAMENOQUOTES
#undef RWARGNAMEQUOTESOPTIONAL
#undef RWARGBITSINIT

#if ISREAD==1
#undef RWARGTEST
#undef RWARGRTNERR
#undef PRTWARN
#endif

#if !ISREAD
#undef WR2ARG
#endif

#undef ISREAD

/* end of orbit_cas_rwarg.h */
